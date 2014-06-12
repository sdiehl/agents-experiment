{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Simulator (
  simulate,
) where

import Prelude hiding (print)

import Text.Printf
import Text.Show.Pretty

import Lens.Family
import Lens.Family.TH
import Lens.Family.State.Strict

import Control.Error
import Control.Exception hiding (throw)
import Control.Monad.Trans
import Control.Monad.State.Strict hiding (forM)
import Control.Monad.Writer hiding (forM)
import Control.Monad.Reader hiding (forM)
import Control.Applicative hiding (Const)

import Data.Data
import Data.Traversable (traverse, forM)
import qualified Data.Set as S

import AST as E
import Utils (printfa, PrintfArgT(..))

-------------------------------------------------------------------------------
-- Condition
-------------------------------------------------------------------------------

-- Type a named reference.
data SName
  = CMeasure
  | CTag
  | CPrototype Object
  deriving (Eq, Show)
-- XXX: make prisms for SName

type Scope = [(Ident, SName)]

-- Scope of things visible in the context of evaluation of an object.
data LocalCtx = LocalCtx
  { _cself :: Object      -- original entity, at the start of tick
  , _croot :: World
  , _cglobals :: Scope
  , _clocals  :: Scope
  , _cnursery :: [Object] -- objects the actuators want to add to the world
  , _csuicide :: Bool     -- object wants to destroy itself on next tick
  } deriving (Show)

mkLenses ''LocalCtx

condMeasure :: Ident -> Local (Maybe Cond)
condMeasure ident = lift $ do
  ms <- use measure
  case filter ((ident ==) . nameOf) ms of
    []                 -> return $ Just NotDefined
    (Measure _ mval:_) -> return $ Just (Const mval)

condTag :: Ident -> Local (Maybe Cond)
condTag ident = lift $ do
  ts <- use tags
  case S.member (Tag ident) ts of
    True  -> return (Just Tr)
    False -> return (Just Fl)


-- Lookup variable in condition evlauation
lookupCond :: Ident -> LocalCtx -> Local (Maybe Cond)
lookupCond ident ctx =
  case lookup ident (ctx ^. clocals) of
    Just CMeasure -> condMeasure ident
    Just CTag     -> condTag ident
    Just _        -> throw $ NameError "Reference used in expression context."

    -- XXX: having tags be implicitly defined and set has the consequence of making spelling errors
    -- indistuignishable from logic errors, probably want some sort of forward declaration at least at the
    -- module level.
    {-Nothing       -> pure $ Nothing-}
    Nothing       -> pure $ Just Fl


-- | Evaluator for the conditional expression logic.
evalC :: Cond -> Local Cond
evalC expr = ev expr
  where
    ev :: Cond -> Local Cond
    ev (Var x) = do
      ctx <- ask
      var <- lookupCond x ctx
      case var of
        Just n -> ev n
        Nothing -> throw $ NameError ("Not in object scope: " ++ x)

    ev ex = case ex of
      Tr         -> pure Tr
      Fl         -> pure Fl
      Const n    -> pure (Const n)
      NotDefined -> pure NotDefined

      And Fl _  -> pure Fl
      And _ Fl  -> pure Fl
      And Tr Tr -> pure Tr
      Or Tr _   -> pure Tr
      Or _ Tr   -> pure Tr
      Or Fl Fl  -> pure Fl

      Not Tr         -> pure Fl
      Not Fl         -> pure Tr
      Not NotDefined -> pure Tr -- XXX
      Not (Const 0)  -> pure Tr
      Not (Const _)  -> pure Fl

      Not x     -> ev =<< liftA  Not (ev x)
      And a b   -> ev =<< liftA2 And (ev a) (ev b)
      Or a b    -> ev =<< liftA2 Or  (ev a) (ev b)

      Gt  (Const a) (Const b) -> condA (a > b)
      Gte (Const a) (Const b) -> condA (a >= b)
      Lt  (Const a) (Const b) -> condA (a > b)
      Lte (Const a) (Const b) -> condA (a <= b)
      Eq  (Const a) (Const b) -> condA (a == b)

      Gt a b  -> ev =<< liftA2 Gt  (ev a) (ev b)
      Gte a b -> ev =<< liftA2 Gte (ev a) (ev b)
      Lt a b  -> ev =<< liftA2 Lt  (ev a) (ev b)
      Lte a b -> ev =<< liftA2 Lte (ev a) (ev b)
      Eq a b  -> ev =<< liftA2 Eq  (ev a) (ev b)

condA :: Applicative f => Bool -> f Cond
condA True  = pure Tr
condA False = pure Fl

condToBool :: Cond -> Bool
condToBool Tr = True
condToBool Fl = False
condToBool x = error $ "condition has no normal form: " ++ (show x)

-------------------------------------------------------------------------------
-- Simulator Monad
-------------------------------------------------------------------------------

data SimulatorError
  = NameError String
  | NotImplemented String
  | Panic String
  deriving (Data, Typeable, Show)

instance Exception SimulatorError

throw = lift . lift . throwT

type Sim a = StateT a (EitherT SimulatorError IO)
type Simulator = Sim SimState
type Local = ReaderT LocalCtx (Sim Object)

data SimState = SimState
  { _ticks   :: Integer
  , _world   :: World
  , _imodule :: Module
  , _probes  :: [Probe]
  } deriving (Eq, Show)

mkLenses ''SimState

emptysim :: SimState
emptysim = SimState 0 emptyWorld emptyModule []

run :: Simulator a -> SimState -> IO (Either SimulatorError SimState)
run f st = runEitherT (execStateT f st)

tick :: Simulator ()
tick = ticks += 1

puts :: String -> Simulator ()
puts msg = liftIO $ putStrLn msg

pshow :: Show a => a -> Simulator ()
pshow msg = liftIO $ putStrLn (ppShow msg)

print :: (Show t, PrintfArg t, MonadIO m) => String -> [t] -> m ()
print fmt xs = liftIO $ putStrLn $ printfa fmt (map P xs)

-------------------------------------------------------------------------------
-- Actuators
-------------------------------------------------------------------------------

runActuator :: Actuator -> Local ()
runActuator (Actuator rec acts) = case rec of

  -- always applies
  Always -> do
    performActions acts

  -- conditionally applies
  If cond -> do
    condtest <- evalC cond
    print "[Cond] %s ==> %s" [show cond, show condtest]
    if condToBool condtest
    then performActions acts
    else return ()

  Once cond -> do
    undefined

runActuators :: [Actuator] -> Local ()
runActuators = mapM_ runActuator

inScope :: r -> ReaderT r m a -> m a
inScope sc m = runReaderT m sc

stepSimulation :: Scope -> Simulator ()
stepSimulation globals = do
  root <- use world
  Module _ decls <- use imodule

  -- traverse the world, and run the logic associated with each entity
  objs <- zoom (world.entities.traverse) $ do
    self <- get
    let locals = localNames self
    let ctx = LocalCtx self root globals locals [] False

    nm <- use name
    print "[Entity] %s" [nm]

    -- in local scope of object execute actuators
    as <- use actuators
    inScope ctx (runActuators as)

    -- if object still exists then return for next tick
    self <- get
    return [self]

  world.entities .= objs

-------------------------------------------------------------------------------
-- Probes
-------------------------------------------------------------------------------

runProbes :: Simulator ()
runProbes = undefined

-------------------------------------------------------------------------------
-- Populate
-------------------------------------------------------------------------------

lookupProto :: Name -> Scope -> Maybe Object
lookupProto nm sc = case lookup nm sc of
  Just (CPrototype proto) -> Just proto
  _ -> Nothing

globalNames :: [Decl] -> [(Ident, SName)]
globalNames xs = execWriter (mapM go xs)
  where
    go (ObjectDecl proto) = tell [(proto ^. name, CPrototype proto)]
    go _ = tell []

localNames :: Object -> [(Ident, SName)]
localNames obj = tnames ++ mnames
  where
    -- XXX blech
    tnames = fmap (\x -> (nameOf x, CTag)) $ S.toList $ (obj ^. tags)
    mnames = fmap (\x -> (nameOf x, CMeasure)) $ obj ^. measure

-- get the module declaration, create everything in it
populateWorld :: Scope -> [Decl] -> Simulator ()
populateWorld sc xs = mapM_ go xs
  where
    go :: Decl -> Simulator ()
    go (WorldDecl _ ents) = mapM_ spawn ents
    go _ = return ()

    spawn :: (Int, Ident) -> Simulator ()
    spawn (count, protoname) = zoom world $  do
      case lookupProto protoname sc of
        Just ent -> entities <>= replicate count ent
        Nothing -> error $ printf "no such entitity: %s" protoname

setupWorld :: Simulator Scope
setupWorld = do
  Module _ decls <- use imodule
  let globals = globalNames decls
  populateWorld globals decls
  return globals

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------

-- Within local context, self and root are stateful references.
self :: Local Object
self = get

root :: Local World
root = asks _croot

-- Apply a function over a measure
toMeasure :: (Integer -> Integer) -> Ident -> Local ()
toMeasure f name = do
  this <- self
  lift $ measure .= go (this ^. measure)
  where
    go [] = []
    go (m@(Measure mname mval) : ms)
      | mname == name = (Measure mname (f mval)) : go ms
      | otherwise     = m : go ms

-- Retrieve a measure of the object in scope.
getMeasure :: Ident -> Local (Maybe Measure)
getMeasure ident = lift $ do
  ms <- use measure
  case filter ((ident ==) . nameOf) ms of
    []    -> return $ Nothing
    (m:_) -> return $ Just m

-- Set a tag
setTag :: Tag -> Local ()
setTag t = lift $ tags <>= S.singleton t

-- Unset a tag
unsetTag :: Tag -> Local ()
unsetTag t = lift $ tags %= S.delete t

-- Sequence a set of actions induced by the actuators in the local state of an object.
performActions :: [Action] -> Local ()
performActions acts = mapM_ evalA acts

-- Evaluate an action, takes an action an some effect over the local world.
evalA :: Action -> Local ()
evalA ex = case ex of

  Create a -> do
    print "[Action] CREATE %s" [show a]

  Destroy a -> do
    print "[Action] DESTROY %s" [show a]

  Set a -> do
    print "[Action] SET %s" [show a]
    setTag a

  Unset a -> do
    print "[Action] UNSET %s" [show a]
    unsetTag a

  Inc m a -> do
    print "[Action] INC: %s %s" [m, show a]
    toMeasure (+ a) m

  Dec m a -> do
    print "[Action] DEC: %s %s" [m, show a]
    toMeasure (subtract a) m

  Zero m -> do
    print "[Action] ZERO: %s" [m]
    toMeasure (const 0) m

  Noop -> return ()

-- one iteration of the simulation
eventLoop :: Scope -> Int -> Simulator ()
eventLoop sc i = do
  print "[Tick] Start %s" [show i]
  stepSimulation sc
  tick
  print "[Tick] Finished %s" [show i]
  puts $ replicate 25 '-'

-- generate the monadic context for n-steps of the simulation
steps :: Int -> (Scope -> Int -> Simulator a) -> Simulator [a]
steps n f = do
  globals <- setupWorld
  iters <- forM [1..n] (f globals)
  return iters

simulate :: Module -> IO (Either SimulatorError SimState)
simulate mod = run loop state
  where
    loop = steps 15 eventLoop
    state = emptysim { _imodule = mod  }
