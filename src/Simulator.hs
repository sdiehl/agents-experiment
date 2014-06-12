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

import Entities as E
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
evalC :: LocalCtx -> Cond -> Local Cond
evalC ctx expr = ev expr
  where
    ev :: Cond -> Local Cond
    ev (Var x) = do
      var <- lookupCond x ctx
      case var of
        Just n -> ev n
        Nothing -> lift $ lift $ throwT (NameError x)

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

      Not x     -> ev =<< Not <$> (ev x)
      And a b   -> ev =<< And <$> (ev a) <*> (ev b)
      Or a b    -> ev =<< Or  <$> (ev a) <*> (ev b)

      Gt  (Const a) (Const b) -> pure $ if a > b  then Tr else Fl
      Gte (Const a) (Const b) -> pure $ if a >= b then Tr else Fl
      Lt  (Const a) (Const b) -> pure $ if a > b  then Tr else Fl
      Lte (Const a) (Const b) -> pure $ if a <= b then Tr else Fl
      Eq  (Const a) (Const b) -> pure $ if a == b then Tr else Fl

      Gt a b  -> ev =<< Gt  <$> (ev a) <*> (ev b)
      Gte a b -> ev =<< Gte <$> (ev a) <*> (ev b)
      Lt a b  -> ev =<< Lt  <$> (ev a) <*> (ev b)
      Lte a b -> ev =<< Lte <$> (ev a) <*> (ev b)
      Eq a b  -> ev =<< Eq  <$> (ev a) <*> (ev b)


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
  { _ticks  :: Integer
  , _world  :: World
  , _setup  :: [Decl] -- XXX: what should this be?
  , _probes :: [Probe]
  } deriving (Eq, Show)

mkLenses ''SimState

emptysim :: SimState
emptysim = SimState 0 emptyWorld [] []

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
runActuator (Actuator rec acts) = case rec of -- runReceptor rec acts

  -- always applies
  Always -> do
    ctx <- ask
    performActions ctx acts

  -- conditionally applies
  If cond -> do
    ctx <- ask
    condtest <- evalC ctx cond
    print "[Cond] %s ==> %s" [show cond, show condtest]
    if condToBool condtest
    then performActions ctx acts
    else return ()

  Once cond -> do
    undefined

runActuators :: [Actuator] -> Local ()
runActuators = mapM_ runActuator

inScope :: r -> ReaderT r m a -> m a
inScope sc m = runReaderT m sc

runActions :: Scope -> Simulator ()
runActions globals = do
  root <- use world
  decls <- use setup

  -- traverse the world, and run the logic associated with each entity
  objs <- zoom (world.entities.traverse) $ do
    self <- get
    let locals = localNames self
    let ctx = LocalCtx self root globals locals [] False
    {-pshow ctx-}

    nm <- use name
    print "[Entity] %s" [nm]

    -- in local scope of object execute actuators
    as <- use actuators
    inScope ctx (runActuators as)

    -- if object still exists then return for next tick
    self <- get
    return [self]

  world.entities .= objs

  return ()

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

populateWorld :: Scope -> [Decl] -> Simulator ()
populateWorld sc xs = mapM_ go xs
  where
    -- get the world declaration, create everything in it

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
  root <- use world
  decls <- use setup
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
performActions :: LocalCtx -> [Action] -> Local ()
performActions ctx acts = mapM_ evalA acts

-- Evaluate an action, takes an action an some effect over the local world.
evalA :: Action -> Local ()
evalA ex = case ex of

  Create a -> do
    print "[Action] CREATE %s" [show a]

  Destroy a -> do
    print "[Action] DESTROY %s" [show a]

  Set a -> do
    print "[Action] SET %s" [show a]

  Unset a -> do
    print "[Action] UNSET %s" [show a]
    setTag a

  Inc m a -> do
    print "[Action] INC: %s %s" [m, show a]
    toMeasure (+ a) m

  Dec m a -> do
    print "[Action] DEC: %s %s" [m, show a]
    toMeasure (subtract a) m

  Zero m -> do
    print "[Action] ZERO: %s" [m]
    toMeasure (const 0) m

  act -> throw $ NotImplemented (show act)

eventLoop :: Scope -> Int -> Simulator ()
eventLoop sc i = do
  print "[Tick] Start %s" [show i]
  runActions sc
  tick
  print "[Tick] Finished %s" [show i]
  puts $ replicate 25 '-'

steps :: Int -> (Scope -> Int -> Simulator a) -> Simulator [a]
steps n f = do
  globals <- setupWorld
  iters <- forM [1..n] (f globals)
  return iters

simulate :: [Decl] -> IO (Either SimulatorError SimState)
simulate decls = run loop state
  where
    loop = steps 15 eventLoop
    state = emptysim { _setup = decls }
