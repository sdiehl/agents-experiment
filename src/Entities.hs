{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Entities where

import Lens.Family
import Lens.Family.TH

import qualified Data.Set as S

-------------------------------------------------------------------------------
-- AST
-------------------------------------------------------------------------------

type Ident = String
type Tags = S.Set Tag

data Action
  = Set Tag                -- tag <tag>
  | Unset Tag              -- untag <tag>
  | Inc Ident Integer      -- dec <property> by <constant>
  | Dec Ident Integer      -- dec <property> by <constant>
  | Zero Ident             -- zero
  | Create Ident           -- create
  | Destroy Ref            -- destroy
  | Noop                   -- noop
  deriving (Eq, Ord, Show)

data Cond
  = Var Ident
  | Const Integer
  | Gt Cond Cond
  | Lt Cond Cond
  | Gte Cond Cond
  | Lte Cond Cond
  | Eq Cond Cond
  | And Cond Cond
  | Or Cond Cond
  | Not Cond
  | Has Tag
  | Tr
  | Fl
  | NotDefined
  deriving (Eq, Ord, Show)

data Receptor
  = Always
  | If Cond
  | Once Cond
  deriving (Eq, Ord, Show)

data Actuator
  = Actuator Receptor [Action]
  deriving (Eq, Ord, Show)

data Ref
  = Self -- self
  | Root -- root
  deriving (Eq, Ord, Show)

data Property
  = Base Ident     -- ident
  | Qual Ref Ident -- <ref>.ident
  deriving (Eq, Ord, Show)

data Tag
  = Tag Ident
  deriving (Eq, Ord, Show)

data Measure = Measure
  { _mname :: Ident
  , _mval :: Integer
  } deriving (Eq, Ord, Show)

data Object = Object
  { _name      :: Ident
  , _measure   :: [Measure]
  , _tags      :: Tags
  , _actuators :: [Actuator]
  } deriving (Eq, Ord, Show)

data Probe = Probe [String]
  deriving (Eq, Ord, Show)

data World = World
  { _worldname :: String
  , _entities :: [Object]
  , _nursery :: [Object]
  } deriving (Eq, Ord, Show)

emptyWorld :: World
emptyWorld = World "" [] []

notags :: Tags
notags = S.empty

mkLenses ''World
mkLenses ''Object
mkLenses ''Measure

class Named a where
  nameOf :: a -> Ident

instance Named Object where
  nameOf = _name

instance Named Measure where
  nameOf = _mname

instance Named Tag where
  nameOf (Tag a) = a

-------------------------------------------------------------------------------
-- Concrete Syntax
-------------------------------------------------------------------------------

data Module = Module String [Decl]

data Decl
 = EntityDecl Integer String
 | WorldDecl String [(Int, String)]
 | ObjectDecl Object
 | ProbeDecl Probe (Maybe String)
 deriving (Eq, Ord, Show)

data ObjDecl
  = M Measure
  | A Actuator
  deriving (Eq, Ord, Show)

mkObj :: String -> [ObjDecl] -> Object
mkObj name decls = Object name measures notags actuators
  where
    measures  = fmap unM $ filter isM decls
    actuators = fmap unA $ filter isA decls

    isM (M _) = True
    isM _ = False

    isA (A _) = True
    isA _ = False

    unA (A a) = a
    unM (M a) = a


-------------------------------------------------------------------------------
-- AST Example
-------------------------------------------------------------------------------

-- XXX: just prototyping the AST

{-
beer :: Integer -> Measure
beer = Measure "beer"

health :: Integer -> Measure
health = Measure "health"

alcohol :: Integer -> Measure
alcohol = Measure "alcohol"

timer :: Integer -> Measure
timer = Measure "timer"

thirsty :: Tag
thirsty = Tag "thirsty"

restaurant :: Object
restaurant = Object
  "restaurant"
  [beer 100]
  notags
  []

station :: Object
station = Object
  "station"
  [timer 0]
  notags
  [a1, a2]
  where
    a1 = Actuator
      (If (Var "timer" `Gte` Const 10))
      [Create "tourist"]
    a2 = Actuator Always [Inc (Qual Self "timer") 1]

tourist :: Object
tourist = Object
  "health"
  [health 10, alcohol 0, beer 0]
  notags
  [a1, a2, a3, a4, a5]
  where
    a1 = Actuator Always [Dec (Qual Self "health") 1]
    a2 = Actuator
      (If (Not ((Var "health") `Gt` (Const 0))))
      [Destroy Self]
    a3 = Actuator
      (If (((Var "health") `Lt` (Const 5)) `And` Not (Has (Tag "thirsty")) ))
      [Set thirsty]
    a4 = Actuator
      (If (((Var "health") `Gt` (Const 5)) `And` (Has (Tag "thirsty")) ))
      [Unset thirsty]
    a5 = Actuator
      (If (((Var "beer") `Gt` (Const 0))))
      [
        Inc (Qual Self "health") 5,
        Inc (Qual Self "alcohol") 1
      ]

waiter :: Object
waiter = Object
  "waiter"
  []
  notags
  []

world :: World
world = World "" entities probes nursery
  where
    entities =
         [restaurant]
      ++ [station]
      ++ (replicate 10 tourist)
      ++ (replicate 20 waiter)
    probes = []
    nursery = []
-}
