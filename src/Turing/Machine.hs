-- | Deterministic Turing machine representation and helpers.
--
-- This module models Turing machines, validates transition tables, and offers
-- combinators for renaming and composing machines.
module Turing.Machine
  ( StateId (..)
  , state
  , stateIndex
  , stateMarker
  , Direction (..)
  , Transition (..)
  , Machine (..)
  , MachineError (..)
  , mkMachine
  , mapStates
  , offsetStates
  , chainMachines
  ) where

import qualified Data.Map.Strict  as Map
import           Data.Map.Strict  (Map)
import qualified Data.Set         as Set
import           Data.Set         (Set)

-- | Identifier for a Turing machine state.
newtype StateId = StateId { unStateId :: Int }
  deriving (Eq, Ord, Show)

-- | Construct a 'StateId' from an integer.
state :: Int -> StateId
state = StateId

-- | Extract the numeric index of a 'StateId'.
stateIndex :: StateId -> Int
stateIndex (StateId n) = n

-- | Map a state index to an uppercase marker character.
stateMarker :: StateId -> Char
stateMarker (StateId n)
  | n < 0     = error "stateMarker: negative state index"
  | otherwise = toEnum (fromEnum 'A' + n)

-- | Direction of head movement for a transition.
data Direction = MoveLeft | MoveRight | Stay
  deriving (Eq, Show)

-- | Transition action resulting from reading a symbol.
data Transition = Transition
  { transitionNext  :: !StateId   -- ^ State to enter after applying the rule.
  , transitionWrite :: !Char      -- ^ Symbol to write onto the tape.
  , transitionMove  :: !Direction -- ^ Head movement direction.
  }
  deriving (Eq, Show)

-- | Complete description of a deterministic Turing machine.
data Machine = Machine
  { machineBlank        :: !Char                     -- ^ Blank tape symbol.
  , machineTapeAlphabet :: !(Set Char)               -- ^ Allowed tape alphabet.
  , machineInitial      :: !StateId                  -- ^ Initial state.
  , machineHalting      :: !(Set StateId)            -- ^ Halting states.
  , machineTransitions  :: !(Map (StateId, Char) Transition) -- ^ Transition table.
  }
  deriving (Eq, Show)

-- | Error raised when an invalid machine is constructed.
data MachineError = MachineError { machineErrorMessage :: String }
  deriving (Eq, Show)

-- | Validate and assemble a deterministic Turing machine.
mkMachine
  :: Char
  -> Set Char
  -> StateId
  -> Set StateId
  -> Map (StateId, Char) Transition
  -> Either MachineError Machine
mkMachine blank alphabet initial halting transitions = do
  ensure (Set.member blank alphabet) "blank symbol is not part of the tape alphabet"
  ensure (all (`Set.member` alphabet) readSymbols)
    "transition reads must belong to the tape alphabet"
  ensure (all (`Set.member` alphabet) writeSymbols)
    "transition writes must belong to the tape alphabet"
  pure Machine
    { machineBlank = blank
    , machineTapeAlphabet = alphabet
    , machineInitial = initial
    , machineHalting = halting
    , machineTransitions = transitions
    }
  where
    ensure condition message
      | condition = Right ()
      | otherwise = Left (MachineError message)

    readSymbols = [ sym | ((_, sym), _) <- Map.toList transitions ]
    writeSymbols = [ transitionWrite t | t <- Map.elems transitions ]

-- | Apply a mapping to every state that appears in the machine.
mapStates :: (StateId -> StateId) -> Machine -> Machine
mapStates f machine = machine
  { machineInitial = f (machineInitial machine)
  , machineHalting = Set.map f (machineHalting machine)
  , machineTransitions = Map.fromList
      [ ( (f srcState, sym)
        , t { transitionNext = f (transitionNext t) }
        )
      | ((srcState, sym), t) <- Map.toList (machineTransitions machine)
      ]
  }

-- | Shift all state indices by the provided delta.
offsetStates :: Int -> Machine -> Machine
offsetStates delta = mapStates (\(StateId n) -> StateId (n + delta))

-- | Combine two machines by chaining the halting states of the first into the
-- initial state of the second.
chainMachines :: Machine -> Machine -> Either MachineError Machine
chainMachines first second
  | machineBlank first /= machineBlank second =
      Left (MachineError "blank symbols must match to chain machines")
  | otherwise = do
      ensureNoOutgoingHalting first
      let offset = succ (maximumStateIndex first)
          second' = offsetStates offset second
          alphabet = machineTapeAlphabet first `Set.union` machineTapeAlphabet second
          bridge = buildBridge alphabet (machineHalting first) (machineInitial second')
          transitions1 = machineTransitions first
          transitions2 = machineTransitions second'
      ensureDisjoint transitions1 transitions2
      ensureDisjoint transitions1 bridge
      ensureDisjoint bridge transitions2
      pure Machine
        { machineBlank = machineBlank first
        , machineTapeAlphabet = alphabet
        , machineInitial = machineInitial first
        , machineHalting = machineHalting second'
        , machineTransitions = transitions1 `Map.union` bridge `Map.union` transitions2
        }
  where
    ensureNoOutgoingHalting machine =
      let outgoing =
            [ ()
            | ((srcState, _), _) <- Map.toList (machineTransitions machine)
            , srcState `Set.member` machineHalting machine
            ]
      in case outgoing of
           [] -> Right ()
           _  -> Left (MachineError "halting states cannot have outgoing transitions")

    ensureDisjoint a b =
      case Map.keys (Map.intersection a b) of
        [] -> Right ()
        _  -> Left (MachineError "machines share transitions after chaining")

    buildBridge alphabet haltingStates target = Map.fromList
      [ ((haltState, sym), Transition target sym Stay)
      | haltState <- Set.toList haltingStates
      , sym <- Set.toList alphabet
      ]

-- | Determine the largest state index present in a machine.
maximumStateIndex :: Machine -> Int
maximumStateIndex machine = foldl' max initialIndex allIndices
  where
    initialIndex = stateIndex (machineInitial machine)
    allIndices =
      initialIndex
        : map stateIndex (Set.toList (machineHalting machine))
        ++ [ stateIndex srcState | ((srcState, _), _) <- Map.toList (machineTransitions machine) ]
        ++ [ stateIndex (transitionNext t) | t <- Map.elems (machineTransitions machine) ]
