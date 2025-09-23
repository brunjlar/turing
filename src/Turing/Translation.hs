-- | Compile Turing machines into rewrite rules.
--
-- The translation preserves machine behaviour by encoding the head position
-- and state marker into rewrite rules understood by the Rules engine.
module Turing.Translation
  ( TapeEncoding (..)
  , defaultTapeEncoding
  , compileMachine
  , initialTape
  ) where

import           Data.List        (sort)
import qualified Data.Map.Strict  as Map
import qualified Data.Set         as Set

import           Rewrite          (Rule (..), Rules)
import           Turing.Machine   (Direction (..), Machine (..), MachineError (..),
                                   StateId, stateMarker, transitionMove,
                                   transitionNext, transitionWrite)
import qualified Turing.Machine   as TM

-- | Symbols used to bound the tape when translating machines.
data TapeEncoding = TapeEncoding
  { tapeLeftBoundary  :: !Char -- ^ Marker for the left edge of the tape.
  , tapeRightBoundary :: !Char -- ^ Marker for the right edge of the tape.
  }
  deriving (Eq, Show)

-- | Default tape encoding with @<@ and @>@ sentinels.
defaultTapeEncoding :: TapeEncoding
defaultTapeEncoding = TapeEncoding '<' '>'

-- | Translate a machine into equivalent rewrite rules.
compileMachine :: TapeEncoding -> Machine -> Rules Char
compileMachine encoding machine = concatMap (compileEntry encoding machine) entries
  where
    entries = Map.toList (machineTransitions machine)

compileEntry :: TapeEncoding -> Machine -> ((StateId, Char), TM.Transition) -> Rules Char
-- | Internal helper that renders the rules for a single transition.
compileEntry (TapeEncoding left right) machine ((state, readSym), trans) =
  case transitionMove trans of
    MoveLeft  -> map leftRule leftContexts
    MoveRight -> map rightRule rightContexts
    Stay      -> [Rule [marker state, readSym] [marker (transitionNext trans), transitionWrite trans]]
  where
    marker = stateMarker
    blank = machineBlank machine
    alphabet = sort (Set.toList (machineTapeAlphabet machine))
    leftContexts = left : alphabet
    rightContexts = alphabet ++ [right]

    leftRule neighbor
      | neighbor == left =
          Rule [left, marker state, readSym]
               [left, marker (transitionNext trans), blank, transitionWrite trans]
      | otherwise =
          Rule [neighbor, marker state, readSym]
               [marker (transitionNext trans), neighbor, transitionWrite trans]

    rightRule neighbor
      | neighbor == right =
          Rule [marker state, readSym, right]
               [transitionWrite trans, marker (transitionNext trans), blank, right]
      | otherwise =
          Rule [marker state, readSym, neighbor]
               [transitionWrite trans, marker (transitionNext trans), neighbor]

-- | Construct the initial tape layout for a machine and user input.
initialTape :: TapeEncoding -> Machine -> [Char] -> Either MachineError [Char]
initialTape (TapeEncoding left right) machine input = do
  ensure (all (`Set.member` alphabet) input)
    "initial tape contains symbols outside the tape alphabet"
  pure (left : marker initialState : cells ++ [right])
  where
    marker = stateMarker
    alphabet = machineTapeAlphabet machine
    initialState = machineInitial machine
    blank = machineBlank machine
    cells = case input of
      []     -> [blank]
      (c:cs) -> c : cs

    ensure condition message
      | condition = Right ()
      | otherwise = Left (MachineError message)
