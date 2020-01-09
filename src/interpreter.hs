module Interpreter (runProgram) where


import qualified Data.Map as Map


data ProgramState = ProgramState { 
    regA :: Int,
    regC :: Int,
    regD :: Int,
    memory :: Map Int Int
}


initializeProgram :: [Int] -> ProgramState


stepProgram :: ProgramState -> ProgramState


stepProgramToEnd :: ProgramState -> ProgramState


runProgram :: [Int] -> ProgramState
