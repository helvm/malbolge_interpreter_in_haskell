--module Interpreter (runProgram) where


import qualified Data.Map as Map


data ProgramState = ProgramState {
    addrMax :: Int,
    regA :: Int,
    regC :: Int,
    regD :: Int,
    memory :: Map.Map Int Int
}
    deriving Show

createZeroedMemory :: Int -> Map.Map Int Int
createZeroedMemory size = 
    czmAux Map.empty (size - 1)
    where czmAux mem (-1) = mem
          czmAux mem i    = czmAux (Map.insert i 0 mem) (i - 1)

initializeProgramState :: Int -> ProgramState
initializeProgramState maxAddress = 
    ProgramState {
        addrMax = maxAddress,
        regA = 0, regC = 0, regD = 0,
        memory = createZeroedMemory maxAddress
    }

updateMemory :: Int -> Int -> ProgramState -> ProgramState
updateMemory address value state =
    state {
        memory = Map.alter (\_ -> Just value) address (memory state) 
    }

loadProgram :: [Int] -> ProgramState -> ProgramState
loadProgram [] = 

--initializeMemory :: ProgramState -> ProgramState


--stepProgram :: ProgramState -> ProgramState


--stepProgramToEnd :: ProgramState -> ProgramState


--runProgram :: ProgramState -> ProgramState
