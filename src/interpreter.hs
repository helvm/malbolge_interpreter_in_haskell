module Interpreter (runProgram) where

import qualified Data.Map as Map

data ProgramState = ProgramState {
    addrMax :: Int,
    regA :: Int,
    regC :: Int,
    regD :: Int,
    memory :: Map.Map Int Int
}
    deriving Show

intToTernary :: Int -> [Int]
intToTernary num = ittAux n []
    where ittAux 0 a = a
          ittAux n a = ittAux (div n 3) ((mod n 3):a)

ternaryToInt :: [Int] -> Int
ternaryToInt trits = ttiAux trits 0 (length trits)
    where ttiAux [] a _     = a
          ttiAux (t:ts) a i = ttiAux ts (t * 3 ^ i + a) (i - 1)

padTernary :: Int -> [Int] -> [Int]
padTernary len trits = 
    if length trits >= len then
        trits
    else
        padTernary len (0:trits)

crazyOperation :: Int -> Int -> Int
crazyOperation a b =
    let aTrits = padTernary (intToTernary a) in
        let btrits = padTernary (intToTernary b) in
            coAux aTrits bTrits []
                where coAux [] [] cTrits             = cTrits
                      coAux [] (bt:bts) cTrits       = 
                      coAux (at:ats) [] cTrits       =
                      coAux (at:ats) (bt:bts) cTrits = 


createZeroedMemory :: Int -> Map.Map Int Int
createZeroedMemory size = czmAux Map.empty (size - 1)
    where czmAux mem (-1) = mem
          czmAux mem i    = czmAux (Map.insert i 0 mem) (i - 1)

initializeProgramState :: Int -> ProgramState
initializeProgramState maxAddress = ProgramState {
    addrMax = maxAddress,
    regA = 0, regC = 0, regD = 0,
    memory = createZeroedMemory maxAddress
}

updateMemory :: Int -> Int -> ProgramState -> ProgramState
updateMemory address value state = state {
    memory = Map.alter (\_ -> Just value) address (memory state) 
}

loadProgram :: [Int] -> ProgramState -> ProgramState
loadProgram values state = lpAux values state 0
    where lpAux [] state _     = state
          lpAux (x:xs) state i = lpAux xs (updateMemory i x state) (i + 1)

initializeMemory :: ProgramState -> ProgramState


--stepProgram :: ProgramState -> ProgramState


--stepProgramToEnd :: ProgramState -> ProgramState


--runProgram :: ProgramState -> ProgramState
