module Interpreter (runProgram) where

import qualified Data.Map as Map

data ProgramState = ProgramState {
    addrMax :: Int,
    progEnd :: Int,
    terminated :: Bool,
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

tritCrazyOperation :: Int -> Int -> Int
tritCrazyOperation a b =
    | a == b && a != 1 = 1
    | a == b && a = 1  = 0
    | a == 0           = 0
    | a == 2           = 2
    | a == 1 && b == 0 = 1
    | a == 1 && b == 2 = 2
    | otherwise        = 0

crazyOperation :: Int -> Int -> Int
crazyOperation a b =
    let aTrits = padTernary (intToTernary a) in
        let bTrits = padTernary (intToTernary b) in
            coAux aTrits bTrits []
                where coAux [] [] cTrits             = reverse cTrits
                      coAux [] (bt:bts) cTrits       = coAux [] bts ((tritCrazyOperation 0 bt):cTrits)
                      coAux (at:ats) [] cTrits       = coAux ats [] ((tritCrazyOperation at 0):cTrits)
                      coAux (at:ats) (bt:bts) cTrits = coAux ats bts ((tritCrazyOperation at bt):cTrits)


createZeroedMemory :: Int -> Map.Map Int Int
createZeroedMemory size = czmAux Map.empty (size - 1)
    where czmAux mem (-1) = mem
          czmAux mem i    = czmAux (Map.insert i 0 mem) (i - 1)

initializeProgramState :: Int -> ProgramState
initializeProgramState maxAddress = ProgramState {
    addrMax = maxAddress,
    progEnd = 1,
    terminated = False,
    regA = 0, regC = 0, regD = 0,
    memory = createZeroedMemory maxAddress
}

updateMemoryValue :: Int -> Int -> ProgramState -> ProgramState
updateMemoryValue address value state = state {
    memory = Map.alter (\_ -> Just value) address (memory state) 
}

getMemoryValue :: Int -> Map.Map Int Int -> Int
getMemoryValue address mem = case Map.lookup address mem of
    Nothing -> 0
    Some x  -> x

loadProgram :: [Int] -> ProgramState -> ProgramState
loadProgram values state = lpAux values (state { progEnd = max (length values) 1 }) 0
    where lpAux [] state _     = state
          lpAux (x:xs) state i = lpAux xs (updateMemoryValue i x state) (i + 1)

initialMemoryCellValue :: Int -> Int -> Map.Map Int Int -> Int
initialMemoryCellValue i j mem = crazyOperation (getMemoryValue j mem) (getMemoryValue i mem)

initializeMemory :: ProgramState -> ProgramState
initializeMemory state = imAux (progEnd state) ((progEnd state) - 1) state
    where imAux i j s
            | i >= (maxAddress state) = s
            | otherwise               = imAux i (i + 1) (updateMemoryValue (i + 1) (initialMemoryCellValue i j (memory s)) s)

instruction4Jmp :: ProgramState -> ProgramState
instruction4Jmp state = state { regC = getMemoryValue (regD state) (memory state) }

instruction5Out :: ProgramState -> ProgramState


instruction23In :: ProgramState -> ProgramState


instruction39RotrMov :: ProgramState -> ProgramState


instruction40Mov :: ProgramState -> ProgramState
instruction40Mov state = state { regD = getMemoryValue (regD state) (memory state) }

instruction62CrzMov :: ProgramState -> ProgramState
instruction62CrzMov state = 
    let crzda = crazyOperation (getMemoryValue (regD state) (memory state)) (regA state) in
        updateMemoryValue (regD state) crzda (state { regA = crzda })

instruction68Nop :: ProgramState -> ProgramState
instruction68Nop state = state

instruction81End :: ProgramState -> ProgramState
instruction81End state = state { terminated = True }

stepProgram :: ProgramState -> ProgramState
stepProgram state = spAux (regC state)
    where spAux i 
            | i == 4  =
            | i == 5  =
            | i == 23 =  
            | i == 39 = 
            | i == 40 = 
            | i == 62 = 
            | i == 68 = 
            | i == 81 = 

getNextInstruction :: ProgramState -> Int
getNextInstruction state = case (Map.lookup (regC state) (memory state)) of
    Nothing -> mod (regC state) 94
    Some x  -> mod ((regC state) + x) 94

runProgram :: ProgramState -> ProgramState
