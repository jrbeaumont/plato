module Main (main) where

import Data.List
import Data.Char
import Text.Printf
import qualified Data.Text as Text
import System.Directory
import Control.Exception
import System.Environment
import System.IO.Error

import Tuura.Concept.STG
import Tuura.Concept.STG.Simulation

import qualified Language.Haskell.Interpreter as GHC
import qualified Language.Haskell.Interpreter.Unsafe as GHC

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then putStrLn "Exactly one path needed"
        else do
            r <- GHC.runInterpreter $ doWork (head args)
            case r of
               Left err -> putStrLn $ displayException err
               Right () -> return ()

{- Our own Signal type. Contains the signal index, from 0 to x-1 if
 - there are x signals. -}
data DynSignal = Signal Int deriving Eq

instance Show DynSignal where show (Signal i) = [chr (ord 'A' + i)]

{- Temporary module to help us use any number of signals in the user's
 - circuit. Otherwise, we would be bound to a number of arguments
 - (signals) known at compile time.
 - Keep the data DynSignal line in sync with the one above! -}
signalsApply :: Int -> [String]
signalsApply num = [
    "import Data.Char",
    "data DynSignal = Signal Int deriving Eq",
    "signs = [Signal i | i <- [0.." ++ show (num-1) ++ "]]",
    "apply c = c " ++ unwords ["(signs !! " ++ show i ++ ")" | i <- [0..num-1]]]

writeTmpFile :: [String] -> IO ()
writeTmpFile ls =
    writeFile tmpModuleFile $ unlines withModule
      where withModule = "module Helper where" : ls

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

{- Exported names in the user's haskell module (file) -}
circuitName, tmpModuleFile :: String
circuitName   = "circuit"
tmpModuleFile = ".Helper.hs"

{- Helper functions because we deal with String, not Text. -}
count :: String -> String -> Int
count sub str = Text.count (Text.pack sub) (Text.pack str)

strRepeat :: Int -> String -> String
strRepeat n str = Text.unpack $ Text.replicate n (Text.pack str)

loadModulesTopLevel :: [String] -> GHC.Interpreter ()
loadModulesTopLevel paths = do
    GHC.loadModules paths
    mods <- GHC.getLoadedModules
    GHC.setTopLevelModules mods

doWork :: String -> GHC.Interpreter () {- TODO: much of this is duplicated -}
doWork path = do
    {- Load user's module to gather info. -}
    loadModulesTopLevel [path]
    {- Use the circuit's type to gather how many signals it takes. -}
    t <- GHC.typeOf circuitName
    let numSigns = count "->" t
    {- Load the generated module too. -}
    liftIO $ writeTmpFile $ signalsApply numSigns
    loadModulesTopLevel [path, tmpModuleFile]
    liftIO $ removeIfExists tmpModuleFile
    {- Fetch our signals. -}
    signs <- GHC.interpret "signs" (GHC.as :: [DynSignal])
    {- Obtain the circuit in terms of any signal (takes them as args). -}
    let ctype = strRepeat numSigns "DynSignal ->" ++ "CircuitConcept DynSignal"
    circuit <- GHC.unsafeInterpret circuitName ctype
    {- Use our generated code to apply our signals to the circuit above -}
    apply <- GHC.unsafeInterpret "apply" $ "(" ++ ctype ++ ") -> CircuitConcept DynSignal"
    let fullCircuit = apply circuit
    (_, _) <- liftIO $ runSimulation (doTranslate signs fullCircuit) (State $ const False)
    return ()

doTranslate :: MonadIO m => [DynSignal] -> Concept (State DynSignal) (Transition DynSignal) DynSignal -> StateT (State DynSignal) m ()
doTranslate signs circuit = do
    case validate signs circuit of
        Valid -> do
            let initStrs = map (\s -> (show s, (getDefined $ initial circuit s))) signs
            let arcStrs = map (\(from, to) -> (show from, show to)) (arcs circuit)
            let orStrs = manageOrs (ors circuit) arcStrs
            let inputSigns = filter ((==Input) . interface circuit) signs
            let outputSigns = filter ((==Output) . interface circuit) signs
            let internalSigns = filter ((==Internal) . interface circuit) signs
            liftIO $ putStr $ genSTG inputSigns outputSigns internalSigns initStrs arcStrs orStrs
            return ()
        Invalid unused incons undef -> liftIO $ do
            putStr $ "Error. \n"
            when (unused /= []) (putStr $ "The following signals are not declared as input, output or internal: \n"
                                    ++ unlines (map show unused) ++ "\n")
            when (incons /= []) $ putStr $ "The following signals have inconsistent inital states: \n"
                                    ++ unlines (map show incons) ++ "\n"
            when (undef  /= []) $ putStr $ "The following signals have undefined initial states: \n"
                                    ++ unlines (map show undef) ++ "\n"

manageOrs :: [([Transition DynSignal], Transition DynSignal)] -> [(String, String)] -> [String]
manageOrs ors andArcs = do
    let effect = snd (head ors)
    -- Filter for just one of the effects
    let orsForEffect = filter (\o -> snd o == effect) ors
    let remainder = ors \\ orsForEffect
    -- Get just the list of lists of causes
    let causesLists = map (\l -> fst l) ors
    -- Remap these
    let mapped = mapOrs causesLists
    let n = length mapped
    -- Create arc pairs for these ors
    let orArcs = arcOrs mapped effect n
    -- Create read-arcs for these
    let orStrs = concatMap transition orArcs
    -- Add new transitions to consistency loop
    let consis = addSymbTransition effect n
    -- Include arcs for and causality to all new transitions
    let newAndArcs = updateAndArcs andArcs effect n
    -- Combine all of these
    let orStr = consis ++ orStrs ++ concatMap transition newAndArcs
    if (remainder /= []) then orStr ++ manageOrs remainder andArcs else orStr
    --if (remainder /= []) then orArcs ++ manageOrs remainder else orArcs

updateAndArcs :: [(String, String)] -> Transition DynSignal -> Int -> [(String, String)]
updateAndArcs andArcs effect n = do
    let andsForEffect = filter (\a -> snd a == (show effect)) andArcs
    concat (map (\s -> addAndArcs s n) andsForEffect)

addAndArcs :: (String, String) -> Int -> [(String, String)]
addAndArcs a n = [a] ++ map (\s -> (fst a, (snd a) ++ "/" ++ (show s))) [1..n - 1]

addSymbTransition :: Transition DynSignal -> Int -> [String]
addSymbTransition effect n
        | "+" `isSuffixOf` (show effect) = map (\x -> (printf "%s0 %s/%s\n" (init (show effect)) (show effect) (show x)) ++ (printf "%s/%s %s1" (show effect) (show x) (init (show effect)))) [1..n - 1]
        | otherwise                      = map (\x -> (printf "%s1 %s/%s\n" (init (show effect)) (show effect) (show x)) ++ (printf "%s/%s %s0" (show effect) (show x) (init (show effect)))) [1..n - 1]

mapOrs :: [[Transition DynSignal]] -> [[Transition DynSignal]]
mapOrs causesLists = addOrs (head causesLists) (tail causesLists)

addOrs :: [Transition DynSignal] -> [[Transition DynSignal]] -> [[Transition DynSignal]]
addOrs causes causesLists = do
    concat (map (\c -> addOr c causesLists) causes)

addOr :: Transition DynSignal -> [[Transition DynSignal]] -> [[Transition DynSignal]]
addOr c causesLists
        | causesLists == [] = [[c]]
        | otherwise = do
            let causes = head causesLists
            let remainder = tail causesLists
            concat (map (\d -> addOneToAll c (addOr d remainder)) causes)

addOneToAll :: Transition DynSignal -> [[Transition DynSignal]] -> [[Transition DynSignal]]
addOneToAll c causeLists
        | causeLists == [] = []
        | otherwise = [head causeLists ++ [c]] ++ addOneToAll c (tail causeLists)

arcOrs :: [[Transition DynSignal]] -> Transition DynSignal -> Int -> [(String, String)]
arcOrs causes effect n
        | n == 1 = map (\c -> (show c, show effect)) (head causes)
        | otherwise = (map (\c -> (show c, (show effect  ++ "/" ++ show (n - 1)))) (head causes)) ++ arcOrs (tail causes) effect (n - 1)

output :: [(String, Bool)] -> [String]
output = sort . nub . map fst

symbLoop :: String -> [String]
symbLoop s = map (\f -> printf f s s) ["%s0 %s+", "%s+ %s1", "%s1 %s-", "%s- %s0"]

readArc :: String -> String -> [String]
readArc f t = [f ++ " " ++ t, t ++ " " ++ f]

transition :: (String, String) -> [String]
transition (f, t)
        | "+" `isSuffixOf` f = readArc (init f ++ "1") t
        | otherwise          = readArc (init f ++ "0") t

initVals :: [String] -> [(String, Bool)] -> [String]
initVals l symbInits = foldr (\s -> (++) [printf "%s%i" s $ initVal s symbInits]) [] l

boolBit :: Bool -> Int
boolBit b = if b then 1 else 0

initVal :: String -> [(String, Bool)] -> Int
initVal s ((ls,v):l)
        | s == ls = boolBit v
        | otherwise = initVal s l
initVal _ _ = 0

tmpl :: String
tmpl = unlines [".model out", ".inputs %s", ".outputs %s", ".internals %s", ".graph", "%s%s.marking {%s}", ".end"]

printOrs :: (String, String) -> [String]
printOrs (f, t) = [f ++ " " ++ t]

genSTG :: [DynSignal] -> [DynSignal] -> [DynSignal] -> [(String, Bool)] -> [(String, String)] -> [String] -> String
genSTG inputSigns outputSigns internalSigns initStrs arcStrs orStrs =
    printf tmpl (unwords ins) (unwords outs) (unwords ints) (unlines trans) (unlines orStrs) (unwords marks)
    where
        allSigns = output initStrs
        outs = map show outputSigns
        ins = map show inputSigns
        ints = map show internalSigns
        trans = concatMap symbLoop allSigns ++ concatMap transition arcStrs
        marks = initVals allSigns initStrs
        --ors = concatMap transition orStrs

data ValidationResult a = Valid | Invalid [a] [a] [a] deriving Eq

validate :: Eq a => [a] -> CircuitConcept a -> ValidationResult a
validate signs circuit
    | unused ++ inconsistent ++ undef == [] = Valid
    | otherwise                             = Invalid unused inconsistent undef
  where
    unused       = filter ((==Unused) . interface circuit) signs
    inconsistent = filter ((==Inconsistent) . initial circuit) signs
    undef        = filter ((==Undefined) . initial circuit) signs
