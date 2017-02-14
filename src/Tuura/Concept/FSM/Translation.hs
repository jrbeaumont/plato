module Tuura.Concept.FSM.Translation where

import Data.List
import Data.List.Extra
import Data.Ord (comparing)
import Control.Monad
import Data.Char  (digitToInt)
import Data.Maybe (fromMaybe, listToMaybe)
import Numeric    (readInt)
import Text.Printf

import Tuura.Concept.FSM

import Tuura.Plato.Translation

import qualified Language.Haskell.Interpreter as GHC
import qualified Language.Haskell.Interpreter.Unsafe as GHC

-- Type aliases
-- type Causality a = [([Transition a], Transition a)]
type CausalityX a = [([TransitionX a], Transition a)]

-- Transition type using Tristate. Used for initial construction of arcs.
data TransitionX a = TransitionX
    {
        msignal   :: a,
        mnewValue :: Tristate -- Transition x True corresponds to x+
    }
    deriving Eq

instance Show a => Show (TransitionX a) where
    show (TransitionX s (Tristate (Just True) )) = show s ++ "+"
    show (TransitionX s (Tristate (Just False))) = show s ++ "-"
    show (TransitionX s (Tristate Nothing     )) = show s ++ "x"

data Tristate = Tristate (Maybe Bool)
    deriving Eq

instance Show Tristate where
    show (Tristate (Just True) ) = "1"
    show (Tristate (Just False)) = "0"
    show (Tristate Nothing     ) = "x"

triTrue = Tristate (Just True)
triFalse = Tristate (Just False)
triX = Tristate Nothing

-- FSM arc type used during state expansion
data FsmArcX a = FsmArcX
    {
        srcEncx :: [Tristate],
        transx :: Transition a,
        destEncx :: [Tristate]
    }

instance Show a => Show (FsmArcX a) where
    show (FsmArcX senc tran tenc) = "(" ++ show senc ++ " " ++ show tran ++ " " ++ show tenc ++ ")\n"

-- Final FSM arc type using Ints for state encoding
data FsmArc a = FsmArc
    {
        srcEnc :: Int,
        trans :: Transition a,
        destEnc :: Int
    }

instance Show a => Show (FsmArc a) where
    show (FsmArc senc tran tenc) = "s" ++ show senc ++ " " ++ show tran ++ " s" ++ show tenc

-- data ValidationResult a = Valid | Invalid [a] [a] [a] deriving Eq

translateFSM :: (Show a, Ord a) => String -> String -> [a] -> GHC.Interpreter ()
translateFSM circuitName ctype signs = do
    circ <- GHC.unsafeInterpret circuitName ctype
    apply <- GHC.unsafeInterpret "apply" $ "(" ++ ctype ++ ") -> CircuitConcept Signal"
    let circuit = apply circ
    case validate signs circuit of
        Valid -> do
            --let initStrs = map (\s -> (show s, (getDefined $ initial circuit s))) signs
            let allSigns = map show signs
            let sortedArcs = concatMap handleArcs (groupSortOn snd (arcs circuit))
            let arcStrs = map show (createAllArcs sortedArcs)-- concatMap handleArcs (groupSortOn snd (arcs circuit))
            let inputSigns = filter ((==Input) . interface circuit) signs
            let outputSigns = filter ((==Output) . interface circuit) signs
            let internalSigns = filter ((==Internal) . interface circuit) signs
            GHC.liftIO $ putStr (genFSM inputSigns outputSigns internalSigns arcStrs allSigns) -- initStrs)
        Invalid unused incons undef ->
            GHC.liftIO $ putStr ("Error. \n" ++ addErrors unused incons undef)

validate :: Eq a => [a] -> CircuitConcept a -> ValidationResult a
validate signs circuit
    | unused ++ inconsistent ++ undef == [] = Valid
    | otherwise                             = Invalid unused inconsistent undef
  where
    unused       = filter ((==Unused) . interface circuit) signs
    inconsistent = filter ((==Inconsistent) . initial circuit) signs
    undef        = filter ((==Undefined) . initial circuit) signs

handleArcs :: Show a => [([Transition a], Transition a)] -> [([Transition a], Transition a)]
handleArcs arcLists = result
        where
            effect = snd (head arcLists)
            effectCauses = map fst arcLists
            transCauses = cartesianProduct effectCauses
            result = map (\m -> (m, effect)) transCauses
            -- n = length transCauses
            -- arcMap = concat (map (\m -> arcPairs m effect) (zip transCauses [0..(n-1)]))

-- Create .sg file string
-- genFSM :: (Show a, Ord a) => [([Transition a], Transition a)] -> String
-- genFSM causality = printf tmpl (unlines showArcs) initialMarking
--     where arcs = createAllArcs causality
--           showArcs = map show arcs
--           initialMarking = "s" ++ show (srcEnc (head arcs)) -- TODO: Implement properly!

genFSM :: Show a => [a] -> [a] -> [a] -> [String] -> [String] -> String -- -> [(String, Bool)] -> String
genFSM inputSigns outputSigns internalSigns arcStrs allSigns =
     printf tmpl (unwords ins) (unwords outs) (unwords ints) (unlines arcStrs) -- (unwords marks)
    where
        --allSigns = output initStrs
        outs = map show outputSigns
        ins = map show inputSigns
        ints = map show internalSigns
        --allArcs = concatMap consistencyLoop allSigns ++ arcStrs
        --marks = initVals allSigns initStrs

tmpl :: String
tmpl = unlines [".inputs %s", ".outputs %s", ".internals %s", ".state graph", "%s.marking {s0}", ".end"]

fullList :: ([a], a) -> [a]
fullList (l,t) = t:l

fullListm :: ([TransitionX a], Transition a) -> [TransitionX a]
fullListm (l,t) = (toTransitionX t):l

-- Given [([a], b)], remove all b from a
removeDupes :: Eq a => [([Transition a], Transition a)] -> [([Transition a], Transition a)]
-- (filter ((/= ((signal . snd) x)) . signal) (fst x), snd x)
removeDupes = map (ap ((,) . ap (filter . (. signal) . (/=) . signal . snd) fst) snd)

toTransitionX :: Transition a -> TransitionX a
toTransitionX = liftM2 TransitionX signal (Tristate . Just . newValue)

-- Extract signal list from transition list
onlySignals :: Eq a => [[Transition a]] -> [[a]]
onlySignals = map (map signal)

-- Find all signal names in design
getAllSignals :: Ord a => [[Transition a]] -> [a]
getAllSignals = sort . foldl union [] . onlySignals

addMissingSignals :: Ord a => [([Transition a], Transition a)] -> CausalityX a
addMissingSignals x = zip (zipWith (++) newTransitions oldTransitions) (map snd noDupes)
    where noDupes = removeDupes x
          oldTransitions = map (map toTransitionX . fst) noDupes
          newTransitions = ((map . map) (flip TransitionX triX) . missingSignals . transitionList) noDupes
          transitionList =  map fullList
          missingSignals y = map (getAllSignals y \\) (onlySignals y)

encode :: Ord a => [TransitionX a] -> [Tristate]
encode  = map mnewValue . sortTransitions
    where sortTransitions = sortBy (comparing msignal)

createArcs :: Ord a => [([Transition a], Transition a)] -> [FsmArcX a]
createArcs xs = zipWith3 createArc makeSrcEncs makeDestEncs activeTransitions
    where createArc senc tenc transx = FsmArcX senc transx tenc
          makeDestEncs = a xs
          makeSrcEncs = (a . map flipTransition) xs
          a = map (encode . fullListm) . addMissingSignals
          flipTransition x = (fst x, (negate . snd) x)
          negate = liftM2 Transition signal (not . newValue)
          activeTransitions = (map snd .  addMissingSignals) xs

replaceAtIndex item ls n = a ++ (item:b)
    where (a, (_:b)) = splitAt n ls

expandX :: FsmArcX a -> [FsmArcX a]
expandX xs = case elemIndex triX (srcEncx xs) of
               Nothing -> [xs]
               Just n  -> do
                let newTrue = makeArc (replaceAtIndex triTrue (srcEncx xs) n)
                                       (replaceAtIndex triTrue (destEncx xs) n)
                let newFalse = makeArc (replaceAtIndex triFalse (srcEncx xs) n)
                                  (replaceAtIndex triFalse (destEncx xs) n)
                let result = [newTrue, newFalse]
                expandX newTrue ++ expandX newFalse
                  where makeArc s d = FsmArcX s (transx xs) d

-- -- Expand source state so no X's remain
-- expandX :: FsmArcX a -> [FsmArcX a]
-- expandX xs = case elemIndex triX (srcEncx xs) of
--                Nothing -> [xs]
--                Just n  -> [makeArc (replaceAtIndex triTrue (srcEncx xs) n)
--                                    (replaceAtIndex triTrue (destEncx xs) n),
--                            makeArc (replaceAtIndex triFalse (srcEncx xs) n)
--                                    (replaceAtIndex triFalse (destEncx xs) n)]
--                                where makeArc s d = FsmArcX s (transx xs) d

expandAllXs :: [FsmArcX a] -> [FsmArcX a]
expandAllXs = concatMap expandX

-- http://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
-- covert string to int
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

-- State encoding to Sn where n is reverse of encoding in b10
encToInt :: [Tristate] -> Int
encToInt enc = fromMaybe 0 ((readBin . concatMap show . reverse) enc)

--
fsmarcxToFsmarc :: FsmArcX a -> FsmArc a
fsmarcxToFsmarc arc = FsmArc newSourceEnc (transx arc) newDestEnc
    where newSourceEnc = (encToInt . srcEncx) arc
          newDestEnc = (encToInt . destEncx) arc

-- Produce all arcs with all X's resolved
createAllArcs :: Ord a => [([Transition a], Transition a)] -> [FsmArc a]
createAllArcs = map fsmarcxToFsmarc . expandAllXs . createArcs
