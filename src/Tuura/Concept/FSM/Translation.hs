import Data.List
import Data.Ord (comparing)
import Control.Monad
import Data.Char  (digitToInt)
import Data.Maybe (fromMaybe, listToMaybe)
import Numeric    (readInt)
import Text.Printf

type Causality a = [([Transition a], Transition a)]

data Transition a = Transition
    {
        signal   :: a,
        newValue :: Bool -- Transition x True corresponds to x+
    }
    deriving Eq

instance Show a => Show (Transition a) where
    show (Transition s True ) = show s ++ "+"
    show (Transition s False) = show s ++ "-"

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

rise :: a -> Transition a
rise a = Transition a True

fall :: a -> Transition a
fall a = Transition a False

data Tristate = Tristate (Maybe Bool)
    deriving Eq

instance Show (Tristate) where
    show (Tristate (Just True) ) = "1"
    show (Tristate (Just False)) = "0"
    show (Tristate Nothing     ) = "x"

data FsmArcX a = FsmArcX
    {
        srcEncx :: [Tristate],
        transx :: Transition a,
        destEncx :: [Tristate]
    }

instance Show a => Show (FsmArcX a) where
    show (FsmArcX senc tran tenc) = "(" ++ show senc ++ " " ++ show tran ++ " " ++ show tenc ++ ")\n"

data FsmArc a = FsmArc
    {
        srcEnc :: Int,
        trans :: Transition a,
        destEnc :: Int
    }

instance Show a => Show (FsmArc a) where
    show (FsmArc senc tran tenc) = "s" ++ show senc ++ " " ++ show tran ++ " s" ++ show tenc

data Signal = A | B | C | D deriving (Eq, Ord, Show, Enum)

list a b c d = [([rise a, fall c, rise d], rise c),
                ([rise b, fall c, rise d], rise c),
                ([rise b, rise c, fall d], rise a)
               ]

andrey a b c d = [([rise a, fall c, rise d], rise c),
                  ([rise b, fall c, rise d], rise c)
                 ]

trs a b c = [rise a, fall b, rise c]

y = andrey A B C D
b = [Tristate (Just True), Tristate (Just True), Tristate (Just False)]

genFSM :: (Show a, Ord a) => Causality a -> String
genFSM causality = printf tmpl (unlines showArcs) initialMarking
    where arcs = stateArcs causality
          showArcs = map show arcs
          initialMarking = "s" ++ show (srcEnc (head arcs)) -- TODO: Implement properly!

fullList :: ([a], a) -> [a]
fullList (l,t) = t:l

fullListm :: ([TransitionX a], Transition a) -> [TransitionX a]
fullListm (l,t) = (toTransitionX t):l

-- Given [([a], b)], remove all b from a
removeDupes :: Eq a => Causality a -> Causality a
--(filter ((/= ((signal . snd) x)) . signal) (fst x), snd x)
removeDupes = map (ap ((,) . ap (filter . (. signal) . (/=) . signal . snd) fst) snd)

toTransitionX :: Transition a -> TransitionX a
toTransitionX = (liftM2 TransitionX signal (Tristate . Just . newValue))

onlySignals :: Eq a => [[Transition a]] -> [[a]]
onlySignals = map (map signal)

getAllSignals :: Ord a => [[Transition a]] -> [a]
getAllSignals = (sort . foldl union ([]) . onlySignals)

missingSignals :: Ord a => [[Transition a]] -> [[a]]
missingSignals x = map ((\\) (getAllSignals x)) (onlySignals x)

addMissingSignals :: Ord a => Causality a -> [([TransitionX a], Transition a)]
addMissingSignals x = zip (zipWith (++) (newTransitions x) ((map . map) toTransitionX oldTransitions)) (map snd x)
    where oldTransitions = map fst x
          newTransitions = (((map . map) ((flip TransitionX) (Tristate Nothing))) . missingSignals . transitionList)
          transitionList =  (map fullList)

readyForEncoding :: Ord a => Causality a -> [([TransitionX a], Transition a)]
readyForEncoding =  addMissingSignals . removeDupes

encode :: Ord a => [TransitionX a] -> [Tristate]
encode  = (map mnewValue) . sortTransitions
    where sortTransitions = sortBy (comparing msignal)

makeDestEncs :: Ord a => Causality a -> [[Tristate]]
makeDestEncs = (map encode) . (map fullListm) . readyForEncoding

flipTransition :: Transition a -> Transition a
flipTransition x = Transition (signal x) (not (newValue x))

flipTransitions :: ([Transition a], Transition a) -> ([Transition a], Transition a)
flipTransitions x = (fst x, flipTransition (snd x))

makeSrcEncs :: Ord a => Causality a -> [[Tristate]]
makeSrcEncs = (map encode) . (map fullListm) . readyForEncoding . (map flipTransitions)

activeTransitions :: Ord a => Causality a -> [Transition a]
activeTransitions = (map (snd)) .  readyForEncoding

createArc :: [Tristate] -> [Tristate] -> Transition a -> FsmArcX a
createArc senc tenc transx = FsmArcX senc transx tenc

createArcs :: Ord a => Causality a -> [FsmArcX a]
createArcs xs = zipWith3 (createArc) (makeSrcEncs xs) (makeDestEncs xs) (activeTransitions xs)
   
replaceAtIndex item ls n = a ++ (item:b)
    where (a, (_:b)) = splitAt n ls

expandSourceX :: FsmArcX a -> [FsmArcX a]
expandSourceX xs = case elemIndex (Tristate Nothing) (srcEncx xs) of
               Nothing -> [xs]
               Just n  -> [makeArc (replaceAtIndex (Tristate (Just True)) (srcEncx xs) n),
                           makeArc (replaceAtIndex (Tristate (Just False)) (srcEncx xs) n)]
                               where makeArc s = FsmArcX s (transx xs) (destEncx xs)

expandSourceXs :: [FsmArcX a] -> [FsmArcX a]
expandSourceXs = concatMap expandSourceX

expandTargetX :: FsmArcX a -> [FsmArcX a]
expandTargetX xs = case elemIndex (Tristate Nothing) (destEncx xs) of
               Nothing -> [xs]
               Just n  -> [makeArc (replaceAtIndex (Tristate (Just True)) (destEncx xs) n),
                           makeArc (replaceAtIndex (Tristate (Just False)) (destEncx xs) n)]
                               where makeArc s = FsmArcX (srcEncx xs) (transx xs) s

expandTargetXs :: [FsmArcX a] -> [FsmArcX a]
expandTargetXs = concatMap expandTargetX

expandAllXs :: [FsmArcX a] -> [FsmArcX a]
expandAllXs = expandTargetXs . expandSourceXs

createAllArcs :: Ord a => Causality a -> [FsmArcX a]
createAllArcs = expandAllXs . createArcs

-- http://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

encToInt :: [Tristate] -> Int
encToInt enc = fromMaybe 0 ((readBin . concatMap show . reverse) enc)

fsmarcxToFsmarc :: FsmArcX a -> FsmArc a
fsmarcxToFsmarc arc = FsmArc newSourceEnc (transx arc) newDestEnc
    where newSourceEnc = (encToInt . srcEncx) arc
          newDestEnc = (encToInt . destEncx) arc

stateArcs :: Ord a => Causality a -> [FsmArc a]
stateArcs x = map fsmarcxToFsmarc (createAllArcs x)

tmpl :: String
tmpl = unlines [".state graph", "%s.marking{%s}", ".end"]
