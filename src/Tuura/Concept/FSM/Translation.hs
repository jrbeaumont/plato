import Data.List
import Data.Ord (comparing)
import Control.Monad
import Data.Char  (digitToInt)
import Data.Maybe (fromMaybe, listToMaybe)
import Numeric    (readInt)
import Text.Printf

data Transition a = Transition
    {
        signal   :: a,
        newValue :: Bool -- Transition x True corresponds to x+
    }
    deriving Eq

instance Show a => Show (Transition a) where
    show (Transition s True ) = show s ++ "+"
    show (Transition s False) = show s ++ "-"

data MaybeTransition a = MaybeTransition
    {
        msignal   :: a,
        mnewValue :: Tristate -- Transition x True corresponds to x+
    }
    deriving Eq

instance Show a => Show (MaybeTransition a) where
    show (MaybeTransition s (Tristate (Just True) )) = show s ++ "+"
    show (MaybeTransition s (Tristate (Just False))) = show s ++ "-"
    show (MaybeTransition s (Tristate Nothing     )) = show s ++ "x"

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
        sourceEncodingx :: [Tristate],
        transx :: Transition a,
        targetEncodingx :: [Tristate]
    }

instance Show a => Show (FsmArcX a) where
    show (FsmArcX senc tran tenc) = "(" ++ show senc ++ " " ++ show tran ++ " " ++ show tenc ++ ")\n"

data FsmArc a = FsmArc
    {
        sourceEncoding :: Int,
        trans :: Transition a,
        targetEncoding :: Int
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

genFSM :: (Show a, Ord a) => [([Transition a], Transition a)] -> String
genFSM causality = printf tmpl (unlines showArcs) ("s" ++ show (sourceEncoding (head arcs)))
    where arcs = intArcs causality
          showArcs = map show arcs

sortTransitions :: Ord a => [MaybeTransition a] -> [MaybeTransition a]
sortTransitions = sortBy (comparing msignal)

fullList :: ([a], a) -> [a]
fullList (l,t) = t:l

fullListm :: ([MaybeTransition a], Transition a) -> [MaybeTransition a]
fullListm (l,t) = ((liftM2 MaybeTransition signal (Tristate . Just . newValue)) t):l

-- Given [([a], b)], remove all b from a
removeDupes :: Eq a => [([Transition a], Transition a)] -> [([Transition a], Transition a)]
--(filter ((/= ((signal . snd) x)) . signal) (fst x), snd x)
removeDupes = map (ap ((,) . ap (filter . (. signal) . (/=) . signal . snd) fst) snd)

toMaybeTransition :: [[Transition a]] -> [[MaybeTransition a]]
toMaybeTransition = (map . map) (liftM2 MaybeTransition signal (Tristate . Just . newValue))

onlySignals :: Eq a => [[Transition a]] -> [[a]]
onlySignals = map (map signal)

getAllSignals :: Ord a => [[Transition a]] -> [a]
getAllSignals = (sort . foldl union ([]) . onlySignals)

missingSignals :: Ord a => [[Transition a]] -> [[a]]
missingSignals x = map ((\\) (getAllSignals x)) (onlySignals x)

addMissingSignals :: Ord a => [([Transition a], Transition a)] -> [([MaybeTransition a], Transition a)]
addMissingSignals x = zip (zipWith (++) (newTransitions x) (toMaybeTransition oldTransitions)) (map snd x)
    where oldTransitions = map fst x
          newTransitions = (((map . map) ((flip MaybeTransition) (Tristate Nothing))) . missingSignals . transitionList)
          transitionList =  (map fullList)

readyForEncoding :: Ord a => [([Transition a], Transition a)] -> [([MaybeTransition a], Transition a)]
readyForEncoding =  addMissingSignals . removeDupes

encode :: Ord a => [MaybeTransition a] -> [Tristate]
encode  = (map mnewValue) . sortTransitions

constructTargetEncodings :: Ord a => [([Transition a], Transition a)] -> [[Tristate]]
constructTargetEncodings = (map encode) . (map fullListm) . readyForEncoding

flipTransition :: Transition a -> Transition a
flipTransition x = Transition (signal x) (not (newValue x))

flipTransitions :: ([Transition a], Transition a) -> ([Transition a], Transition a)
flipTransitions x = (fst x, flipTransition (snd x))

constructSourceEncodings :: Ord a => [([Transition a], Transition a)] -> [[Tristate]]
constructSourceEncodings = (map encode) . (map fullListm) . readyForEncoding . (map flipTransitions)

activeTransitions :: Ord a => [([Transition a], Transition a)] -> [Transition a]
activeTransitions = (map (snd)) .  readyForEncoding

createArc :: [Tristate] -> [Tristate] -> Transition a -> FsmArcX a
createArc senc tenc transx = FsmArcX senc transx tenc

createArcs :: Ord a => [([Transition a], Transition a)] -> [FsmArcX a]
createArcs xs = zipWith3 (createArc) (constructSourceEncodings xs) (constructTargetEncodings xs) (activeTransitions xs)
   
replaceAtIndex item ls n = a ++ (item:b) where (a, (_:b)) = splitAt n ls

expandSourceX :: FsmArcX a -> [FsmArcX a]
expandSourceX xs = case elemIndex (Tristate Nothing) (sourceEncodingx xs) of
               Nothing -> [xs]
               Just n  -> [makeArc (replaceAtIndex (Tristate (Just True)) (sourceEncodingx xs) n),
                           makeArc (replaceAtIndex (Tristate (Just False)) (sourceEncodingx xs) n)]
                               where makeArc s = FsmArcX s (transx xs) (targetEncodingx xs)

expandSourceXs :: [FsmArcX a] -> [FsmArcX a]
expandSourceXs = concatMap expandSourceX

expandTargetX :: FsmArcX a -> [FsmArcX a]
expandTargetX xs = case elemIndex (Tristate Nothing) (targetEncodingx xs) of
               Nothing -> [xs]
               Just n  -> [makeArc (replaceAtIndex (Tristate (Just True)) (targetEncodingx xs) n),
                           makeArc (replaceAtIndex (Tristate (Just False)) (targetEncodingx xs) n)]
                               where makeArc s = FsmArcX (sourceEncodingx xs) (transx xs) s

expandTargetXs :: [FsmArcX a] -> [FsmArcX a]
expandTargetXs = concatMap expandTargetX

expandAllXs :: [FsmArcX a] -> [FsmArcX a]
expandAllXs = expandTargetXs . expandSourceXs

createAllArcs :: Ord a => [([Transition a], Transition a)] -> [FsmArcX a]
createAllArcs = expandAllXs . createArcs

-- http://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

encToInt :: [Tristate] -> Int
encToInt enc = fromMaybe 0 ((readBin . concatMap show . reverse) enc)

fsmarcxToFsmarc :: FsmArcX a -> FsmArc a
fsmarcxToFsmarc arc = FsmArc ((encToInt . sourceEncodingx) arc) (transx arc) ((encToInt . targetEncodingx) arc)

intArcs :: Ord a => [([Transition a], Transition a)] -> [FsmArc a]
intArcs x = map fsmarcxToFsmarc (createAllArcs x)

tmpl :: String
tmpl = unlines [".state graph", "%s.marking{%s}", ".end"]
