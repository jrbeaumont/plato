import Data.List
import Data.Ord (comparing)
import Control.Monad
import Data.Char  (digitToInt)
import Data.Maybe (fromMaybe, listToMaybe)
import Numeric    (readInt)
import Text.Printf

type Causality a = [([Transition a], Transition a)]
type CausalityX a = [([TransitionX a], Transition a)]

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

instance Show Tristate where
    show (Tristate (Just True) ) = "1"
    show (Tristate (Just False)) = "0"
    show (Tristate Nothing     ) = "x"

triTrue = Tristate (Just True)
triFalse = Tristate (Just False)
triX = Tristate Nothing

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
toTransitionX = liftM2 TransitionX signal (Tristate . Just . newValue)

onlySignals :: Eq a => [[Transition a]] -> [[a]]
onlySignals = map (map signal)

getAllSignals :: Ord a => [[Transition a]] -> [a]
getAllSignals = sort . foldl union [] . onlySignals

addMissingSignals :: Ord a => Causality a -> CausalityX a
addMissingSignals x = zip (zipWith (++) newTransitions oldTransitions) (map snd noDupes)
    where noDupes = removeDupes x
          oldTransitions = map (map toTransitionX . fst) noDupes
          newTransitions = ((map . map) (flip TransitionX triX) . missingSignals . transitionList) noDupes
          transitionList =  map fullList
          missingSignals y = map (getAllSignals y \\) (onlySignals y)

encode :: Ord a => [TransitionX a] -> [Tristate]
encode  = map mnewValue . sortTransitions
    where sortTransitions = sortBy (comparing msignal)

createArcs :: Ord a => Causality a -> [FsmArcX a]
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

expandSrcX :: FsmArcX a -> [FsmArcX a]
expandSrcX xs = case elemIndex triX (srcEncx xs) of
               Nothing -> [xs]
               Just n  -> [makeArc (replaceAtIndex triTrue (srcEncx xs) n),
                           makeArc (replaceAtIndex triFalse (srcEncx xs) n)]
                               where makeArc s = FsmArcX s (transx xs) (destEncx xs)

expandSrcXs :: [FsmArcX a] -> [FsmArcX a]
expandSrcXs = concatMap expandSrcX

expandDestX :: FsmArcX a -> [FsmArcX a]
expandDestX xs = case elemIndex triX (destEncx xs) of
               Nothing -> [xs]
               Just n  -> [makeArc (replaceAtIndex triTrue (destEncx xs) n),
                           makeArc (replaceAtIndex triFalse (destEncx xs) n)]
                               where makeArc = FsmArcX (srcEncx xs) (transx xs)

expandDestXs :: [FsmArcX a] -> [FsmArcX a]
expandDestXs = concatMap expandDestX

expandAllXs :: [FsmArcX a] -> [FsmArcX a]
expandAllXs = expandDestXs . expandSrcXs

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
