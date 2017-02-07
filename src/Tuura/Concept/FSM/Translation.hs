import Data.List
import Data.Ord (comparing)
import Control.Monad

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

--data Fsmstate a = Fsmstate
--    {
--        encoding :: Int,
--        arcs :: [(Transition a, Int)]
--    }
data Fsmstate a = Fsmstate
    {
        encoding :: Tristate,
        arcs :: [(Transition a, Tristate)]    -- (Transition, Target Encoding)
    }

instance Show a => Show (Fsmstate a) where
    show (Fsmstate enc arc) = show enc ++ " " ++ show arc

data FsmArcX a = FsmArcX
    {
        sourceEncoding :: [Tristate],
        trans :: Transition a,
        targetEncoding :: [Tristate]
    }

instance Show a => Show (FsmArcX a) where
    show (FsmArcX senc tran tenc) = "(" ++ show senc ++ " " ++ show tran ++ " " ++ show tenc ++ ")\n"

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


boolToChar :: Maybe Bool -> Char
boolToChar (Just True)  = '1'
boolToChar (Just False) = '0'
boolToChar Nothing      = 'x'

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

--createFsmstate :: [Char] -> [Char] -> Transition a -> Fsmstate a
--createFsmstate senc tenc trans = Fsmstate senc ([(trans, tenc)])

createArc :: [Tristate] -> [Tristate] -> Transition a -> FsmArcX a
createArc senc tenc trans = FsmArcX senc trans tenc

createArcs :: Ord a => [([Transition a], Transition a)] -> [FsmArcX a]
createArcs xs = zipWith3 (createArc) (constructSourceEncodings xs) (constructTargetEncodings xs) (activeTransitions xs)
   
numOfX :: [Tristate] -> Int
numOfX = (length . filter (== (Tristate Nothing)))

replaceAtIndex item ls n = a ++ (item:b) where (a, (_:b)) = splitAt n ls

expandSourceX :: FsmArcX a -> [FsmArcX a]
expandSourceX xs = case elemIndex (Tristate Nothing) (sourceEncoding xs) of
               Nothing -> [xs]
               Just n  -> [makeArc (replaceAtIndex (Tristate (Just True)) (sourceEncoding xs) n),
                           makeArc (replaceAtIndex (Tristate (Just False)) (sourceEncoding xs) n)]
                               where makeArc s = FsmArcX s (trans xs) (targetEncoding xs)

expandSourceXs :: [FsmArcX a] -> [FsmArcX a]
expandSourceXs = concatMap expandSourceX

expandTargetX :: FsmArcX a -> [FsmArcX a]
expandTargetX xs = case elemIndex (Tristate Nothing) (targetEncoding xs) of
               Nothing -> [xs]
               Just n  -> [makeArc (replaceAtIndex (Tristate (Just True)) (targetEncoding xs) n),
                           makeArc (replaceAtIndex (Tristate (Just False)) (targetEncoding xs) n)]
                               where makeArc s = FsmArcX (sourceEncoding xs) (trans xs) s

expandTargetXs :: [FsmArcX a] -> [FsmArcX a]
expandTargetXs = concatMap expandTargetX

expandAllXs :: [FsmArcX a] -> [FsmArcX a]
expandAllXs = expandTargetXs . expandSourceXs


--createTargetStates :: Ord a => [([Transition a], Transition a)] -> [Fsmstate a]
--createTargetStates = (map fullListm) . addMissingSignals . removeDupes
