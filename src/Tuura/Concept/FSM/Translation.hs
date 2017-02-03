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
	mnewValue :: Maybe Bool -- Transition x True corresponds to x+
    }
    deriving Eq

instance Show a => Show (MaybeTransition a) where
    show (MaybeTransition s (Just True) ) = show s ++ "+"
    show (MaybeTransition s (Just False)) = show s ++ "-"
    show (MaybeTransition s Nothing     ) = show s ++ "x"

rise :: a -> Transition a
rise a = Transition a True

fall :: a -> Transition a
fall a = Transition a False

data Fsmstate a = Fsmstate
    {
	encoding :: Int,
	arcs :: [(Transition a, Int)]
    }

instance Show a => Show (Fsmstate a) where
    show (Fsmstate enc arc) = show enc ++ " " ++ show arc

list a b c d = [([rise a, fall c, rise d], rise c),
		([rise b, fall c, rise d], rise c),
		([rise b, rise c, fall d], rise a)
	       ]

trs a b c = [rise a, fall b, rise c]


boolToChar :: Maybe Bool -> Char
boolToChar (Just True) = '1'
boolToChar (Just False) = '0'
boolToChar Nothing = 'x'

sortTransitions :: Ord a => [MaybeTransition a] -> [MaybeTransition a]
sortTransitions = sortBy (comparing msignal)

intsFromTransitions :: [MaybeTransition a] -> [Char]
intsFromTransitions = map (boolToChar . mnewValue)

fullList :: ([Transition a], Transition a) -> [Transition a]
fullList (l,t) = t:l

removeDupes :: Eq a => [([Transition a], Transition a)] -> [([Transition a], Transition a)]
--(filter ((/= ((signal . snd) x)) . signal) (fst x), snd x)
removeDupes = map (ap ((,) . ap (filter . (. signal) . (/=) . signal . snd) fst) snd)

toMaybeTransition :: [[Transition a]] -> [[MaybeTransition a]]
toMaybeTransition = (map . map) (liftM2 MaybeTransition signal (Just . newValue))

onlySignals :: Eq a => [[Transition a]] -> [[a]]
onlySignals = map (map signal)

getAllSignals :: Ord a => [[Transition a]] -> [a]
getAllSignals = (sort . foldl union ([]) . onlySignals)

missingSignals :: Ord a => [[Transition a]] -> [[a]]
missingSignals x = map ((\\) (getAllSignals x)) (onlySignals x)

addMissingSignals :: Ord a => [[Transition a]] -> [[MaybeTransition a]]
addMissingSignals x = zipWith (++) (newTransitions x) (toMaybeTransition x)

newTransitions :: Ord a => [[Transition a]] -> [[MaybeTransition a]]
newTransitions = (((map . map) ((flip MaybeTransition) Nothing)) . missingSignals)

transitionList :: Eq a => [([Transition a], Transition a)] -> [[Transition a]]
transitionList =  (map fullList) . removeDupes

constructEncodings :: Ord a => [([Transition a], Transition a)] -> [[Char]]
constructEncodings = (map encode) . addMissingSignals . transitionList
    where encode  = intsFromTransitions . sortTransitions
