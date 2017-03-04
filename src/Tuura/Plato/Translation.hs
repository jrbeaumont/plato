module Tuura.Plato.Translation where

import Data.Char
import Data.List.Extra
import Data.Maybe

import Tuura.Concept.Circuit.Basic
import Tuura.Concept.Circuit.Derived

data ValidationResult a = Valid | Invalid [ValidationError a] deriving Eq

data ValidationError a = UnusedSignal a
                       | InconsistentInitialState a
                       | UndefinedInitialState a
                       | InvariantViolated a
                       deriving Eq

data Signal = Signal Int deriving Eq

instance Show Signal where
    show (Signal i)
        | i < 26    = [chr (ord 'A' + i)]
        | otherwise = 'S' : show i

instance Ord Signal
    where
        compare (Signal x) (Signal y) = compare x y

-- TODO: Tidy up function, it looks ugly.
addErrors :: (Eq a, Show a) => [ValidationError a] -> String
addErrors errs =
        if (unused errs) /= []
          then "The following signals are not declared as input, output or internal: \n"
             ++ unlines (map show (unused errs)) ++ "\n"
          else "" ++
        if (incons errs) /= []
          then "The following signals have inconsistent inital states: \n"
             ++ unlines (map show (incons errs)) ++ "\n"
          else "" ++
        if (undefd errs) /= []
          then "The following signals have undefined initial states: \n"
             ++ unlines (map show (undefd errs)) ++ "\n"
          else "" ++
        if (invVio errs /= [])
          then "These signals violate the invariant: \n"
             ++ unlines (map show (invVio errs)) ++ "\n"
          else ""
    where
        unused es = [ a | UnusedSignal a <- es ]
        incons es = [ a | InconsistentInitialState a <- es ]
        undefd es = [ a | UndefinedInitialState a <- es ]
        invVio es = [ a | InvariantViolated a <- es]

validate :: Ord a => [a] -> CircuitConcept a -> [([Transition a], Transition a)] -> ValidationResult a
validate signs circuit sortedCauses
    | unused ++ inconsistent ++ undef ++ reachInv == [] = Valid
    | otherwise = Invalid ((map UnusedSignal unused) ++ (map InconsistentInitialState inconsistent)
                  ++ (map UndefinedInitialState undef) ++ (map InvariantViolated reachInv))
  where
    unused       = filter ((==Unused) . interface circuit) signs
    inconsistent = filter ((==Inconsistent) . initial circuit) signs
    undef        = filter ((==Undefined) . initial circuit) signs
    reachInv     = concatMap (\(NeverAll es) -> map signal es) reachTest
    reachTest    = map fromJust (filter (/=Nothing) (map (testReachables sortedCauses) (invariant circuit)))

testReachables :: Eq a => [([Transition a], Transition a)] -> Invariant (Transition a) -> Maybe (Invariant (Transition a))
testReachables sortedCauses (NeverAll es) = if any (\e -> isInvReachable e (remain e) (invertedEs (remain e)) sortedCauses []) es
    then Just (NeverAll es)
    else Nothing
  where
    invertedEs xs = map (\e -> Transition { signal = (signal e), newValue = not (newValue e)}) xs
    remain x = es \\ [x]

isInvReachable :: Eq a => Transition a -> [Transition a] -> [Transition a] -> [([Transition a], Transition a)] -> [Transition a] -> Bool
isInvReachable effect xs invertedXs causes visited
    | invertedXs == []                  = False
    | relCauses  == []                  = True
    | any (`elem` relCauses) xs         = True
    | any (`elem` relCauses) invertedXs = all (\c -> isInvReachable c xs (filter (`notElem` relCauses) invertedXs) causes newVisited) relCauses
    | otherwise                         = any (\c -> isInvReachable c xs invertedXs causes newVisited) relCauses
  where
    relCauses = filter (`notElem` visited) (concatMap fst (filter (\c -> snd c == effect) causes))
    newVisited = visited ++ [effect] ++ relCauses

checkInitialStates :: Ord a => [a] -> Invariant (Transition a) -> (a -> InitialValue) -> [a]
checkInitialStates signs (NeverAll es) initials = nubOrd (if (all (`elem` initialStates) es) then (invariantError es) else [])
  where
    initialStates = map (\s -> Transition { signal = s , newValue = getDefined $ initials s }) signs

invariantError :: [Transition a] -> [a]
invariantError = map signal

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct l = sequence l
