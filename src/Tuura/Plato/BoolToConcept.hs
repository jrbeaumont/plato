module Tuura.Plato.BoolToConcept where

import Data.List
import Data.Foldable

import Tuura.Boolean

fromFunctions :: String -> String -> (Bool, String)
fromFunctions setString resetString = do
    let setResult = parseExpr setString
    let resetResult = parseExpr resetString
    if (left setResult /= "")
      then (False, "parse error at " ++ left setResult)
    else if (left resetResult /= "")
      then (False, "parse error at " ++ left resetResult)
      else do
      let set = right setResult
      let reset = right resetResult
      let setVars = nub $ toList set
      let resetVars = nub $ toList reset
      let cnfSet = convertToCNF set
      let cnfReset = convertToCNF reset
      let allVars = nub $ setVars ++ resetVars
      (True, createConceptSpec allVars cnfSet cnfReset)
  where
    right (Right x) = x
    right (Left _) = right (parseExpr "")
    left  (Left x) = show x
    left _ = ""

createConceptSpec :: [String] -> CNF String -> CNF String -> String
createConceptSpec vars set reset = modName ++ imp
                                ++ circuit ++ topConcept ++ wh
                                ++ outRise ++ outFall
                                ++ inInter ++ outInter
                                ++ initState
    where
      modName    = "\nmodule Concept where \n\n"
      imp        = "import Tuura.Concept.STG \n\n"
      circuit    = "circuit " ++ unwords vars ++ " out = "
      topConcept = "outRise <> outFall <> interface <> initialState\n"
      wh         = "  where\n"
      rConcept   = intersperse "<>" $ map (genConcepts True) set
      outRise    = "    outRise = " ++ unwords rConcept
      fConcept   = intersperse "<>" $ map (genConcepts False) reset
      outFall    = "\n    outFall = " ++ unwords fConcept
      inputVars  = intersperse "," vars
      inInter    = "\n    interface = inputs [" ++ unwords inputVars ++ "]"
      outInter   = " <> outputs [out]"
      initState  = "\n    initialState = "
                ++ "initialise0 [" ++ unwords inputVars ++ " , out]"
