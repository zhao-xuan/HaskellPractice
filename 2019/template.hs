module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
    = putStrLn . showF
    where
    showF (Var v)
        = v
    showF (Not f)
        = '!' : showF f
    showF (And f f')
        = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
        = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp item list
  = fromJust (lookup item list)

-- 3 marks
vars :: Formula -> [Id]
vars = nub . sort . vars'
    where vars' (Var v) = [v]
          vars' (Not f) = vars' f
          vars' (And f f') = (vars' f) ++ (vars' f')
          vars' (Or f f') = (vars' f) ++ (vars' f')

-- 1 mark
idMap :: Formula -> IdMap
idMap formula
  = idmap' varlist 1
    where varlist = vars formula
          idmap' [] _ = []
          idmap' (v1 : rest) index = (v1, index) : (idmap' rest (index + 1))
--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
    = And (distribute a b) (distribute a c)
distribute (And a b) c
    = And (distribute a c) (distribute b c)
distribute a b
    = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (And f f')) = (Or (toNNF (Not f)) (toNNF (Not f'))) 
toNNF (Not (Or f f'))  = (And (toNNF (Not f)) (toNNF (Not f')))
toNNF (Not (Not f))    = toNNF f
toNNF (Or f f')        = Or (toNNF f) (toNNF f')
toNNF (And f f')       = And (toNNF f) (toNNF f')
toNNF (Not f)          = Not (toNNF f)
toNNF (Var v)          = Var v

-- 3 marks
toCNF :: Formula -> CNF
toCNF formula
  = cnf' nnf
     where nnf = toNNF formula
           cnf' (And f f') = And (cnf' f) (cnf' f')
           cnf' (Or f f')  = distribute f f'
           cnf' (Not f)    = Not (cnf' f)
           cnf' (Var v)    = Var v
-- 4 marks
flatten :: CNF -> CNFRep
flatten cnf
  = flatten' cnf
      where varList = idMap cnf
            flatten' :: CNF -> CNFRep
            flatten' (And f f')       = (flatten' f) ++ (flatten' f')
            flatten' (Or f f')        = [concat ((flatten' f) ++ (flatten' f'))]
            flatten' (Not (Var v))    = [[-(lookUp v varList)]]
            flatten' (Var v)          = [[lookUp v varList]]

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits cnf
  = propUnits' cnf cnf
    where deleteUnit :: CNFRep -> Int -> CNFRep
          deleteUnit [] _ = []
          deleteUnit (this : rest) i
            | elem i this = (dropWhile (\x -> (x == i) || (x == -i)) this) : deleteUnit rest i
            | otherwise   = this : deleteUnit rest i
          propUnits' :: CNFRep -> CNFRep -> (CNFRep, [Int])
          propUnits' [] savedCnf = (savedCnf, [])
          propUnits' c@(clause : rest) savedCnf
            | length clause == 1 = (fst proped, clause!!0 : snd proped)
            | otherwise          = (fst (propUnits' rest savedCnf), snd (propUnits' rest savedCnf))
                where proped = propUnits' rest (deleteUnit savedCnf (clause!!0))
          

-- 4 marks
dp :: CNFRep -> [[Int]]
dp 
    = undefined

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
    = undefined