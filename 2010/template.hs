import Data.List

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix pre str = elem pre (inits str)

removePrefix :: String -> String -> String
removePrefix pre str
--Pre: s is a prefix of s'
  = drop (length pre) str

suffixes :: [a] -> [[a]]
suffixes str = take (length str) (tails str)

isSubstring :: String -> String -> Bool
isSubstring sub str = or (map (isPrefix sub) (suffixes str))

findSubstrings :: String -> String -> [Int]
findSubstrings sub str = [i | i <- [0..((length str) - 1)], (map (isPrefix sub) (suffixes str)) !! i]

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf i)                = [i]
getIndices (Node ((_, st) : rest)) = (getIndices st) ++ concat (map getIndices (map snd rest))

partition' :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition' s1 s2 = (common, r1, r2)
    where l1 = [0..(length s1) - 1]
          l2 = [0..(length s2) - 1]
          common = take index s1
          index = length (takeWhile (==True) (map (\x -> fst x == snd x) (zip s1 s2)))
          r1 = drop (length common) s1
          r2 = drop (length common) s2

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' sub (Node ((str, st) : rest))
  | rest == [] = []
  | r1 == ""   = getIndices st
  | c /= ""    = findSubstrings' r1 st
  | otherwise  = findSubstrings' sub (Node rest)
    where (c, r1, r2) = partition' sub str
findSubstrings' sub (Leaf i) = if sub == "" then [] else [i]


------------------------------------------------------

insert' :: (String, Int) -> SuffixTree -> SuffixTree
insert' ins@(s, n) (Node []) = Node [(s, Leaf n)]
insert' ins@(s, n) (Node ((a, t) : rest))
  | p == ""   = (Node ((a, t) : nextNode))
  | p == a    = (Node ((a, (insert' (r1, n) t)) : rest))
  | otherwise = (Node ((p, (Node [(r2, t), (r1, Leaf n)])) : rest))
      where (p, r1, r2)   = partition' s a --r1 = s - p, r2 = a - p
            Node nextNode = insert' ins (Node rest)

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert') (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring t
  = last (sortOn length (lrs t))

lrs :: SuffixTree -> [String]
lrs (Node [])                    = [[]]
lrs (Node ((_, Leaf i) : rest))  = lrs (Node rest)
lrs (Node ((s, Node st) : rest)) = (map (s ++) (lrs (Node st))) ++ lrs (Node rest)

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]
                       
t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0),
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7),
                      ("", Leaf 10)]),
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]