import Data.List 

type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node a _ _) = a

rank :: BinTree a -> Int
rank (Node _ i _) = i

children :: BinTree a -> [BinTree a]
children (Node _ _ c) = c

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees bt1@(Node a1 i1 c1) bt2@(Node a2 i2 c2)
  = if value bt1 >= value bt2 then (Node a2 (i2 + 1) (bt1 : c2))
                              else (Node a1 (i1 + 1) (bt2 : c1))

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin 
  = minimum . (map value)

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps bh1 bh2
  | null bh1 = bh2
  | null bh2 = bh1
  | rank t == rank t' = mergeHeaps [(combineTrees t t')] (mergeHeaps (tail bh1) (tail bh2))
  | otherwise = if rank t < rank t' then t : (mergeHeaps (tail bh1) bh2)
                                    else t' : (mergeHeaps bh1 (tail bh2))
      where t  = head bh1
            t' = head bh2

insert' :: Ord a => a -> BinHeap a -> BinHeap a
insert' val bh = mergeHeaps [Node val 0 []] bh

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin bh = mergeHeaps c bh'
  where c   = (reverse . children) min
        bh' = delete min bh
        min = (filter (\(Node x i l) -> x == extractMin bh) bh) !! 0

remove :: Eq a => a -> BinHeap a -> BinHeap a
remove
  = undefined

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin
  = undefined

binSort :: Ord a => [a] -> [a]
binSort [] = []
binSort list = destruct (construct list)
    where construct [] = []
          construct (s : ss) = insert' s (construct ss)
          destruct [] = []
          destruct bh = (extractMin bh) : (destruct (deleteMin bh))

--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary bh = replicate (maximum) 0
    where ranks = map rank bh
          index = []
          

binarySum :: [Int] -> [Int] -> [Int]
binarySum
  = undefined

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]


