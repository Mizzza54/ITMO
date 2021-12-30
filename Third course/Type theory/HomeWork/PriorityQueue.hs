{-# LANGUAGE RankNTypes #-}

data PriorityQueue = PQ (forall b . (forall a . ( a
                                                , Integer -> a -> a
                                                , a -> (a, Integer)
                                                ) -> b) -> b)

test :: PriorityQueue -> Bool
test (PQ r) = r x where
  x (empty, insert, exMin) =
    let
      insertList l = (foldr insert empty l)
    in let 
      h1 = insertList ([1,3..9] ++ [10,8..2])
      h2 = insertList ([9,7..1] ++ [2,4..10])
      h3 = insertList ([2,4..10] ++ [9,7..1])
      h4 = insertList ([10,8..2] ++ [1,3..9])
      h5 = insertList ([1..5] ++ [1..5])
      h6 = insertList [1,5,2,3,7,6,10,9,8,4]
      h7 = insertList [1]
      res h = (snd $ unzip $ foldr (\i l@((h, v):t) -> (exMin h):l) [(h, 0)] [1..10])
      isSorted [] = True
      isSorted (n:[]) = True
      isSorted (a:t@(b:_)) = a >= b && isSorted t
    in
      foldr (&&) True $ map (isSorted . res) [h1, h2, h3, h4, h5, h6]

data BT a = Node Integer a [BT a]

type BH a = [BT a]

btSingleton :: Ord a => a -> BT a
btSingleton x = Node 0 x []

btMin :: Ord a => BT a -> a
btMin (Node _ v _) = v

btRank :: Ord a => BT a -> Integer
btRank (Node r _ _) = r

bhMin :: Ord a => BH a -> a
bhMin h = minimum $ (map btMin) h

btMerge :: Ord a => BT a -> BT a -> BT a
btMerge t1@(Node r v1 c1) t2@(Node _ v2 c2) =
  if v1 <= v2 then Node (r + 1) v1 (t2:c1)
    else Node (r + 1) v2 (t1:c2)

btMerge3 :: Ord a => a -> BT a -> BT a -> BT a
btMerge3 x t1@(Node r v1 c1) t2@(Node _ v2 c2) =
  if x <= v1 && x <= v2 then Node (r + 1) x [t1, t2]
    else if v1 <= x && v1 <= v2 then Node (r + 1) v1 (t2:(btSingleton x):c1)
    else Node (r + 1) v2 (t1:(btSingleton x):c2)

bhtMerge :: Ord a => Maybe (BT a) -> BH a -> BH a -> BH a
bhtMerge Nothing [] [] = []
bhtMerge (Just t) [] [] = [t]
bhtMerge Nothing h [] = h
bhtMerge Nothing [] h = h
bhtMerge Nothing h1@(n1:t1) h2@(n2:t2) = 
  if (btRank n1) < (btRank n2) then n1:(bhtMerge Nothing t1 h2)
    else if (btRank n1) == (btRank n2) then bhtMerge (Just $ btMerge n1 n2) t1 t2
    else n2:(bhtMerge Nothing h1 t2)
bhtMerge h1@(Just n1) h2@(n2:t2) [] =    
  if (btRank n1) < (btRank n2) then n1:(bhtMerge Nothing h2 [])
    else if (btRank n1) == (btRank n2) then bhtMerge (Just $ btMerge n1 n2) t2 []
    else n2:(bhtMerge h1 t2 [])
bhtMerge h1 [] h2 = bhtMerge h1 h2 []
bhtMerge h1@(Just n1) h2@(n2:t2) h3@(n3:t3) =
  if (btRank n1) < (btRank n2) && (btRank n1) < (btRank n3) then n1:(bhtMerge Nothing h2 h3)
    else if (btRank n2) == (btRank n3) then n1:(bhtMerge (Just $ btMerge n2 n3) t2 t3)
    else if (btRank n1) == (btRank n2) then bhtMerge (Just $ btMerge n1 n2) t2 h3
    else bhtMerge (Just $ btMerge n1 n3) h2 t3  

bhMerge :: Ord a => BH a -> BH a -> BH a
bhMerge = bhtMerge Nothing

bhInsert :: Ord a => a -> BH a -> BH a
bhInsert x [] = [btSingleton x]
bhInsert x h@(n:[]) = 
  if (btRank n) == 0 then [btMerge (btSingleton x) n]
    else (btSingleton x):h 
bhInsert x h@(n1:n2:t) = 
  if (btRank n1) == (btRank n2) then (btMerge3 x n1 n2):t
    else (btSingleton x):h


btSplit :: Ord a => [BT a] -> ([BT a], [a])
btSplit [] = ([], [])
btSplit (n@(Node 0 v _):t) = (big, v:small) where (big, small) = btSplit t
btSplit (n:t) = (n:big, small) where (big, small) = btSplit t

btExMin :: Ord a => BT a -> (a, [BT a], [a])
btExMin (Node _ v c) = (v, big, small) where (big, small) = btSplit c 

bhMinIdxStart :: Ord a => Int -> BH a -> (Int, BT a) 
bhMinIdxStart i (n:[]) = (i, n)
bhMinIdxStart i (n@(Node _ v _):t) = 
  if v <= (btMin n1) then (i, n) else (i1, n1)  
  where (i1, n1) = bhMinIdxStart (i + 1) t

bhMinIdx :: Ord a => BH a -> (Int, BT a)
bhMinIdx = bhMinIdxStart 0

bhExMin :: Ord a => BH a -> (BH a, a)  
bhExMin [] = ([], undefined)
bhExMin h =
  let
    (i, mt) = bhMinIdx h
  in let
    (v, big, small) = btExMin mt 
  in (foldr bhInsert (bhMerge ((take i h) ++ (drop (i + 1) h)) (reverse big)) small, v)

bhEmpty :: Ord a => BH a
bhEmpty = []

data BPQ a = Nil | BNode a (BH (BPQ a)) 

instance Eq a => Eq (BPQ a) where
  (BNode a1 _) == (BNode a2 _) = a1 == a2

instance Ord a => Ord (BPQ a) where
  (BNode a1 _) `compare` (BNode a2 _) = compare a1 a2

bpqSingleton :: Ord a => a -> BPQ a
bpqSingleton x = BNode x bhEmpty

bpqMerge :: Ord a => BPQ a -> BPQ a -> BPQ a
bpqMerge Nil q = q
bpqMerge q Nil = q
bpqMerge q1@(BNode a1 c1) q2@(BNode a2 c2) =
  if a1 < a2 then BNode a1 $ bhInsert q2 c1
    else BNode a2 $ bhInsert q1 c2

bpqInsert :: Ord a => a -> BPQ a -> BPQ a
bpqInsert x q = bpqMerge (bpqSingleton x) q

bpqExMin :: Ord a => BPQ a -> (BPQ a, a)
bpqExMin Nil = (Nil, undefined)
bpqExMin (BNode a c) = (BNode a1 (bhMerge q1 q2), a)
  where (q1, (BNode a1 q2)) = bhExMin c

bpqEmpty = Nil

bhPQ = PQ (\t -> t (bhEmpty, bhInsert, bhExMin))
bPQ = PQ (\t -> t (bpqEmpty, bpqInsert, bpqExMin))

queues = [bhPQ, bPQ]

main :: IO ()
main =
   print [test q | q <- queues]
