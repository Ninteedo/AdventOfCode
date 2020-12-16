-- Indexed data represented as a tree

module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT,
                    prop_set_get, prop_toList_fromList,
                    rebalance
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + max (size left) (size right)
    where
        max :: Ord a => a -> a -> a
        max x y | x > y = x
                | otherwise = y

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k a left right) = toList left ++ [(k,a)] ++ toList right

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = go
    where go Leaf = Node key value Leaf Leaf --replaces the leaf with a node with 2 leaves and the key and value
          go (Node k v left right) | key == k = Node k value left right --changes the value of the node with matching key
                                   | key < k  = Node k v (go left) right
                                   | key > k  = Node k v left (go right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key = go
    where
        go Leaf = Nothing
        go (Node k v left right) | key == k = Just v
                                 | key < k  = go left
                                 | key > k  = go right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList = foldr (uncurry set) Leaf

prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT key = go
    where
        go Leaf = Leaf
        go (Node k v left right) | k < key = Node k v (go left) (go right)
                                 | otherwise = go left

prop_filterLT :: Int -> [(Int, Int)] -> Bool
prop_filterLT key keysAndVs = filterLT key (fromList keysAndVs) `equal` fromList [ (k,v) | (k,v) <- keysAndVs, k < key ]

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT key = go
    where
        go Leaf = Leaf
        go (Node k v left right) | k > key = Node k v (go left) (go right)
                                 | otherwise = go right

prop_filterGT :: Int -> [(Int, Int)] -> Bool
prop_filterGT key keysAndVs = filterGT key (fromList keysAndVs) `equal` fromList [ (k,v) | (k,v) <- keysAndVs, k > key ]

equal :: Ord k => Eq a => Keymap k a -> Keymap k a -> Bool
equal km1 km2 = all (`elem` toList km2) (toList km1) && all (`elem` toList km1) (toList km2)

-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge km1 km2 = foldr (uncurry set) km1 (toList km2)

prop_merge :: [(Int, Int)] -> [(Int, Int)] -> Bool
prop_merge kvs1 kvs2 = merge (fromList kvs1) (fromList kvs2) `equal` fromList (kvs2 ++ kvs1)

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del k km = merge (filterLT k km) (filterGT k km)


-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select f = go
    where
        go Leaf = Leaf
        go (Node k v left right) | f v = Node k v (go left) (go right)
                                 | otherwise = del k (Node k v (go left) (go right))

prop_select :: Ord k => Keymap k Int -> Bool
prop_select km = all (\func -> select func km `equal`
                        fromList [ (k,v) | (k,v) <- toList km, func v ])
                     [f,g,h,i]
    where
        f = odd
        g = even
        h = (< 10)
        i = (> 10)

rebalance :: Ord k => Keymap k a -> Keymap k a
rebalance (Node k v (Node k' v' l' r') r) | depth l' >= depth r' && depth l' > depth r = Node k' v' l' (Node k v r' r)
rebalance (Node k v l (Node k' v' l' r')) | depth r' >= depth l' && depth r' > depth l = Node k' v' (Node k v l' l) r'
rebalance (Node k v (Node k' v' a (Node k'' v'' b c)) d) | depth (Node k'' v'' b c) > depth d = Node k'' v'' (Node k' v' a b) (Node k v c d)
rebalance (Node k v a (Node k' v' (Node k'' v'' b c) d)) | depth (Node k'' v'' b c) > depth a = Node k'' v'' (Node k v a b) (Node k' v' c d)
rebalance a = a

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
