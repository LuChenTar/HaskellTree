module Binary_Search_Tree_Test_Generator () where

import Binary_Tree
import Binary_Search_Tree
import Test.QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (Binary_Tree a) where
    arbitrary = arbitrary >>= return . (foldr insert_element_to_tree Null)
