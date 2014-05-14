module Binary_Tree_Test_Generator () where

import Binary_Tree
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Binary_Tree a) where
    arbitrary = arbitrary >>= return . (foldr expand_tree_at_fringe Null)

expand_tree_at_fringe :: a -> Binary_Tree a -> Binary_Tree a
expand_tree_at_fringe e tree =
    case tree of
      Null               -> Node e Null Null
      Node e' Null right -> Node e' (expand_tree_at_fringe e Null) right
      Node e' left Null  -> Node e' left (expand_tree_at_fringe e Null)
      Node e' left right -> Node e' (expand_tree_at_fringe e left) right
