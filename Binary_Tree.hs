-- File/Module name: (.hs)
-- Author: Spinda
-- Date: < 14 May 2014>
-- Description: Haskel lab 10 tree

module Binary_Tree (
 Binary_Tree (Null, Node, element, left_tree, right_tree), 
 size_of_tree, -- :: Integral b => Binary_Tree a -> b
 depth_of_tree, -- :: Integral b => Binary_Tree a -> b
 flatten_tree, -- :: Binary_Tree a -> [a]
 leaves_of_tree, -- :: Binary_Tree a -> [a]
 map_function_over_binary_tree -- :: (a -> b) -> Binary_Tree a -> Binary_Tree b
) where

data Binary_Tree a = Null | Node {element :: a, left_tree, right_tree :: Binary_Tree a}
    deriving (Show, Eq)
size_of_tree :: (Integral b) => Binary_Tree a -> b
size_of_tree tree = case tree of 
    Null -> 0
    Node _ treel treer -> 1 + size_of_tree treel + size_of_tree treer

--(Node 13.2 (Node 7.9 (Node 2.0 Null Null) (Node (-18.0) Null Null)) (Node 3.2 Null Null))

depth_of_tree :: (Integral b) => Binary_Tree a -> b
depth_of_tree tree = case tree of
    Null -> 0
    Node _ treel treer -> 1 + max (depth_of_tree treel) (depth_of_tree treer)

flatten_tree :: Binary_Tree a -> [a]
flatten_tree tree = case tree of
    Null -> []
    Node element treel treer -> [element] ++ flatten_tree treel ++ flatten_tree treer

leaves_of_tree :: Binary_Tree a -> [a]
leaves_of_tree tree = case tree of
    Null -> []
    Node element Null Null -> [element]
    Node _ treel treer -> (leaves_of_tree treel) ++ (leaves_of_tree treer)

map_function_over_binary_tree :: (a -> b) -> Binary_Tree a -> Binary_Tree b
map_function_over_binary_tree f tree = case tree of
    Null -> Null
    Node element treel treer -> Node (f element) (map_function_over_binary_tree f treel) (map_function_over_binary_tree f treer)