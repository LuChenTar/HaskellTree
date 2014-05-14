import Binary_Tree
import Data.List(sort)

type BS_Tree a = Binary_Tree a

is_valid_binary_search_tree :: Ord a => BS_Tree a -> Bool
is_valid_binary_search_tree tree = case tree of
    Null -> True
    Node n treel treer -> case (treel, treer) of
        (Null, Null) -> True
        (Null, _) -> (n < minimum (flatten_tree treer)) && (is_valid_binary_search_tree treer)
        (_, Null) -> (n > maximum (flatten_tree treer)) && (is_valid_binary_search_tree treel)
        (_, _) -> (maximum (flatten_tree treel) < n || treel == Null) 
                && (minimum (flatten_tree treer) > n || treer == Null) 
                && (is_valid_binary_search_tree treel && is_valid_binary_search_tree treer)

minimum_element :: Ord a => BS_Tree a -> a
minimum_element tree = minimum (flatten_tree tree)

maximum_element :: Ord a => BS_Tree a -> a
maximum_element tree = maximum (flatten_tree tree)

contains_element :: Ord a => a -> BS_Tree a -> Bool
contains_element element tree = case (flatten_tree tree) of
    [] -> False
    t:ts -> case t == element of
        True -> True
        False -> in_tree ts element
            where
                in_tree :: Ord a => [a] -> a -> Bool
                in_tree (l:ls) e = case ls of
                    [] -> False
                    _  -> case l == e of
                            True -> True
                            False -> in_tree ls e

flatten_in_order :: Ord a => BS_Tree a -> [a] 
flatten_in_order tree = sort (flatten_tree tree)

insert_element_to_tree :: Ord a => a -> BS_Tree a -> BS_Tree a
insert_element_to_tree element tree = case contains_element element tree of
    False -> case tree of
        Null -> Node element Null Null
        Node e treel treer -> case compare e element of
            GT -> Node e (insert_into element treel) treer
            LT -> Node e treel (insert_into element treer)
            EQ -> Node e treel treer
    True -> tree

insert_into :: Ord a => a -> BS_Tree a -> BS_Tree a
insert_into element tree = case tree of
    Null -> Node element Null Null
    Node e treel treer -> case compare e element of
        GT -> Node e (insert_into element treel) treer
        LT -> Node e treel (insert_into element treer)
        EQ -> Node e treel treer


test_case_4 :: BS_Tree Char
test_case_4 = Node 'g' (Node 'c' (Node 'a' Null (Node 'b' Null Null)) (Node 'e' (Node 'f' Null Null) (Node 'g' Null Null))) (Node 'i' (Node 'h' Null Null) (Node 'j' Null Null))
test_case_5 :: BS_Tree Integer
test_case_5 = Node 92 (Node 11 Null Null) (Node 101 Null (Node 102 Null Null))





