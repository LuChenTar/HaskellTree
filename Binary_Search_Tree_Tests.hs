import Binary_Tree
import Binary_Search_Tree
import Binary_Search_Tree_Test_Generator ()
import Data.List
import Test.QuickCheck

prop_unflattened_size :: BS_Tree String -> Bool
prop_unflattened_size tree =
    length (flatten_in_order tree) == size_of_tree tree

prop_min_element :: BS_Tree Integer -> Bool
prop_min_element tree = tree == Null || minimum_element tree == head (flatten_in_order tree)

prop_add_one_element :: BS_Tree Char -> Bool
prop_add_one_element tree
	| contains_element 'a' tree =     (size_of_tree tree :: Integer) == size_of_tree (insert_element_to_tree 'a' tree)
	| otherwise                 = 1 + (size_of_tree tree :: Integer) == size_of_tree (insert_element_to_tree 'a' tree)

prop_add_min_max_elements :: BS_Tree Char -> Bool
prop_add_min_max_elements tree = 
	tree == Null 
	|| tree == insert_element_to_tree (maximum_element tree) (insert_element_to_tree (minimum_element tree) tree)

prop_distinct_elements :: String -> Bool
prop_distinct_elements list = length (nub list) == length (flatten_in_order (insert_list_to_tree list Null))
	where
		insert_list_to_tree list' tree = case list' of
			[]    -> tree
			x: xs -> insert_element_to_tree x (insert_list_to_tree xs tree)

prop_is_valid_binary_search_tree :: BS_Tree Char -> Bool
prop_is_valid_binary_search_tree tree = check_tree_bounds tree Nothing Nothing

	where
		check_tree_bounds :: (Ord a) => BS_Tree a -> (Maybe a) -> (Maybe a) -> Bool
		check_tree_bounds c_tree min_e max_e = case c_tree of 
			Null              -> True
			Node e left right ->    min_e `maybe_less_than` (Just e) && (Just e) `maybe_less_than` max_e
				                  && check_tree_bounds left  min_e (maybe_minmax min (Just e) max_e)
				                  && check_tree_bounds right (maybe_minmax max min_e (Just e)) max_e

		maybe_minmax :: (a -> a -> a) -> (Maybe a) -> (Maybe a) -> (Maybe a)
		maybe_minmax f a b = case (a, b) of
			(Nothing , Nothing ) -> Nothing
			(_       , Nothing ) -> a
			(Nothing , _       ) -> b
			(Just a_v, Just b_v) -> Just (f a_v b_v)

		maybe_less_than :: (Ord a) => (Maybe a) -> (Maybe a) -> Bool
		maybe_less_than a b 	= case (a, b) of
			(Nothing , Nothing ) -> True
			(_       , Nothing ) -> True
			(Nothing , _       ) -> True
			(Just a_v, Just b_v) -> a_v < b_v

prop_reasonably_balanced :: BS_Tree Integer -> Bool
prop_reasonably_balanced tree = 
	tree == Null 
	|| ((size_of_tree tree :: Integer) < 4)
	|| (fromIntegral (depth_of_tree tree :: Integer) :: Double) 
	      <= 2 * logBase 2 (1 + fromIntegral (size_of_tree tree :: Integer))

test_prop_unflattened_size            :: IO ()  
test_prop_min_element                 :: IO ()
test_prop_add_one_element             :: IO ()
test_prop_add_min_max_elements        :: IO ()
test_prop_distinct_elements           :: IO ()
test_prop_is_valid_binary_search_tree :: IO ()
test_prop_reasonably_balanced         :: IO ()

test_prop_unflattened_size            = quickCheck prop_unflattened_size
test_prop_min_element                 = quickCheck prop_min_element
test_prop_add_one_element             = quickCheck prop_add_one_element
test_prop_add_min_max_elements        = quickCheck prop_add_min_max_elements
test_prop_distinct_elements           = quickCheck prop_distinct_elements
test_prop_is_valid_binary_search_tree = quickCheck prop_is_valid_binary_search_tree
test_prop_reasonably_balanced         = quickCheck prop_reasonably_balanced
