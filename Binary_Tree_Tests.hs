import Binary_Tree
import Binary_Tree_Test_Generator ()
import Test.QuickCheck

prop_unflattened_size :: Binary_Tree String -> Bool
prop_unflattened_size tree = length (flatten_tree tree) == size_of_tree tree

prop_keep_size :: Binary_Tree Integer -> Bool
prop_keep_size tree = (size_of_tree tree :: Integer) == size_of_tree (map_function_over_binary_tree (+1) tree)

prop_reverse_function :: Binary_Tree Integer -> Bool
prop_reverse_function tree = tree == map_function_over_binary_tree (+(-1)) (map_function_over_binary_tree (+1) tree)

prop_larger_than_deep :: Binary_Tree Integer -> Bool
prop_larger_than_deep tree = (size_of_tree tree :: Integer) >= depth_of_tree tree

test_prop_unflattened_size :: IO ()
test_prop_keep_size        :: IO ()
test_prop_reverse_function :: IO ()
test_prop_larger_than_deep :: IO ()

test_prop_unflattened_size = quickCheck prop_unflattened_size
test_prop_keep_size        = quickCheck prop_keep_size
test_prop_reverse_function = quickCheck prop_reverse_function
test_prop_larger_than_deep = quickCheck prop_reverse_function