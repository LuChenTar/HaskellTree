data Rose_Tree a = Rose_Node a [Rose_Tree a]
    deriving (Show, Eq)

size_of_tree :: Integral b => Rose_Tree a -> b
size_of_tree (Rose_Node a rs) = foldr (+) 1 (map size_of_tree rs)

depth_of_tree :: Integral b => Rose_Tree a -> b
depth_of_tree tree = case tree of
    Rose_Node a [] -> 1
    Rose_Node a rs -> 1 + maximum (map depth_of_tree rs)


leaves_of_tree :: Rose_Tree a -> [a] 
leaves_of_tree tree = get_leaf [tree]
    where
        get_leaf :: [Rose_Tree a] -> [a]
        get_leaf tree = case tree of
            [] -> []
            t:ts -> case t of
                Rose_Node x [] -> x : get_leaf ts
                Rose_Node x xs -> get_leaf xs ++ get_leaf ts

map_function_over_rose_tree :: (a -> b) -> Rose_Tree a -> Rose_Tree b
map_function_over_rose_tree f tree = case tree of
    Rose_Node a [] -> Rose_Node (f a) []
    Rose_Node a tree -> Rose_Node (f a) (map (map_function_over_rose_tree f) tree)

test_case :: Rose_Tree Integer
test_case = Rose_Node 1 [Rose_Node 2 [], Rose_Node 3 [Rose_Node 4 []], Rose_Node 5 [Rose_Node 6 [], Rose_Node 7 []]]