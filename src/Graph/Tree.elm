module Graph.Tree
  ( Tree, Forest
  -- BUILDING
  , empty, leaf, inner, unfoldTree, unfoldForest
  -- QUERY
  , isEmpty, root
  -- TRAVERSAL
  , levelOrder, levelOrderList
  , preOrder, preOrderList
  , postOrder, postOrderList
  ) where


{-| This module provides a simple tree data type of arbitrary arity (a rose tree).
There are primitives for building and traversing such a tree.

# Data
@docs Tree, Forest

# Building
@docs empty, leaf, inner, unfoldTree, unfoldForest

# Query
@docs isEmpty, root

# Traversal
@docs levelOrder, levelOrderList, preOrser, preOrderList, postOrder postOrderList

-}

    
import Queue exposing (Queue)


type alias InnerNode a =
    { label : a
    , children : List (Tree a)
    }

    
{-| Data type representing an n-ary tree with node labels of type `a`
Building such a tree is done with the `empty`, `leaf` and `inner` smart
constructors. An example for a tree with three leafs and a root node:

    tree = inner 4 [leaf 1, leaf 2, leaf 3] 
-}
type Tree a =
    MkTree (Maybe (InnerNode a))
    

{-| This is just an alias for a list of trees, called a forest in the
literature.
-}
type alias Forest a =
    List (Tree a)


{- BUILDING -}


{-| Construct an empty tree with no nodes. -}
empty : Tree a
empty = MkTree Nothing


{-| Construct a tree with a single node from a value for the node's label.

    tree : Tree Int
    tree = leaf 42
 -}
leaf : a -> Tree a
leaf val =
  inner val []


{-| Construct a new tree by `inner label children`, combining a number of
subtrees `children` with a `label` for the new inner node which will be
the root of the tree. Empty subtrees are filtered out. An example:

    tree1 = inner 4 [leaf 1, leaf 2, leaf 3, empty] 
    tree2 = inner 4 [leaf 1, leaf 2, leaf 3] 
    tree1 == tree2
-}
inner : a -> List (Tree a) -> Tree a
inner label children =
  children
   |> List.filter (not isEmpty)
   |> InnerNode label
   |> Just
   |> MkTree


unfoldTree : (seed -> (label, List seed)) -> seed -> Tree label
unfoldTree next seed =
  let
    (label, seeds) = next seed
  in
    inner label (List.map (unfoldTree next) seeds)


unfoldForest : (seed -> (label, List seed)) -> List seed -> List (Tree label)
unfoldForest next seeds =
  List.map (unfoldTree next) seeds

      
{- QUERY -}


isEmpty : Tree a -> Bool
isEmpty tree =
  case root tree of
    Just _ -> False
    Nothing -> True


root : Tree a -> Maybe (InnerNode a)
root tree = 
    case tree of
        MkTree maybe -> maybe


{- TRAVERSAL -}


-- no type annotation for this, traversal is quite daunting.
listForTraversal traversal tree =
  -- we will compute a DList that we then can turn into a List.
  let
    f inner rest =
        (::) inner.label >> rest
    acc = identity
  -- the call to postOrder returns a DList ([a] -> [a]), so [] turns it into a list
  in
    traversal f acc tree []     


pushMany : List a -> Queue a -> Queue a
pushMany vals queue =
  List.foldl Queue.push queue vals


levelOrder : (InnerNode a -> acc -> acc) -> acc -> Tree a -> acc
levelOrder f acc tree =
  let
    go acc toVisit =
      case Queue.pop toVisit of
        Nothing -> acc
        Just (tree', toVisit') ->
          case root tree' of
            Nothing -> go acc toVisit'
            Just inner ->
              go (f inner acc) (pushMany inner.children toVisit')
  in
    go acc (Queue.empty |> Queue.push tree)

              
levelOrderList : Tree a -> List a
levelOrderList =
  listForTraversal levelOrder
                        

postOrder : (InnerNode a -> acc -> acc) -> acc -> Tree a -> acc
postOrder f acc tree =
  let
    folder = flip (postOrder f)
  in
    case root tree of
      Nothing -> acc
      Just inner ->
        f inner (List.foldl folder acc inner.children)


postOrderList : Tree a -> List a
postOrderList =
  listForTraversal postOrder

                   
preOrder : (InnerNode a -> acc -> acc) -> acc -> Tree a -> acc
preOrder f acc tree =
  let
    folder = flip (preOrder f)
  in
    case root tree of
      Nothing -> acc
      Just inner ->
        List.foldl folder (f inner acc) inner.children


preOrderList : Tree a -> List a
preOrderList =
  listForTraversal preOrder
