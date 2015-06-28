module Graph.Tree where

    
import Queue exposing (Queue)


type alias InnerNode a =
    { label : a
    , children : List (Tree a)
    }

    
type Tree a =
    MkTree (Maybe (InnerNode a))
    

type alias Forest a =
    List (Tree a)


{- BUILDING -}
  

empty : Tree a
empty = MkTree Nothing


leaf : a -> Tree a
leaf val =
  inner val []

        
inner : a -> List (Tree a) -> Tree a
inner label children =
  MkTree (Just (InnerNode label children))
        

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

{- TRAVERSALS -}


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
