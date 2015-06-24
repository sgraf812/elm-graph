module Graph.Tree where


type alias InnerNode a =
    { label : a
    , children : List (Tree a)
    }

    
type Tree a = MkTree (Maybe (InnerNode a))


unTree : Tree a -> Maybe (InnerNode a)
unTree tree =
    case tree of
        MkTree maybe -> maybe
    

type alias Forest a =
    List (Tree a)


empty : Tree a
empty = MkTree Nothing


isEmpty : Tree a -> Bool
isEmpty tree =
    case unTree tree of
        Just _ -> False
        Nothing -> True


root : Tree a -> Maybe (InnerNode a)
root = unTree
        

unfoldTree : (seed -> (label, List seed)) -> seed -> Tree label
unfoldTree next seed =
    let (label, seeds) = next seed
    in Root
        { label = label
        , children = List.map (unfoldTree next) seeds
        }


unfoldForest : (seed -> (label, List seed)) -> List seed -> List (Tree label)
unfoldForest next seeds =
    List.map (unfoldTree next) seeds


postorder : (InnerNode a -> acc) -> acc -> Tree a -> acc
postorder f acc tree =
    let folder = flip (postorder f)
    in case root tree of
        Nothing -> acc
        Just inner ->
            f inner (List.foldl folder acc inner.children)


postorderList : Tree a -> [a]
postorderList tree =
    -- we will compute a DList instead
    let f inner rest =
            (::) inner.label >> rest
        acc = identity
    -- the call to postorder returns a DList ([a] -> [a]), so [] turns it into a list
    in postorder f acc tree [] 

    
preorder : (InnerNode a -> acc) -> acc -> Tree a -> acc
preorder f acc tree =
    let folder = flip (postorder f)
    in case root tree of
        Nothing -> acc
        Just inner ->
            List.foldl folder (f inner acc) inner.children


preorderList : Tree a -> [a]
preorderList tree =
    let f inner rest =
            inner.label :: rest
        acc = []
    in preorder f acc tree


