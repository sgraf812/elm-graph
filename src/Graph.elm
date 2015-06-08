module Graph
    ( NodeId, Node, Edge, Adjacency, NodeContext, Decomposition, Graph
    -- Building
    , empty, update, insert, remove 
    -- Query
    , isEmpty, member, get, nodeRange
    -- List representations
    , nodeIds, nodes, edges, fromNodesAndEdges
    -- Foci
    , id, label, from, to, node, incoming, outgoing
    , nodeById, anyNode
    

    , fold
    , mapContexts
    , mapNodes
    , mapEdges

    , symmetricClosure, reverseEdges 
    , dfsList, dfs, guidedDfs
    , bfsList, bfs, guidedBfs
    , heightLevels

    , toString'
    , g, g2
    ) where
    

import IntDict as IntDict exposing (IntDict)
import Maybe as Maybe exposing (Maybe)
import Lazy as Lazy exposing (Lazy)
import Focus as Focus exposing (Focus, (=>))
import Debug
    
type alias NodeId = Int


type alias Node n =
    { id : NodeId
    , label : n
    }

     
type alias Edge e =
    { from : NodeId
    , to : NodeId
    , label : e 
    }

       
type alias Adjacency e = IntDict e


type alias NodeContext n e =
    { node : Node n
    , incoming : Adjacency e
    , outgoing : Adjacency e
    }

           
type alias Decomposition n e =
    { matched : NodeContext n e
    , rest : Lazy (Graph n e)
    }



-- We will only have the Patricia trie based DynGraph implementation for simplicity.
-- Also, there is no real practical reason to separate that or to allow other implementations
-- which would justify the complexity.

type alias GraphRep n e = IntDict (NodeContext n e)

    
type Graph n e = Graph (GraphRep n e)


unGraph : Graph n e -> GraphRep n e
unGraph graph = case graph of Graph rep -> rep


{- BUILD -}

empty : Graph n e
empty = Graph IntDict.empty


type EdgeUpdate e =
    Insert e
    | Remove e


type alias EdgeDiff e =
   { incoming : IntDict (EdgeUpdate e)
   , outgoing : IntDict (EdgeUpdate e)
   }


emptyDiff : EdgeDiff e
emptyDiff =
   { incoming = IntDict.empty
   , outgoing = IntDict.empty
   }
     

computeEdgeDiff : Maybe (NodeContext n e) -> Maybe (NodeContext n e) -> EdgeDiff e
computeEdgeDiff old new =
    let collectUpdates edgeUpdate updatedId label =
            let replaceUpdate old =
                    case (old, edgeUpdate label) of
                        (Just (Remove lbl), (Insert lbl)) -> Nothing
                        (Just (Remove _), (Insert newLbl)) -> Just (Insert newLbl)
                        (Just (Remove _), (Remove _)) ->
                            Debug.crash "computeEdgeDiff collected two removals for the same edge. This is an error in the implementation of Data.Graph and you should file a bug report!"
                        (Just (Insert _), _) ->
                            Debug.crash "computeEdgeDiff collected inserts before removals. This is an error in the implementation of Data.Graph and you should file a bug report!"
                        (Nothing, eu) -> Just eu
            in IntDict.update updatedId replaceUpdate
        collect edgeUpdate adj updates =
            IntDict.foldl (collectUpdates edgeUpdate) updates adj
    in case (old, new) of
        (Nothing, Nothing) -> emptyDiff
        (Just ctx, Just ctx) -> emptyDiff
        (Just rem, Nothing) ->
            { outgoing = IntDict.empty |> collect Remove rem.incoming
            , incoming = IntDict.empty |> collect Remove rem.outgoing
            }
        (Nothing, Just ins) -> 
            { outgoing = IntDict.empty |> collect Insert ins.incoming
            , incoming = IntDict.empty |> collect Insert ins.outgoing
            }
        (Just rem, Just ins) ->
            { outgoing = IntDict.empty |> collect Remove rem.incoming |> collect Insert ins.incoming
            , incoming = IntDict.empty |> collect Remove rem.outgoing |> collect Insert ins.outgoing
            }


applyEdgeDiff : NodeId -> EdgeDiff e -> GraphRep n e -> GraphRep n e
applyEdgeDiff nodeId diff =
    let foldl' f dict acc = IntDict.foldl f acc dict
        edgeUpdateToMaybe edgeUpdate =
            case edgeUpdate of
                Insert lbl -> Just lbl
                Remove _ -> Nothing
        updateAdjacency edgeFocus updatedId edgeUpdate =
            let updateLbl = Focus.set edgeFocus (edgeUpdateToMaybe edgeUpdate)
            in IntDict.update updatedId (Maybe.map updateLbl) -- ignores edges to nodes not in the graph
    in  foldl' (updateAdjacency (incoming => lookup nodeId)) diff.incoming
     >> foldl' (updateAdjacency (outgoing => lookup nodeId)) diff.outgoing


update : NodeId -> (Maybe (NodeContext n e) -> Maybe (NodeContext n e)) -> Graph n e -> Graph n e
update nodeId updater =
   -- This basically wraps updater so that the edges are consistent.
   -- This is, it cannot use the lookup focus, because it needs to update other contexts, too. 
   let updater' rep = 
           let old = IntDict.get nodeId rep
               new = updater old
               diff = computeEdgeDiff old new
           in rep
               |> applyEdgeDiff nodeId diff
               |> IntDict.update nodeId updater
   in Focus.update graphRep updater'


insert : NodeContext n e -> Graph n e -> Graph n e
insert nodeContext graph =
    update nodeContext.node.id (always (Just nodeContext)) graph


remove : NodeId -> Graph n e -> Graph n e
remove nodeId graph = 
    update nodeId (always Nothing) graph
            

{- QUERY -}


isEmpty : Graph n e -> Bool
isEmpty graph = IntDict.isEmpty (unGraph graph)


member : NodeId -> Graph n e -> Bool
member nodeId =
   Focus.get graphRep >> IntDict.member nodeId


get : NodeId -> Graph n e -> Maybe (NodeContext n e)
get nodeId =
   Focus.get (graphRep => lookup nodeId) 

   
nodeRange : Graph n e -> Maybe (NodeId, NodeId)
nodeRange graph =
    let rep = Focus.get graphRep graph
    in  IntDict.findMin rep `Maybe.andThen` \(min, _) ->
        IntDict.findMax rep `Maybe.andThen` \(max, _) ->
        Just (min, max)


{- LIST REPRESENTATIONS -}
            

nodes : Graph n e -> List (Node n)
nodes graph =
    case graph of
      Graph rep -> List.map .node (IntDict.values rep)

      
nodeIds : Graph n e -> List (NodeId)
nodeIds graph =
    case graph of
      Graph rep -> IntDict.keys rep

      
edges : Graph n e -> List (Edge e)
edges graph =
    let foldl' f dict list = IntDict.foldl f list dict -- so that we can use pointfree notation
        prependEdges node1 ctx =
             foldl' (\node2 e -> (::) { to = node2, from = node1, label = e }) ctx.outgoing 
    in case graph of
         Graph rep ->
             foldl' prependEdges rep []

             
fromNodesAndEdges : List (Node n) -> List (Edge e) -> Graph n e
fromNodesAndEdges nodes edges = 
    let nodeRep = List.foldl (\n rep -> IntDict.insert n.id { node = n, outgoing = IntDict.empty, incoming = IntDict.empty } rep) IntDict.empty nodes
        addEdge edge rep =
            let updateOutgoing ctx =
                    { ctx | outgoing <- IntDict.insert edge.to edge.label ctx.outgoing }
                updateIncoming ctx =
                    { ctx | incoming <- IntDict.insert edge.from edge.label ctx.incoming }
            in rep
                |> IntDict.update edge.from (Maybe.map updateOutgoing)
                |> IntDict.update edge.to (Maybe.map updateIncoming)
    in Graph (List.foldl addEdge nodeRep edges)
        

{- FOCI -}


id : Focus { record | id : field } field
id = Focus.create .id (\update record -> { record | id <- update record.id }) 

     
label : Focus { record | label : field } field
label = Focus.create .label (\update record -> { record | label <- update record.label }) 


from : Focus { record | from : field } field
from = Focus.create .from (\update record -> { record | from <- update record.from }) 

       
to : Focus { record | to : field } field
to = Focus.create .to (\update record -> { record | to <- update record.to }) 


node : Focus { record | node : field } field
node = Focus.create .node (\update record -> { record | node <- update record.node }) 


incoming : Focus { record | incoming : field } field
incoming = Focus.create .incoming (\update record -> { record | incoming <- update record.incoming }) 
          

outgoing : Focus { record | outgoing : field } field
outgoing = Focus.create .outgoing (\update record -> { record | outgoing <- update record.outgoing }) 


graphRep : Focus (Graph n e) (GraphRep n e)
graphRep = Focus.create unGraph (\update -> unGraph >> update >> Graph)


lookup : NodeId -> Focus (IntDict v) (Maybe v)
lookup nodeId = Focus.create (IntDict.get nodeId) (IntDict.update nodeId)


nodeById : NodeId -> Focus (Graph n e) (Maybe (NodeContext n e))
nodeById nodeId = Focus.create (get nodeId) (update nodeId)
          

anyNode : Focus (Graph n e) (Maybe (NodeContext n e))
anyNode =
    let getMinId = Focus.get graphRep >> IntDict.findMin >> Maybe.map fst
        get graph = getMinId graph `Maybe.andThen` \id -> Focus.get (nodeById id) graph
        update upd graph =
            let nodeId = Maybe.withDefault 0 (getMinId graph)
            in Focus.update (nodeById nodeId) upd graph
    in Focus.create get update


-- TRANSFORMS


fold : (NodeContext n e -> Lazy acc -> acc) -> acc -> Graph n e -> acc
fold f acc graph =
    case Focus.get anyNode graph of
        Just ctx -> f ctx <| Lazy.lazy <| \_ -> fold f acc (remove ctx.node.id graph)
        Nothing -> acc


mapContexts : (NodeContext n1 e1 -> NodeContext n2 e2) -> Graph n1 e1 -> Graph n2 e2
mapContexts f = fold (\ctx -> Lazy.force >> insert (f ctx)) empty


mapNodes : (n1 -> n2) -> Graph n1 e -> Graph n2 e
mapNodes f = fold (\ctx -> Lazy.force >> insert { ctx | node <- { id = ctx.node.id, label = f ctx.node.label } }) empty

             
mapEdges : (e1 -> e2) -> Graph n e1 -> Graph n e2
mapEdges f =
    fold (\ctx -> Lazy.force >> insert
                  { ctx
                  | outgoing <- IntDict.map (\n e -> f e) ctx.outgoing
                  , incoming <- IntDict.map (\n e -> f e) ctx.incoming })
         empty


{- GRAPH OPS -}


symmetricClosure : (NodeId -> NodeId -> e -> e -> e) -> Graph n e -> Graph n e
symmetricClosure edgeMerger =
    -- We could use mapContexts, but this will be more efficient.
    let orderedEdgeMerger from to outgoing incoming =
            if from <= to
            then edgeMerger from to outgoing incoming
            else edgeMerger to from incoming outgoing
        updateContext nodeId ctx =
            let edges = IntDict.uniteWith (orderedEdgeMerger nodeId) ctx.outgoing ctx.incoming
            in { ctx | outgoing <- edges, incoming <- edges }
    in Focus.update graphRep (IntDict.map updateContext)


reverseEdges : Graph n e -> Graph n e
reverseEdges =
    let updateContext nodeId ctx =
            { ctx | outgoing <- ctx.incoming, incoming <- ctx.outgoing }
    in Focus.update graphRep (IntDict.map updateContext)


{- DFS -}


guidedDfs
    :  (Graph n e -> NodeContext n e -> info -> List (NodeId, info))
    -> (NodeContext n e -> info -> Lazy acc -> acc)
    -> List (NodeId, info)
    -> acc
    -> Graph n e
    -> acc
guidedDfs selectNeighbors visitNode seeds acc graph =
    let go stack graph =
            case stack of
                [] -> acc
                (next, info) :: stack' ->
                    case get next graph of
                        Nothing -> Debug.crash "Graph.guidedDfs: Tried to visit non-existent or already deleted node. This probably means that your selectNeighbors function or your seed is invalid! Otherwise it's a bug worth reporting."
                        Just ctx ->
                            visitNode ctx
                                      info <| Lazy.lazy <| \_ ->
                                      go (selectNeighbors graph ctx info ++ stack') (remove next graph)
    in if isEmpty graph
       then acc
       else go seeds graph


type alias GuidedDfsOrBfs n e info acc =
    (Graph n e -> NodeContext n e -> info -> List (NodeId, info))
    -> (NodeContext n e -> info -> Lazy acc -> acc)
    -> List (NodeId, info)
    -> acc
    -> Graph n e
    -> acc


startWithAnyNodeAndVisitOutgoingNeighbors
    :  GuidedDfsOrBfs n e () acc
    -> (NodeContext n e -> Lazy acc -> acc)
    -> acc
    -> Graph n e
    -> acc
startWithAnyNodeAndVisitOutgoingNeighbors guidedDfsOrBfs visitNode acc graph =
    case Focus.get anyNode graph of
        Just ctx ->
            let selectNeighbors _ ctx _ = ctx |> Focus.get outgoing |> IntDict.keys |> List.map (\id -> (id, ()))
                visitNode' ctx _ acc = visitNode ctx acc
                seed = (ctx.node.id, ())
            in guidedDfsOrBfs selectNeighbors visitNode' [seed] acc graph
        Nothing -> acc


dfs : (NodeContext n e -> Lazy acc -> acc) -> acc -> Graph n e -> acc
dfs = startWithAnyNodeAndVisitOutgoingNeighbors guidedDfs


accumulateNodesInList : NodeContext n e -> Lazy (List (NodeContext n e)) -> List (NodeContext n e)
accumulateNodesInList ctx rest =
    ctx :: Lazy.force rest

    
dfsList : Graph n e -> List (NodeContext n e)
dfsList graph =
    dfs accumulateNodesInList [] graph


{- BFS -}


type alias Queue a =
    { front : List a
    , back : List a
    }


enqueue : List a -> Queue a -> Queue a
enqueue vals queue =
    { queue
    | back <- vals ++ queue.back
    }


popFront : Queue a -> Maybe (a, Queue a)
popFront queue =
    case queue.front of
        val :: front' -> Just (val, { queue | front <- front' })
        [] ->
            case List.reverse queue.back of
                [] -> Nothing
                val :: front' -> Just (val, { front = front', back = [] })
    

guidedBfs
    :  (Graph n e -> NodeContext n e -> info -> List (NodeId, info))
    -> (NodeContext n e -> info -> Lazy acc -> acc)
    -> List (NodeId, info)
    -> acc
    -> Graph n e
    -> acc
guidedBfs selectNeighbors visitNode seeds acc graph =
    let go queue graph =
            case popFront queue of
                Nothing -> acc
                Just ((next, info), queue') ->
                    case get next graph of
                        -- This can actually happen since we don't filter the queue for already visited nodes.
                        Nothing -> go queue' (remove next graph) 
                        Just ctx ->
                            visitNode ctx
                                      info <| Lazy.lazy <| \_ ->
                                      go (enqueue (selectNeighbors graph ctx info) queue') (remove next graph)
    -- There is significant overlap with guidedDfs, differing just in the container type used.
    in if isEmpty graph
       then acc
       else go { front = seeds, back = [] } graph
            

bfs : (NodeContext n e -> (Lazy acc) -> acc) -> acc -> Graph n e -> acc
bfs = startWithAnyNodeAndVisitOutgoingNeighbors guidedBfs


bfsList : Graph n e -> List (NodeContext n e)
bfsList graph =
    bfs accumulateNodesInList [] graph

    
heightLevels : Graph n e -> List (List (NodeContext n e))
heightLevels graph =
    let isSource nodeId =
            graph
             |> get nodeId
             |> Maybe.map (.incoming >> IntDict.isEmpty)
             |> Maybe.withDefault False 
        sources =
            graph
             |> nodeIds
             |> List.filter isSource
             |> List.map (\id -> (id, 0))
        hasOnlyOneIncomingEdge graph' nodeId =
            graph'
             |> get nodeId
             |> Maybe.map (.incoming >> IntDict.size >> (==) 1)
             |> Maybe.withDefault False
        selectNeighbors graph' ctx height =
            ctx.outgoing
             |> IntDict.keys
             |> List.filter (hasOnlyOneIncomingEdge graph')
             |> List.map (\id -> (id, height + 1))
        visitNode ctx depth rest =
            let (levels, minDepth) = Lazy.force rest
            in case levels of
                [] -> ([[ctx]], depth)
                level :: lowerLevels ->
                    if  | depth == minDepth -> ((ctx :: level) :: lowerLevels, minDepth)
                        | depth + 1 == minDepth -> ([ctx] :: levels, minDepth - 1)
                        | otherwise -> Debug.crash "Graph.heightLevels: Reached a branch which is impossible by invariants. Please file a bug report!"
    in guidedBfs selectNeighbors visitNode sources ([], 0) graph |> fst

      
{- toString -}

toString' : Graph n e -> String
toString' graph =
    let nodeList = nodes graph
        edgeList = edges graph
    in
        "Graph.fromNodesAndEdges " ++ toString nodeList ++ " " ++ toString edgeList


g : Graph () String
g = fromNodesAndEdges [ { id = 1, label = () }, { id = 2, label = () } ] [ { from = 2, to = 1, label = "arrow" } ]


type alias State s a = s -> (a, s)

n : n -> State NodeId (Node n)
n lbl id =
    ({ id = id, label = lbl }, id + 1)

(>>=) : State s a -> (a -> State s b) -> State s b
state >>= f = \id ->
    let (n, id') = state id
    in f n id'

pure : a -> State s a
pure val id = (val, id)

sequence : List (State s a) -> State s (List a)
sequence states =
    case states of
        [] -> pure []
        s :: states' ->
            s >>= \val -> sequence states' >>= \rest -> pure (val :: rest)
    
            --\id -> let (x, id') = s id
             --          (rest, id'') = sequence states' id'
              --     in x :: rest
e from to = { from = from, to = to, label = () }
         
g2 : Graph String ()
g2 =
    fromNodesAndEdges
        (sequence [n "Shorts", n "Socks", n "Pants", n "Undershirt", n "Sweater", n "Coat", n "Shoes" ] 0 |> fst)
        [e 0 2, e 1 6, e 2 5, e 2 6, e 3 4, e 4 5]