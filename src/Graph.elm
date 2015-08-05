module Graph
    ( NodeId, Node, Edge, Adjacency, NodeContext, Graph
    -- Building
    , empty, update, insert, remove 
    -- Query
    , isEmpty, size, member, get, nodeRange
    -- List representations
    , nodeIds, nodes, edges, fromNodesAndEdges
    -- Foci
    , id, label, from, to, node, incoming, outgoing
    , nodeById, anyNode


    , fold
    , mapContexts
    , mapNodes
    , mapEdges

    , isSimple
    , symmetricClosure, reverseEdges 
    , dfs, guidedDfs
--    , bfsList, bfs, guidedBfs
    , heightLevels, topologicalSort

    , toString'
    ) where


{-|

# Data
@docs NodeId, Node, Edge, Adjacency, NodeContext, Graph

# Building
@docs empty, update, insert, remove

# Query
@docs isEmpty, size, member, get, nodeRange

# List representations
@docs nodeIds, nodes, edges, fromNodesAndEdges

# Foci
@docs id, label, from, to, node, incoming, outgoing, nodeById, anyNode

# Transforms
@docs fold, mapContexts, mapNodes, mapEdges, reverseEdges, symmetricsClosure

# Characterization
@docs isSimple

# Traversals
@docs NeighborSelector, DfsNodeVisitor, SimpleNodeVisitor, dfs, guidedDfs, bfs, guidedBfs

# Topological Sort
@docs topologicalSort, heightLevels

-}


import Graph.Tree as Tree exposing (Tree, Forest)
import IntDict as IntDict exposing (IntDict)
import Maybe as Maybe exposing (Maybe)
import Lazy as Lazy exposing (Lazy)
import Focus as Focus exposing (Focus, (=>))
import Queue as Queue exposing (Queue)
import Debug


{-| The type used for identifying nodes, an integer.
-}
type alias NodeId = Int


{-| The type representing a node: An identifier with
a label.
-}
type alias Node n =
  { id : NodeId
  , label : n
  }

     
{-| Represents a directd edge in the graph. In addition
to start and end node identifiers, a label value can
be attached to an edge.
-}
type alias Edge e =
  { from : NodeId
  , to : NodeId
  , label : e 
  }

       
{-| Adjacency is represented as an ordered dictionary
rather than as an ordered list. This enables more dynamic
graphs with efficient edge removal and insertion on the run.
-}
type alias Adjacency e =
  IntDict e


{-| Represents a node with its incoming and outgoing edges
(predecessors and successors).
-}
type alias NodeContext n e =
  { node : Node n
  , incoming : Adjacency e
  , outgoing : Adjacency e
  }

           
-- We will only have the Patricia trie based DynGraph implementation for simplicity.
-- Also, there is no real practical reason to separate that or to allow other implementations
-- which would justify the complexity.

type alias GraphRep n e =
  IntDict (NodeContext n e)

{-| The central graph type. It is parameterized both over the node label type `n`
and the edge label type `e`.

One can build such a graph with the primitives under *Build*. Most of the time
`fromNodesAndEdges` works fairly well.

For simplicity, this library just uses a patricia trie based graph representation, which means
it is just an efficient version of `Dict NodeId (NodeContext n e)`. This allows efficient insertion and
removal of nodes of the graph after building.
-}
type Graph n e =
  Graph (GraphRep n e)


unGraph : Graph n e -> GraphRep n e
unGraph graph = case graph of Graph rep -> rep


{- BUILD -}

{-| An empty graph.

    size empty == 0
-}
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


{-| 
-}
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


size : Graph n e -> Int
size =
   Focus.get graphRep >> IntDict.size
                

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


{- TRANSFORMS -}


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


{- CHARACTERIZATION -}


isSimple : Graph n e -> Bool
isSimple graph =
    let checkForLoop ctx rest =
            IntDict.member ctx.node.id ctx.incoming || Lazy.force rest
    in graph
        |> fold checkForLoop False
        |> not


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


{- Traversals -}


type alias NeighborSelector n e =
    NodeContext n e
    -> List NodeId


type alias DfsNodeVisitor n e acc =
    NodeContext n e
    -> acc
    -> (acc, acc -> acc)


type alias SimpleNodeVisitor n e acc =
    NodeContext n e
    -> acc
    -> acc


alongOutgoingEdges : NeighborSelector n e
alongOutgoingEdges ctx =
    IntDict.keys (ctx.outgoing)


onDiscovery : SimpleNodeVisitor n e acc -> DfsNodeVisitor n e acc
onDiscovery visitor ctx acc =
    (visitor ctx acc, identity)


onFinish : SimpleNodeVisitor n e acc -> DfsNodeVisitor n e acc
onFinish visitor ctx acc =
    (acc, visitor ctx)


guidedDfs
    :  NeighborSelector n e
    -> DfsNodeVisitor n e acc
    -> List NodeId
    -> acc
    -> Graph n e
    -> (acc, Graph n e)
guidedDfs selectNeighbors visitNode seeds acc graph =
    let go seeds acc graph =
            case seeds of
                [] -> -- We are done with this connected component, so we return acc and the rest of the graph
                    (acc, graph)
                next :: seeds' ->
                    case get next graph of
                        -- This can actually happen since we don't filter for already visited nodes.
                        Nothing -> go seeds' acc graph
                        Just ctx ->
                            let neighbors = selectNeighbors ctx
                                visitChildren acc' = go neighbors acc' graph' |> fst
                                (accAfterDiscovery, finishNode) = visitNode ctx acc
                                (accBeforeFinish, graph') = go neighbors accAfterDiscovery (remove next graph)
                                accAfterFinish = finishNode accBeforeFinish
                            in go seeds' accAfterFinish graph'
    in go seeds acc graph


dfs : DfsNodeVisitor n e acc -> acc -> Graph n e -> acc
dfs visitNode acc graph =
    guidedDfs alongOutgoingEdges visitNode (nodeIds graph) acc graph |> fst


dfsTree : NodeId -> Graph n e -> Tree (NodeContext n e)
dfsTree seed graph =
   case dfsForest [seed] graph of
       [] -> Tree.empty
       [tree] -> tree
       _ -> Debug.crash "dfsTree: There can't be more than one DFS tree. This invariant is violated, please report this bug."


dfsForest : List NodeId -> Graph n e -> Forest (NodeContext n e)
dfsForest seeds graph =
    let visitNode ctx trees =
            ([], Tree.inner ctx >> flip (::) trees)
    in guidedDfs alongOutgoingEdges visitNode seeds [] graph |> fst


heightLevels : Graph n e -> List (List (NodeContext n e))
heightLevels graph =
    let
      countIndegrees =
           fold
             (\ctx dict ->
                 IntDict.insert
                   ctx.node.id
                   (IntDict.size ctx.incoming)
                   (Lazy.force dict))
             IntDict.empty

      subtract a b =
          b - a

      decrementAndNoteSources id _ (nextLevel, indegrees) =
          let
            indegrees' = IntDict.update id (Maybe.map (subtract 1)) indegrees
          in
            case IntDict.get id indegrees' of
                Just 0 ->
                  case get id graph of
                    Just ctx -> (ctx :: nextLevel, indegrees')
                    Nothing -> Debug.crash "Graph.heightLevels: Could not get a node of a graph which should be there by invariants. Please file a bug report!"
                Nothing -> (nextLevel, indegrees')

      decrementIndegrees source nextLevel indegrees =
          IntDict.foldl decrementAndNoteSources (nextLevel, indegrees) source.outgoing

      go currentLevel nextLevel indegrees graph =
          case (currentLevel, nextLevel) of
              ([], []) ->
                  [[]]
              ([], _) ->
                  [] :: go nextLevel [] indegrees graph
              (source :: currentLevel', _) -> 
                  let
                    (nextLevel', indegrees') = decrementIndegrees source nextLevel indegrees
                  in
                    case go currentLevel' nextLevel' indegrees' (remove source.node.id graph) of
                      [] -> Debug.crash "Graph.heightLevels: Reached a branch which is impossible by invariants. Please file a bug report!"
                      level :: levels -> (source :: level) :: levels
    in
      go [] [] (countIndegrees graph) graph


topologicalSort : Graph n e -> List (NodeContext n e)
topologicalSort graph =
    graph
     |> dfsForest (nodeIds graph) 
     |> List.concatMap Tree.preOrderList


{- toString -}

toString' : Graph n e -> String
toString' graph =
    let nodeList = nodes graph
        edgeList = edges graph
    in
        "Graph.fromNodesAndEdges " ++ toString nodeList ++ " " ++ toString edgeList
