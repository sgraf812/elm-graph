module Graph
    ( NodeId, Node, Edge, Adjacency, NodeContext, Graph
    -- Building
    , empty, update, insert, remove
    -- Query
    , isEmpty, size, member, get, nodeIdRange
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
@docs isEmpty, size, member, get, nodeIdRange

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
type alias NodeId =
  Int


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
unGraph graph =
  case graph of Graph rep -> rep


{- BUILD -}

{-| An empty graph.

    size empty == 0
-}
empty : Graph n e
empty =
  Graph IntDict.empty


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
  let
    collectUpdates edgeUpdate updatedId label =
      let
        replaceUpdate old =
          case (old, edgeUpdate label) of
            (Just (Remove lbl), (Insert lbl)) ->
              Nothing
            (Just (Remove _), (Insert newLbl)) ->
              Just (Insert newLbl)
            (Just (Remove _), (Remove _)) ->
              Debug.crash "Graph.computeEdgeDiff: Collected two removals for the same edge. This is an error in the implementation of Data.Graph and you should file a bug report!"
            (Just (Insert _), _) ->
              Debug.crash "Graph.computeEdgeDiff: Collected inserts before removals. This is an error in the implementation of Data.Graph and you should file a bug report!"
            (Nothing, eu) ->
              Just eu
      in
        IntDict.update updatedId replaceUpdate

    collect edgeUpdate adj updates =
      IntDict.foldl (collectUpdates edgeUpdate) updates adj
in
  case (old, new) of
    (Nothing, Nothing) ->
      emptyDiff
    (Just ctx, Just ctx) ->
      emptyDiff
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
applyEdgeDiff nodeId diff graphRep =
  let
    foldl' f dict acc =
      IntDict.foldl f acc dict

    edgeUpdateToMaybe edgeUpdate =
      case edgeUpdate of
        Insert lbl -> Just lbl
        Remove _ -> Nothing

    updateAdjacency edgeFocus updatedId edgeUpdate =
      let
        updateLbl =
          Focus.set edgeFocus (edgeUpdateToMaybe edgeUpdate)
      in
        IntDict.update updatedId (Maybe.map updateLbl) -- ignores edges to nodes not in the graph

  in
    graphRep
      |> foldl' (updateAdjacency (incoming => lookup nodeId)) diff.incoming
      |> foldl' (updateAdjacency (outgoing => lookup nodeId)) diff.outgoing


{-| Analogous to `Dict.update`, `update nodeId updater graph` will find
the node context of the node with id `nodeId` in `graph`. It will then call `updater`
with that `Just` that node context if that node was found and `Nothing`
otherwise. `updater` can then return `Just` an updated node context
(modifying edges is also permitted!) or delete the node by returning
`Nothing`. The updated `graph` is returned.

This is the most powerful building function since all possible per-node
operations are possible (node removal, insertion and updating of context
properties).

The other operations can be implemented in terms of `update` like this:

    remove nodeId graph = update nodeId (always Nothing) graph
    insert nodeContext graph = update nodeContext.node.id (always (Just nodeContext)) graph
-}
update : NodeId -> (Maybe (NodeContext n e) -> Maybe (NodeContext n e)) -> Graph n e -> Graph n e
update nodeId updater =
  -- This basically wraps updater so that the edges are consistent.
  -- This is, it cannot use the lookup focus, because it needs to update other contexts, too.
  let
    updater' rep =
      let
        old =
          IntDict.get nodeId rep

        new =
          updater old

        diff =
          computeEdgeDiff old new
      in
        rep
          |> applyEdgeDiff nodeId diff
          |> IntDict.update nodeId updater
  in
    Focus.update graphRep updater'


{-| Analogous to `Dict.insert`, `insert nodeContext graph` inserts a fresh node
with its context (label, id and edges) into `graph`. If there was already a node
with the same id, it will be replaced by the new node context.

    graph1 = fromNodesAndEdges [Node 1 "1"] []
    newNode =
      { node = Node 2 "2"
      , incoming = IntDict.singleton 1 () -- so there will be an edge from 1 to 2
      , outgoing = IntDict.empty
      }
    graph2 = insert newNode graph1
    size graph2 == 2

It's possible to build up whole graphs this way, but a lot less tedious way would
be simply to use `fromNodesAndEdges`.
-}
insert : NodeContext n e -> Graph n e -> Graph n e
insert nodeContext graph =
  update nodeContext.node.id (always (Just nodeContext)) graph


{-| Analogous to `Dict.remove`, `remove nodeId graph` returns a version of `graph`
without a node with id `nodeId`. If there was no node with that id, then remove
is a no-op:

    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] [Edge 1 2 ()]
    graph == remove 42 graph
    graph |> remove 2 |> size == 1
-}
remove : NodeId -> Graph n e -> Graph n e
remove nodeId graph =
  update nodeId (always Nothing) graph


{- QUERY -}


{-| `isEmpty graph` is true if and only if there are no nodes in the graph.
Some properties to reason about in code, which hold for any `graph`:

    isEmpty graph =
      graph == empty
    isEmpty graph =
      size graph == 0
-}
isEmpty : Graph n e -> Bool
isEmpty graph =
  graph == empty


{-| `size graph` returns the number of nodes in `graph`.

    size empty == 0
    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] []
    size graph == 2
-}
size : Graph n e -> Int
size =
  Focus.get graphRep >> IntDict.size


{-| Analogous to `Dict.member`, `member nodeId graph` is true, if and only if
there is a node with id `nodeId` in `graph`.

    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] []
    member 42 graph == False
    member 1 graph == True
-}
member : NodeId -> Graph n e -> Bool
member nodeId =
  Focus.get graphRep >> IntDict.member nodeId


{-| Analogous to `Dict.get`, `get nodeId graph` returns the `Just` the node
context with id `nodeId` in `graph` if there is one and `Nothing` otherwise.

    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] []
    get 42 graph == Nothing
    get 1 graph == Just <node context of node 1>
-}
get : NodeId -> Graph n e -> Maybe (NodeContext n e)
get nodeId =
  Focus.get (graphRep => lookup nodeId)


{-| `nodeIdRange graph` returns `Just (minNodeId, maxNodeId)` if `graph` is not empty and `Nothing`
otherwise.

This is useful for finding unoccupied node ids without trial and error.

    nodeIdRange empty == Nothing
    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] []
    nodeIdRange graph == Just (1, 2)
-}
nodeIdRange : Graph n e -> Maybe (NodeId, NodeId)
nodeIdRange graph =
  let
    rep =
      Focus.get graphRep graph
  in
    IntDict.findMin rep `Maybe.andThen` \(min, _) ->
    IntDict.findMax rep `Maybe.andThen` \(max, _) ->
    Just (min, max)


{- LIST REPRESENTATIONS -}


{-| `nodes graph` returns a list of all `Node`s (e.g. `id` and `label`) in
`graph`.

    nodes empty == []
    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] []
    nodes graph == [Node 1 "1", Node 2 "2"]
-}
nodes : Graph n e -> List (Node n)
nodes =
  Focus.get graphRep >> IntDict.values >> List.map .node


{-| `nodeIds graph` returns a list of all nodes' ids in `graph`.

    nodeIds empty == []
    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] []
    nodeIds graph == [1, 2]
-}
nodeIds : Graph n e -> List (NodeId)
nodeIds graph =
  Focus.get graphRep >> IntDict.keys


{-| `edges graph` returns a list of all `Edge`s (e.g. a record of `from` and `to` ids
and a `label`) in `graph`.

    edges empty == []
    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] [Edge 1 2 "->"]
    edges graph == [Edge 1 2 "->"]
-}
edges : Graph n e -> List (Edge e)
edges graph =
  let
    foldl' f dict list =
      IntDict.foldl f list dict -- dict and list flipped, so that we can use pointfree notation

    prependEdges node1 ctx =
      foldl' (\node2 e -> (::) { to = node2, from = node1, label = e }) ctx.outgoing
  in
    foldl' prependEdges (unGraph graph) []


{-| `fromNodesAndEdges nodes edges` constructs a graph from the supplied `nodes`
and `edges`. This is the most comfortable way to construct a graph as a whole.

The following constructs a graph with 2 nodes with a string label, connected
by an edge labeled "->".

    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] [Edge 1 2 "->"]
-}
fromNodesAndEdges : List (Node n) -> List (Edge e) -> Graph n e
fromNodesAndEdges nodes edges =
  let
    nodeRep =
      List.foldl
        (\n ->
          IntDict.insert n.id (NodeContext n IntDict.empty IntDict.empty))
        IntDict.empty
        nodes

    addEdge edge rep =
      let
        updateOutgoing ctx =
          { ctx | outgoing <- IntDict.insert edge.to edge.label ctx.outgoing }

        updateIncoming ctx =
          { ctx | incoming <- IntDict.insert edge.from edge.label ctx.incoming }
      in
        rep
          |> IntDict.update edge.from (Maybe.map updateOutgoing)
          |> IntDict.update edge.to (Maybe.map updateIncoming)
    in
      Graph (List.foldl addEdge nodeRep edges)


{- FOCI -}


{-| Focus for the `id` field of `Node`.
-}
id : Focus { record | id : field } field
id =
  Focus.create .id (\update record -> { record | id <- update record.id })


{-| Focus for the `label` field of `Node` and `Edge`.
-}
label : Focus { record | label : field } field
label =
  Focus.create .label (\update record -> { record | label <- update record.label })


{-| Focus for the `from` field of `Edge`.
-}
from : Focus { record | from : field } field
from =
  Focus.create .from (\update record -> { record | from <- update record.from })


{-| Focus for the `to` field of `Edge`.
-}
to : Focus { record | to : field } field
to =
  Focus.create .to (\update record -> { record | to <- update record.to })


{-| Focus for the `node` field of `NodeContext`.
-}
node : Focus { record | node : field } field
node =
  Focus.create .node (\update record -> { record | node <- update record.node })


{-| Focus for the `incoming` field of `NodeContext`.
-}
incoming : Focus { record | incoming : field } field
incoming =
  Focus.create .incoming (\update record -> { record | incoming <- update record.incoming })


{-| Focus for the `outgoing` field of `NodeContext`.
-}
outgoing : Focus { record | outgoing : field } field
outgoing =
  Focus.create .outgoing (\update record -> { record | outgoing <- update record.outgoing })


graphRep : Focus (Graph n e) (GraphRep n e)
graphRep =
  Focus.create unGraph (\update -> unGraph >> update >> Graph)


lookup : NodeId -> Focus (IntDict v) (Maybe v)
lookup nodeId =
  Focus.create (IntDict.get nodeId) (IntDict.update nodeId)


{-|  `nodeById nodeId` focuses on the node with id `nodeId` with a `Graph`.
Since the node might or might not exist, the small part on which we focus wraps
the `NodeContext` in a `Maybe`.

This is a combination of the `get` and `update` functions which is handy for
composition of foci deep into a graph. Unfortunately, we need a combinator which
would get rid of the `Maybe` wrapping (that would be the task of a prism I think),
but suppose we have something like `Focus.withDefault : a -> Focus (Maybe a) a`,
then we could define

    ctx = NodeContext (Node 2 "2") IntDict.empty IntDict.empty
    focus = nodeById 2 => Focus.withDefault ctx => node => label
    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] [Edge 1 2 "->"]
    graph' = Focus.set focus graph "<-"
    Focus.get focus graph' == "<-"

Well, I hope I could bring over the point.
-}
nodeById : NodeId -> Focus (Graph n e) (Maybe (NodeContext n e))
nodeById nodeId =
  Focus.create (get nodeId) (update nodeId)


{-| Focuses on an arbitrary `NodeContext` of a `Graph`.
This exists for the same purposes as `nodeById`, but this focuses on an
arbitrary node rather than on a node with a specific id.
-}
anyNode : Focus (Graph n e) (Maybe (NodeContext n e))
anyNode =
  let
    getMinId =
      Focus.get graphRep >> IntDict.findMin >> Maybe.map fst

    get graph =
      getMinId graph `Maybe.andThen` \id -> Focus.get (nodeById id) graph

    update upd graph =
      let
        nodeId =
          Maybe.withDefault 0 (getMinId graph)
      in
        Focus.update (nodeById nodeId) upd graph
  in
    Focus.create get update


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
