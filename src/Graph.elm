module Graph
  -- Data
  ( NodeId, Node, Edge, Adjacency, NodeContext, Graph
  -- Building
  , empty, update, insert, remove, inducedSubgraph
  -- Query
  , isEmpty, size, member, get, nodeIdRange
  -- List representations
  , nodeIds, nodes, edges, fromNodesAndEdges, fromNodeLabelsAndEdgePairs
  -- Foci
  , id, label, from, to, node, incoming, outgoing
  , nodeById, anyNode

  -- Transforms
  , fold, mapContexts, mapNodes, mapEdges
  , symmetricClosure, reverseEdges

  -- Characterization

  -- Traversals
  , NeighborSelector, alongOutgoingEdges, alongIncomingEdges, SimpleNodeVisitor
  -- DFS
  , DfsNodeVisitor, onDiscovery, onFinish, dfs, dfsTree, dfsForest, guidedDfs
  -- BFS
  , BfsNodeVisitor, ignorePath, bfs, guidedBfs

  -- Topological sort
  , heightLevels, topologicalSort
  -- Strongly connected components
  , stronglyConnectedComponents

  -- String representation
  , toString'
  ) where

{-| This module contains the primitives to build, update and traverse graphs.
If you find that this module is hard to use or the documentation
is insufficient, consider opening an issue for that (and possibly even a
pull request :)).

Internally, we use the `elm-intdict` package for efficient dynamic graph
representation.

# Data
@docs NodeId, Node, Edge, Adjacency, NodeContext, Graph

# Building
@docs empty, update, insert, remove, inducedSubgraph

# Query
@docs isEmpty, size, member, get, nodeIdRange

# List representations
@docs nodeIds, nodes, edges, fromNodesAndEdges, fromNodeLabelsAndEdgePairs

# Foci
@docs id, label, from, to, node, incoming, outgoing, nodeById, anyNode

# Transforms
@docs fold, mapContexts, mapNodes, mapEdges, reverseEdges, symmetricClosure

# Characterization

# Traversals
## Neighbor selectors and node visitors
@docs NeighborSelector, alongOutgoingEdges, alongIncomingEdges, SimpleNodeVisitor
## Depth-first
@docs DfsNodeVisitor, onDiscovery, onFinish, dfs, dfsTree, dfsForest, guidedDfs
## Bread-first
@docs BfsNodeVisitor, ignorePath, bfs, guidedBfs

# Topological Sort
@docs topologicalSort, heightLevels

# Strongly Connected Components
@docs stronglyConnectedComponents

# String representation
@docs toString'

-}

import Graph.Tree as Tree exposing (Tree, Forest)
import IntDict as IntDict exposing (IntDict)
import Maybe as Maybe exposing (Maybe)
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
              Debug.crash "Graph.computeEdgeDiff: Collected two removals for the same edge. This is an error in the implementation of Graph and you should file a bug report!"
            (Just (Insert _), _) ->
              Debug.crash "Graph.computeEdgeDiff: Collected inserts before removals. This is an error in the implementation of Graph and you should file a bug report!"
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
      (Just rem, Nothing) ->
        { outgoing = IntDict.empty |> collect Remove rem.incoming
        , incoming = IntDict.empty |> collect Remove rem.outgoing
        }
      (Nothing, Just ins) ->
        { outgoing = IntDict.empty |> collect Insert ins.incoming
        , incoming = IntDict.empty |> collect Insert ins.outgoing
        }
      (Just rem, Just ins) ->
        if rem == ins
        then emptyDiff
        else
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
with `Just` that node context if that node was found and `Nothing`
otherwise. `updater` can then return `Just` an updated node context
(modifying edges is also permitted!) or delete the node by returning
`Nothing`. The updated `graph` is returned.

This is the most powerful building function since all possible per-node
operations are possible (node removal, insertion and updating of context
properties).

The other operations can be implemented in terms of `update` like this:

    remove nodeId graph =
      update nodeId (always Nothing) graph
    insert nodeContext graph =
      update nodeContext.node.id (always (Just nodeContext)) graph
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

        filterInvalidEdges ctx =
          IntDict.filter (\id _ -> id == ctx.node.id || IntDict.member id rep)

        cleanUpEdges ctx =
          ctx
            |> Focus.update incoming (filterInvalidEdges ctx)
            |> Focus.update outgoing (filterInvalidEdges ctx)

        new =
          old
            |> updater
            |> Maybe.map cleanUpEdges

        diff =
          computeEdgeDiff old new
      in
        rep
          |> applyEdgeDiff nodeId diff
          |> IntDict.update nodeId (always new)
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


{-| The [induced subgraph](http://mathworld.wolfram.com/Edge-InducedSubgraph.html)
of a number of node ids.
-}
inducedSubgraph : List NodeId -> Graph n e -> Graph n e
inducedSubgraph nodeIds graph =
  let
    insertContextById nodeId acc =
      case get nodeId graph of
        Just ctx ->
          insert ctx acc
        Nothing ->
          acc
  in
    List.foldl insertContextById empty nodeIds


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
nodeIds =
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
Oftentimes it is even more convenient to use `fromNodeLabelsAndEdgePairs` when
edges are unlabeled anyway and auto incremented node ids are OK.

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


{-| A more convenient version of `fromNodesAndEdges`, when edges are unlabeled
and there are no special requirements on node ids.

`fromNodeLabelsAndEdgePairs labels edges` implicitly assigns node ids according
to the label's index in `labels` and the list of edge pairs is converted to
unlabeled `Edge`s.

    graph = fromNodeLabelsAndEdgePairs ['a', 'b'] [(0, 1)]
-}
fromNodeLabelsAndEdgePairs : List n -> List (NodeId, NodeId) -> Graph n ()
fromNodeLabelsAndEdgePairs labels edges =
  let
    nodes =
      labels
        |> List.foldl
            (\lbl (id, nodes) -> (id + 1, Node id lbl :: nodes))
            (0, [])
        |> snd

    edges' =
      List.map (\(from, to) -> Edge from to ()) edges
  in
    fromNodesAndEdges nodes edges'



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


{-| A fold over all node contexts. The accumulated value is computed lazily,
so that the fold can exit early when the suspended accumulator is not forced.

    hasLoop ctx = IntDict.member ctx.node.id ctx.incoming
    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] [Edge 1 2 "->"]
    -- The graph should not have any loop.
    fold (\ctx acc -> acc || hasLoop ctx) False graph == False
-}
fold : (NodeContext n e -> acc -> acc) -> acc -> Graph n e -> acc
fold f acc graph =
  let
    go acc graph' =
      let
        maybeContext =
          graph'
            |> nodeIdRange
            |> Maybe.map fst
            |> flip Maybe.andThen (\id -> get id graph) -- get should never return Nothing
      in
        case maybeContext of
          Just ctx ->
            go (f ctx acc) (remove ctx.node.id graph')
          Nothing ->
            acc
  in
    go acc graph


{-| Maps each node context to another one. This may change edge and node labels
(including their types), possibly the node ids and also add or remove edges
entirely through modifying the adjacency lists.

The following is a specification for reverseEdges:

    flipEdges ctx = { ctx | incoming <- ctx.outgoing, outgoing <- ctx.incoming }
    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] [Edge 1 2 "->"]
    reverseEdges graph == mapContexts flipEdges graph
-}
mapContexts : (NodeContext n1 e1 -> NodeContext n2 e2) -> Graph n1 e1 -> Graph n2 e2
mapContexts f =
  fold (\ctx -> insert (f ctx)) empty


{-| Maps over node labels, possibly changing their types. Leaves the graph
topology intact.
-}
mapNodes : (n1 -> n2) -> Graph n1 e -> Graph n2 e
mapNodes f =
  fold
    (\ctx ->
      insert
        { ctx
        | node <- { id = ctx.node.id, label = f ctx.node.label }
        })
    empty


{-| Maps over edge labels, possibly chaing their types. Leaves the graph
topology intact.
-}
mapEdges : (e1 -> e2) -> Graph n e1 -> Graph n e2
mapEdges f =
  fold
    (\ctx ->
      insert
        { ctx
        | outgoing <- IntDict.map (\n e -> f e) ctx.outgoing
        , incoming <- IntDict.map (\n e -> f e) ctx.incoming
        })
    empty


{- CHARACTERIZATION -}



{- GRAPH OPS -}


{-| `symmetricClosure edgeMerger graph` is the
[symmetric closure](https://en.wikipedia.org/wiki/Symmetric_closure) of `graph`,
e.g. the undirected equivalent, where for every edge in `graph` there is also
a corresponding reverse edge. This implies that `ctx.incoming` == `ctx.outgoing`
for each node context `ctx`.

`edgeMerger` resolves conflicts for when there are already edges in both
directions, e.g. the graph isn't truly directed. It is guaranteed that
`edgeMerger` will only be called with the smaller node id passed in first
to enforce consitency of merging decisions.

    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] [Edge 1 2 "->"]
    onlyUndirectedEdges ctx =
      ctx.incoming == ctx.outgoing
    merger from to outgoingLabel incomingLabel =
      outgoingLabel -- quite arbitrary, will not be called for the above graph
    fold
      (\ctx acc -> acc && onlyUndirectedEdges ctx)
      True
      (symmetricClosure merger graph)
      == True
-}
symmetricClosure : (NodeId -> NodeId -> e -> e -> e) -> Graph n e -> Graph n e
symmetricClosure edgeMerger =
  -- We could use mapContexts, but this will be more efficient.
  let
    orderedEdgeMerger from to outgoing incoming =
      if from <= to
      then edgeMerger from to outgoing incoming
      else edgeMerger to from incoming outgoing
    updateContext nodeId ctx =
      let
        edges = IntDict.uniteWith (orderedEdgeMerger nodeId) ctx.outgoing ctx.incoming
      in
        { ctx | outgoing <- edges, incoming <- edges }
    in
      Focus.update graphRep (IntDict.map updateContext)


{-| Reverses the direction of every edge in the graph.
-}
reverseEdges : Graph n e -> Graph n e
reverseEdges =
  let
    updateContext nodeId ctx =
      { ctx
      | outgoing <- ctx.incoming
      , incoming <- ctx.outgoing
      }
  in
    Focus.update graphRep (IntDict.map updateContext)


{- TRAVERSALS -}


{-| Selects the next neighbors for the currently visited node in the traversal.
-}
type alias NeighborSelector n e =
  NodeContext n e
  -> List NodeId


{-| A good default for selecting neighbors is to just go along outgoing edges:

    alongOutgoingEdges ctx =
      IntDict.keys (ctx.outgoing)

`dfs`/`bfs` use this as their selecting strategy.
-}
alongOutgoingEdges : NeighborSelector n e
alongOutgoingEdges ctx =
  IntDict.keys (ctx.outgoing)


{-| A less common way for selecting neighbors is to follow incoming edges:

    alongIncomingEdges ctx =
      IntDict.keys (ctx.incoming)
-}
alongIncomingEdges : NeighborSelector n e
alongIncomingEdges ctx =
  IntDict.keys (ctx.incoming)


{-| A generic node visitor just like that in the ordinary `fold` function.
There are combinators that make these usable for both depth-first traversal
(`onDiscovery`, `onFinish`) and breadth-first traversal (`ignorePath`).
-}
type alias SimpleNodeVisitor n e acc =
  NodeContext n e
  -> acc
  -> acc


{- DFS -}


{-| A node visitor specialized for depth-first traversal. Along with the node
context of the currently visited node, the current accumulated value is passed.
The visitor then has the chance to both modify the value at discovery of the
node through the first return value and also provide a finishing
transformation which is called with the value after all children were processed
and the node is about to be finished.

In the cases where you don't need access to the value both at dicovery and at
finish, look into `onDiscovery` and `onFinish`.
-}
type alias DfsNodeVisitor n e acc =
  NodeContext n e
  -> acc
  -> (acc, acc -> acc)


{-| Transform a `SimpleNodeVisitor` into an equivalent `DfsNodeVisitor`, which
will be called upon node discovery. This eases providing `DfsNodeVisitor`s in
the default case:

    dfsPostOrder : Graph n e  -> List (NodeContext n e)
    dfsPostOrder graph =
      dfs (onDiscovery (::)) [] graph
-}
onDiscovery : SimpleNodeVisitor n e acc -> DfsNodeVisitor n e acc
onDiscovery visitor ctx acc =
  (visitor ctx acc, identity)


{-| Transform a `SimpleNodeVisitor` into an equivalent `DfsNodeVisitor`, which
will be called upon node finish. This eases providing `DfsNodeVisitor`s in
the default case:

    dfsPreOrder : Graph n e  -> List (NodeContext n e)
    dfsPreOrder graph =
      dfs (onFinish (::)) [] graph
-}
onFinish : SimpleNodeVisitor n e acc -> DfsNodeVisitor n e acc
onFinish visitor ctx acc =
  (acc, visitor ctx)


{-| The `dfs*` functions are not powerful enough? Go for this beast.

`guidedDfs selectNeighbors visitNode seeds acc graph` will perform a depth-first
traversal on `graph` starting with a stack of `seeds`. The children of each node
will be selected with `selectNeighbors` (see `NeighborSelector`), the visiting
of nodes is handled by `visitNode` (c.f. `DfsNodeVisitor`), folding `acc` over
the graph.

When there are not any more nodes to be visited, the function will return the
accumulated value together with the unvisited rest of `graph`.

    dfsPreOrder graph =
      -- NodeId 1 is just a wild guess here
      guidedDfs alongOutgoingEdges (onDiscovery (::)) [1] [] graph
-}
guidedDfs
  :  NeighborSelector n e
  -> DfsNodeVisitor n e acc
  -> List NodeId
  -> acc
  -> Graph n e
  -> (acc, Graph n e)
guidedDfs selectNeighbors visitNode seeds acc graph =
  let
    go seeds acc graph =
      case seeds of
        [] -> -- We are done with this connected component, so we return acc and the rest of the graph
          (acc, graph)
        next :: seeds' ->
          case get next graph of
            -- This can actually happen since we don't filter for already visited nodes.
            -- That would be an opportunity for time-memory-tradeoff.
            -- E.g. Passing along a set of visited nodeIds.
            Nothing ->
              go seeds' acc graph
            Just ctx ->
              let
                (accAfterDiscovery, finishNode) =
                  visitNode ctx acc

                (accBeforeFinish, graph') =
                  go (selectNeighbors ctx) accAfterDiscovery (remove next graph)

                accAfterFinish =
                  finishNode accBeforeFinish
              in
                go seeds' accAfterFinish graph'
  in
    go seeds acc graph


{-| An off-the-shelf depth-first traversal. It will visit all components of the
graph in no guaranteed order, discovering nodes `alongOutgoingEdges`.
See the docs of `DfsNodeVisitor` on how to supply such a beast. There are also
examples on how to use `dfs`.
-}
dfs : DfsNodeVisitor n e acc -> acc -> Graph n e -> acc
dfs visitNode acc graph =
  guidedDfs alongOutgoingEdges visitNode (nodeIds graph) acc graph |> fst


{-| `dfsTree seed graph` computes a depth-first [spanning tree](https://en.wikipedia.org/wiki/Spanning_tree) of the component
in `graph` starting from `seed` `alongOutgoingEdges`. This function is exemplary for needing to
utilize the whole power of `DfsNodeVisitor`.
-}
dfsTree : NodeId -> Graph n e -> Tree (NodeContext n e)
dfsTree seed graph =
  case dfsForest [seed] graph of
    [] ->
      Tree.empty
    [tree] ->
      tree
    _ ->
      Debug.crash "dfsTree: There can't be more than one DFS tree. This invariant is violated, please report this bug."


{-| `dfsForest seeds graph` computes a depth-first spanning `Forest` of the
components in `graph` spanned by `seeds` `alongOutgoingEdges`.

A traversal over this forest would be equivalent to a depth-first traversal
over the original graph.
-}
dfsForest : List NodeId -> Graph n e -> Forest (NodeContext n e)
dfsForest seeds graph =
  let
    visitNode ctx trees =
      ([], \children -> Tree.inner ctx children :: trees)
  in
    guidedDfs alongOutgoingEdges visitNode seeds [] graph
      |> fst
      |> List.reverse


{- BFS -}


{-| A specialized node visitor for breadth-first traversal. Compared to a
`SimpleNodeVisitor`, the path of contexts from the root to the current
node is passed instead of just the current node's context. Additionally, the
distance from the root is passed as an `Int` (the root has distance 0 and it
holds always that `length path == distance - 1`).

If you don't need the additional information, you can turn a `SimpleNodeVisitor`
into a `BfsNodeVisitor` by calling `ignorePath`.
-}
type alias BfsNodeVisitor n e acc =
  List (NodeContext n e)
  -> Int
  -> acc
  -> acc


{-| Turns a `SimpleNodeVisitor` into a `BfsNodeVisitor` by ignoring the path
and distance parameters.
This is useful for when the visitor should be agnostic of the
traversal (breadth-first or depth-first or even just `fold`).

    bfsLevelOrder : List (NodeContext n e)
    bfsLevelOrder graph =
      graph
        |> bfs (ignorePath (::)) []
        |> List.reverse
-}
ignorePath : SimpleNodeVisitor n e acc -> BfsNodeVisitor n e acc
ignorePath visit path _ acc =
  case path of
    [] ->
      Debug.crash "Graph.ignorePath: No algorithm should ever pass an empty path into this BfsNodeVisitor."
    ctx :: path' ->
      visit ctx acc


{-| The `bfs` function is not powerful enough? Go for this beast.

`guidedBfs selectNeighbors visitNode seeds acc graph` will perform a breadth-first
traversal on `graph` starting with a queue of `seeds`. The children of each node
will be selected with `selectNeighbors` (see `NeighborSelector`), the visiting
of nodes is handled by `visitNode` (c.f. `BfsNodeVisitor`), folding `acc` over
the graph.

When there are not any more nodes to be visited, the function will return the
accumulated value together with the unvisited rest of `graph`.

    bfsLevelOrder graph =
      -- NodeId 1 is just a wild guess here
      guidedBfs alongOutgoingEdges (ignorePath (::)) [1] [] graph
-}
guidedBfs
    :  NeighborSelector n e
    -> BfsNodeVisitor n e acc
    -> List NodeId
    -> acc
    -> Graph n e
    -> (acc, Graph n e)
guidedBfs selectNeighbors visitNode seeds acc graph =
  let
    enqueueMany distance parentPath nodeIds queue =
      nodeIds
        |> List.map (\id -> (id, parentPath, distance))
        |> List.foldl Queue.push queue
    go seeds acc graph =
      case Queue.pop seeds of
        Nothing -> -- We are done with this connected component, so we return acc and the rest of the graph
          (acc, graph)
        Just ((next, parentPath, distance), seeds') ->
          case get next graph of
            -- This can actually happen since we don't filter for already visited nodes.
            -- That would be an opportunity for time-memory-tradeoff.
            -- E.g. Passing along a set of visited nodeIds.
            Nothing ->
              go seeds' acc graph
            Just ctx ->
              let
                path =
                  ctx :: parentPath

                acc' =
                  visitNode path distance acc

                seeds'' =
                  enqueueMany (distance + 1) path (selectNeighbors ctx) seeds'
              in
                go seeds'' acc' (remove next graph)
  in
    go (enqueueMany 0 [] seeds Queue.empty) acc graph


{-| An off-the-shelf breadth-first traversal. It will visit all components of the
graph in no guaranteed order, discovering nodes `alongOutgoingEdges`.
See the docs of `BfsNodeVisitor` on how to supply such a beast. There are also
examples on how to use `bfs`.
-}
bfs : BfsNodeVisitor n e acc -> acc -> Graph n e -> acc
bfs visitNode acc graph =
  let
    (acc', restGraph') =
      guidedBfs alongOutgoingEdges visitNode (nodeIds graph) acc graph
  in
    case nodeIdRange graph of
      Nothing ->
        acc
      Just (id, _) ->
        let
          (acc', restGraph') =
            guidedBfs alongOutgoingEdges visitNode [id] acc graph
        in
          bfs visitNode acc' restGraph'


{-| Computes the height function of a given graph. This is a more general
[topological sort](https://en.wikipedia.org/wiki/Topological_sorting),
where independent nodes are in the same height level (e.g. the same list
index). A valid topological sort is trivially obtained by flattening the
result of this function.

The height function is useful for solving the maximal clique problem for
certain [perfect graphs](https://en.wikipedia.org/wiki/Perfect_graph)
([comparability graphs](https://en.wikipedia.org/wiki/Comparability_graph)).
There is the excellent reference
[Algorithmic Graph Theory and Perfect Graphs](http://dl.acm.org/citation.cfm?id=984029).
-}
heightLevels : Graph n e -> List (List (NodeContext n e))
heightLevels graph =
  let
    sources =
      fold
        (\ctx acc ->
          if IntDict.isEmpty ctx.incoming
          then ctx :: acc
          else acc)
        []
        graph

    countIndegrees =
      fold
        (\ctx ->
          IntDict.insert
          ctx.node.id
          (IntDict.size ctx.incoming))
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
          _ ->
            (nextLevel, indegrees')

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
              [] ->
                Debug.crash "Graph.heightLevels: Reached a branch which is impossible by invariants. Please file a bug report!"
              level :: levels ->
                (source :: level) :: levels
  in
    go sources [] (countIndegrees graph) graph


{-| Computes a
[topological ordering](https://en.wikipedia.org/wiki/Topological_sorting) of the
given graph.
-}
topologicalSort : Graph n e -> List (NodeContext n e)
topologicalSort graph =
  graph
    |> dfsForest (nodeIds graph)
    |> List.reverse
    |> List.concatMap Tree.preOrderList


{-| Decomposes a graph into its strongly connected components. The resulting
list is a topological ordering of the component graph.
-}
stronglyConnectedComponents : Graph n e -> List (Graph n e)
stronglyConnectedComponents graph =
  -- Based on Cormen, using 2 DFS
  let
    timestamps =
      dfs (onFinish (.node >> .id >> (::))) [] graph

    forest =
      dfsForest timestamps (reverseEdges graph) -- We could optimize out reverseEdges

    components =
      List.map (Tree.preOrderList >> List.foldr insert empty >> reverseEdges) forest
  in
    components




{- toString -}

{-| Returns a string representation of the graph in the format of
`Graph.fromNodesAndEdges [<nodes>] [<edges>]`.
-}
toString' : Graph n e -> String
toString' graph =
  let
    nodeList = nodes graph
    edgeList = edges graph
  in
    "Graph.fromNodesAndEdges " ++ toString nodeList ++ " " ++ toString edgeList
