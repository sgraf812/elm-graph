module Graph
    ( NodeId, Node, Edge, Adjacency, NodeContext, Decomposition, Graph
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
    , dfsList, dfs, guidedDfs
    , bfsList, bfs, guidedBfs
    , heightLevels, topologicalSort

    , toString'
    ) where
    

import IntDict as IntDict exposing (IntDict)
import Maybe as Maybe exposing (Maybe)
import Lazy as Lazy exposing (Lazy)
import Focus as Focus exposing (Focus, (=>))
import Queue as Queue exposing (Queue)
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
    Graph n e
    -> NodeContext n e
    -> List (NodeId, nodeTag)


type alias ComponentSelector n e acc =
    Graph n e
    -> Maybe (NodeId, (acc -> acc))


type alias NodeVisitor n e acc =
    NodeContext n e
    -> Lazy acc
    -> acc


type alias NodeVisitorWithDepth n e acc =
    NodeContext n e
    -> Int
    -> Lazy acc
    -> acc


alongOutgoingEdges : NeighborSelector n e
alongOutgoingEdges graph ctx =
    ctx.outgoing
        |> IntDict.keys


traverseAllComponents : ComponentSelector n e acc
traverseAllComponents graph =
    graph
        |> Focus.get anyNode
        |> Maybe.map (\root -> (root.node.id, identity))


traverseComponentOf : NodeId -> ComponentSelector n e acc
traverseComponentOf nodeId graph =
    graph
      |> Focus.get (nodeById nodeId)
      |> Maybe.map (\root -> (root.node.id, identity))


traverseAnyOneComponent : Graph n e -> ComponentSelector n e acc
traverseAnyOneComponent originalGraph currentGraph =
    if originalGraph /= currentGraph
    then Nothing
    else originalGraph
        |> Focus.get anyNode
        |> Maybe.map (\root -> (root.node.id, identity))


accumulateNodesInList : NodeVisitor n e (List (NodeContext n e))
accumulateNodesInList ctx rest =
    ctx :: Lazy.force rest


ignoreDepth : NodeVisitor n e -> NodeVisitorWithDepth n e
ignoreDepth visitNode ctx depth acc =
    visitNode ctx acc


-- Not Exported.
-- Abstracting over Stack and Queue without HKTs. We monomorphise NodeId and abstract over the container. 
-- XIFO is a wildcard match for FIFO (Queue) and LIFO (Stack)
type alias NodeIdXIFOImpl a =
    { empty : a
    , push : (NodeId, Int) -> a -> a
    , pop : a -> Maybe ((NodeId, Int), a)
    }


stackImpl : NodeIdXIFOImpl (List (NodeId, Int))
stackImpl =
    { empty = []
    , push = (::)
    , pop xs =
        case xs of
            [] -> Nothing
            x :: xs' -> (x, xs')
    }


queueImpl : NodeIdXIFOImpl (Queue (NodeId, Int))
queueImpl =
    { empty = Queue.empty
    , push = Queue.push
    , pop = Queue.pop
    }


guidedTraversal
    :  NodeIdXIFOImpl a
    -> NeighborSelector n e
    -> ComponentSelector n e acc
    -> NodeVisitorWithDepth n e acc
    -> acc
    -> Graph n e
    -> acc
guidedTraversal xifoImpl selectNeighbors nextComponent visitNode acc graph =
    -- It may be worthwhile to convert this all to CPS, but who knows
    let pushMany nodeIds xifo =
            List.foldl xifoImpl.push xifo nodeIds
        component graph =
            case nextComponent graph of
                Nothing -> -- No more roots => the traversal terminates
                    acc
                Just (root, finish) ->
                    graph
                      |> traverse (xifoImpl.empty |> xifoImpl.push root)
                      |> finish
        traverse xifo graph = -- This will visit the rest of the graph only as long as visitNode forces it
            case xifoImpl.pop xifo of
                Nothing -> -- We are done with this connected component
                    component graph
                Just ((next, depth), xifo') ->
                    case get next graph of
                        -- This can actually happen since we don't filter the xifo for already visited nodes.
                        Nothing -> traverse xifo' (remove next graph)
                        Just ctx ->
                            visitNode ctx depth <| Lazy.lazy <| \_ ->
                                let neighbors =
                                      ctx
                                        |> selectNeighbors graph
                                        |> List.map (\nodeId -> (nodeId, depth + 1))
                                in traverse (pushMany neighbors xifo') (remove next graph)
    in component graph


guidedDfs
    :  NeighborSelector n e
    -> ComponentSelector n e acc
    -> NodeVisitorWithDepth n e acc
    -> acc
    -> Graph n e
    -> acc
guidedDfs =
    guidedTraversal stackImpl


guidedBfs
    :  NeighborSelector n e
    -> ComponentSelector n e acc
    -> NodeVisitorWithDepth n e acc
    -> acc
    -> Graph n e
    -> acc
guidedBfs =
    guidedTraversal queueImpl


dfs : NodeVisitor n e acc -> acc -> Graph n e -> acc
dfs visitNode acc graph =
    guidedDfs alongOutgoingEdges (traverseAnyOneComponent graph) (ignoreDepth visitNode) acc graph


bfs : NodeVisitor n e acc -> acc -> Graph n e -> acc
bfs visitNode acc graph = 
    guidedBfs alongOutgoingEdges (traverseAnyOneComponent graph) (ignoreDepth visitNode) acc graph


dfsList : Graph n e -> List (NodeContext n e)
dfsList =
    dfs accumulateNodesInList []


bfsList : Graph n e -> List (NodeContext n e)
bfsList graph =
    bfs accumulateNodesInList [] graph


heightLevels : Graph n e -> List (List (NodeContext n e))
heightLevels graph =
    let isSource graph' nodeId =
            graph'
             |> get nodeId
             |> Maybe.map (.incoming >> IntDict.isEmpty)
             |> Maybe.withDefault False 
        startFromSources graph' =
            graph'
             |> nodeIds
             |> List.filter (isSource graph')
             |> List.head
             |> Maybe.map (\sourceId -> (sourceId, identity))
        hasOnlyOneIncomingEdge graph' nodeId =
            graph'
             |> get nodeId
             |> Maybe.map (.incoming >> IntDict.size >> (==) 1)
             |> Maybe.withDefault False
        selectNeighbors graph' ctx depth =
            ctx.outgoing
             |> IntDict.keys
             |> List.filter (hasOnlyOneIncomingEdge graph')
             |> List.map (\id -> (id, depth + 1))
        visitNode ctx depth rest =
            let (levels, minDepth) = Lazy.force rest
            in case levels of
                [] -> ([[ctx]], depth)
                level :: lowerLevels ->
                    if  | depth == minDepth -> ((ctx :: level) :: lowerLevels, minDepth)
                        | depth + 1 == minDepth -> ([ctx] :: levels, minDepth - 1)
                        | otherwise -> Debug.crash "Graph.heightLevels: Reached a branch which is impossible by invariants. Please file a bug report!"
    in guidedBfs selectNeighbors startFromSources visitNode [] graph


topologicalSort : Graph n e -> List (NodeContext n e)
topologicalSort =
    heightLevels >> List.concat


{- toString -}

toString' : Graph n e -> String
toString' graph =
    let nodeList = nodes graph
        edgeList = edges graph
    in
        "Graph.fromNodesAndEdges " ++ toString nodeList ++ " " ++ toString edgeList
