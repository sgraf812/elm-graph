module Graph
    ( NodeId
    , Node
    , Edge
    , Adjacency
    , NodeContext
    , Decomposition
    , Graph

    , empty
    , isEmpty
    , insert
    , focus
    , focusAny
    , nodeRange

    , nodeIds
    , nodes
    , edges
    , fromNodesAndEdges

    , fold
    , mapContexts
    , mapNodes
    , mapEdges
    
    , toString'
    , g
    ) where
    

import IntDict as IntDict exposing (IntDict)
import Maybe as Maybe exposing (Maybe)
import Lazy as Lazy exposing (Lazy)
import Focus as Focus exposing (Focus, (=>))
    
type alias NodeId = Int


type alias Node n =
    { id : NodeId
    , label : n
    }

id : Focus { record | id : field } field
id = Focus.create .id (\update record -> { record | id <- update record.id }) 

     
label : Focus { record | label : field } field
label = Focus.create .label (\update record -> { record | label <- update record.label }) 

     
type alias Edge e =
    { from : NodeId
    , to : NodeId
    , label : e 
    }


from : Focus { record | from : field } field
from = Focus.create .from (\update record -> { record | from <- update record.from }) 

       
to : Focus { record | to : field } field
to = Focus.create .to (\update record -> { record | to <- update record.to }) 

       
type alias Adjacency e = IntDict e


type alias NodeContext n e =
    { node : Node n
    , incoming : Adjacency e
    , outgoing : Adjacency e
    }

-- Lenses for NodeContext
node : Focus { record | node : field } field
node = Focus.create .node (\update record -> { record | node <- update record.node }) 


incoming : Focus { record | incoming : field } field
incoming = Focus.create .incoming (\update record -> { record | incoming <- update record.incoming }) 
          

outgoing : Focus { record | outgoing : field } field
outgoing = Focus.create .outgoing (\update record -> { record | outgoing <- update record.outgoing }) 

           
type alias Decomposition n e =
    { focused : NodeContext n e
    , rest : Lazy (Graph n e)
    }


-- We will only have the Patricia trie based DynGraph implementation for simplicity.
-- Also, there is no real practical reason to separate that or to allow other implementations
-- which would justify the complexity.

type alias GraphRep n e = IntDict (NodeContext n e)

    
type Graph n e = Graph (GraphRep n e)


unGraph : Graph n e -> GraphRep n e
unGraph graph = case graph of Graph rep -> rep


graphRep : Focus (Graph n e) (GraphRep n e)
graphRep = Focus.create unGraph (\update -> unGraph >> update >> Graph)


empty : Graph n e
empty = Graph IntDict.empty


isEmpty : Graph n e -> Bool
isEmpty graph = IntDict.isEmpty (unGraph graph)


lookup : NodeId -> Focus (IntDict v) (Maybe v)
lookup id = Focus.create (IntDict.get id) (\update -> IntDict.update id update)


updateAdjacency : Bool -> NodeContext n e -> GraphRep n e -> GraphRep n e
updateAdjacency shallInsert updateContext rep =                                
    let updateNeighbors edgeFocus nodeId edge =
            IntDict.update nodeId (Maybe.map (Focus.set edgeFocus (if shallInsert then Just edge else Nothing)))
        -- This essentially iterates over the keys of updateContext.outgoing to delete the corresponding incoming edges
        rep1 = IntDict.foldl (updateNeighbors (outgoing => lookup updateContext.node.id)) rep updateContext.outgoing
        rep2 = IntDict.foldl (updateNeighbors (incoming => lookup updateContext.node.id)) rep1 updateContext.incoming
    in if shallInsert
       then IntDict.insert updateContext.node.id updateContext rep2
       else IntDict.remove updateContext.node.id rep2


insert : NodeContext n e -> Graph n e -> Graph n e
insert nodeContext graph =
    -- We remove the node with the same id from graph, if present
    let graph' = Maybe.withDefault graph (Maybe.map (.rest >> Lazy.force) (focus nodeContext.node.id graph))
    in graph' |> Focus.update graphRep (updateAdjacency True nodeContext)


remove : NodeContext n e -> Graph n e -> Graph n e
remove nodeContext = Focus.update graphRep (updateAdjacency False nodeContext)


focus : NodeId -> Graph n e -> Maybe (Decomposition n e)
focus node graph =
    let decompose nodeContext =
            { focused = nodeContext
            , rest = Lazy.lazy (\_ -> remove nodeContext graph)
            }
    in graph |> Focus.get (nodeById node) |> Maybe.map decompose 


nodeById : NodeId -> Focus (Graph n e) (Maybe (NodeContext n e))
nodeById node =
    let get = Focus.get (graphRep => lookup node)
        update upd graph =
            let old = get graph
                new = upd old
            in case (old, new) of
                 (v, v) -> graph
                 (Nothing, Just ctx) -> insert ctx graph
                 (Just ctx, Nothing) -> remove ctx graph
                 (Just ctx1, Just ctx2) -> graph |> remove ctx1 |> insert ctx2
    in Focus.create get update 
          
--anyNode_ : Focus (Graph n e) (Maybe (NodeContext n e))
--anyNode_ =
 --   Focus.create 

focusAny : Graph n e -> Maybe (Decomposition n e)
focusAny graph =
    case graph of
        Graph rep ->
            IntDict.findMin rep `Maybe.andThen` \(nodeId, _) ->
            focus nodeId graph


nodeRange : Graph n e -> Maybe (NodeId, NodeId)
nodeRange graph =
    case graph of
        Graph rep ->
            IntDict.findMin rep `Maybe.andThen` \(min, _) ->
            IntDict.findMax rep `Maybe.andThen` \(max, _) ->
            Just (min, max)
            

member : NodeId -> Graph n e -> Bool
member id graph =
    case graph of
        Graph rep -> IntDict.member id rep
            

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
        

-- TRANSFORMS


fold : (NodeContext n e -> acc -> acc) -> acc -> Graph n e -> acc
fold f acc graph =
    case focusAny graph of
        Just decomp -> fold f (f decomp.focused acc) (Lazy.force decomp.rest) 
        Nothing -> acc


mapContexts : (NodeContext n1 e1 -> NodeContext n2 e2) -> Graph n1 e1 -> Graph n2 e2
mapContexts f = fold (\ctx -> insert (f ctx)) empty


mapNodes : (n1 -> n2) -> Graph n1 e -> Graph n2 e
mapNodes f = fold (\ctx -> insert { ctx | node <- { id = ctx.node.id, label = f ctx.node.label } }) empty

             
mapEdges : (e1 -> e2) -> Graph n e1 -> Graph n e2
mapEdges f =
    fold (\ctx -> insert
                  { ctx
                  | outgoing <- IntDict.map (\n e -> f e) ctx.outgoing
                  , incoming <- IntDict.map (\n e -> f e) ctx.incoming })
         empty


toString' : Graph n e -> String
toString' graph =
    let nodeList = nodes graph
        edgeList = edges graph
    in
        "Graph.fromNodesAndEdges " ++ toString nodeList ++ " " ++ toString edgeList


g : Graph () String
g = fromNodesAndEdges [ { id = 1, label = () }, { id = 2, label = () } ] [ { from = 2, to = 1, label = "arrow" } ]
