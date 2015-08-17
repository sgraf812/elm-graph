module Test.Graph (tests) where

import String
import Debug
import Set exposing (Set)
import IntDict exposing (IntDict)
import Graph exposing (Graph, NodeId, Node, Edge, NodeContext)
import Focus exposing (Focus)

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)


isJust : Maybe a -> Bool
isJust m =
  case m of
    Just _ -> True
    _ -> False


assertComparing : (a -> b) -> a  -> a -> Assertion
assertComparing f a b =
  assertEqual (f a) (f b)


edgeTriples : Graph n e -> List (NodeId, NodeId, e)
edgeTriples =
  Graph.edges >> List.map (\e -> (e.from, e.to, e.label))


dressUp : Graph String ()
dressUp =
  let
    nodes =
      [ Node 0 "Socks"
      , Node 1 "Undershorts"
      , Node 2 "Pants"
      , Node 3 "Shoes"
      , Node 4 "Watch"
      , Node 5 "Shirt"
      , Node 6 "Belt"
      , Node 7 "Tie"
      , Node 8 "Jacket"
      ]

    e from to =
      Edge from to ()

    edges =
      [ e 0 3 -- socks before shoes
      , e 1 2 -- undershorts before pants
      , e 1 3 -- undershorts before shoes
      , e 2 3 -- pants before shoes
      , e 2 6 -- pants before belt
      , e 5 6 -- shirt before belt
      , e 5 7 -- shirt before tie
      , e 6 8 -- belt before jacket
      , e 7 8 -- tie before jacket
      ]

  in
    Graph.fromNodesAndEdges nodes edges


connectedComponents : Graph Char ()
connectedComponents =
  let
    nodes =
      ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']

    edges =
      [ (0, 1), (1, 2), (1, 4), (1, 5), (2, 3)
      , (2, 6), (3, 2), (3, 7), (4, 0), (4, 5)
      , (5, 6), (6, 5), (6, 7)]
  in
    Graph.fromNodeLabelsAndEdgePairs nodes edges


noNeighbors : Node String -> NodeContext String ()
noNeighbors node =
  NodeContext node IntDict.empty IntDict.empty


tests : Test
tests =
  let
    emptyTests =
      suite "empty"
        [ test "has size 0" <| assertEqual 0 (Graph.size Graph.empty)
        , test "isEmpty" <| assertEqual True (Graph.isEmpty Graph.empty)
        ]

    memberTests =
      suite "member"
        [ test "True" <| assertEqual True (Graph.member 0 dressUp)
        , test "True" <| assertEqual False (Graph.member 99 dressUp)
        ]

    getTests =
      suite "get"
        [ test "id 0, the socks" <|
            assertEqual
              (Just "Socks")
              (dressUp |> Graph.get 0 |> Maybe.map (.node >> .label))
        , test "id 99, Nothing" <| assertEqual Nothing (Graph.get 99 dressUp)
        ]

    nodeIdRangeTests =
      suite "nodeIdRange"
        [ test "dressUp: [0, 8]" <|
            assertEqual
              (Just (0, 8))
              (Graph.nodeIdRange dressUp)
        , test "dressUp - 0: [1, 8]" <|
            assertEqual
              (Just (1, 8))
              (dressUp |> Graph.remove 0 |> Graph.nodeIdRange)
        , test "dressUp - 8: [0, 7]" <|
            assertEqual
              (Just (0, 7))
              (dressUp |> Graph.remove 8 |> Graph.nodeIdRange)
        ]

    listRepTests =
      suite "list conversions"
        [ test "nodeIds" <|
            assertEqual
              [0, 1, 2, 3, 4, 5, 6, 7, 8]
              (dressUp |> Graph.nodeIds)
        , test "nodes" <|
            assertEqual
              [0, 1, 2, 3, 4, 5, 6, 7, 8]
              (dressUp |> Graph.nodes |> List.map .id)
        , test "edges" <|
            assertEqual
              [(0, 3), (1, 2), (1, 3), (2, 3), (2, 6), (5, 6), (5, 7), (6, 8), (7, 8)]
              (dressUp
                 |> Graph.edges
                 |> List.map (\e -> (e.from, e.to))
                 |> List.sort)
        ]

    focusTests =
      suite "foci"
        [ test "get anyNode - empty" <|
            assertEqual
              Nothing
              (Focus.get Graph.anyNode Graph.empty)
        , test "get anyNode - not empty" <|
            assert
              (dressUp |> Focus.get Graph.anyNode |> isJust)
        , test "set anyNode - empty" <|
            assertEqual
              Nothing
              (Graph.empty
                 |> Focus.set Graph.anyNode (Just (noNeighbors (Node 9 "lkj")))
                 |> Graph.get 9)
        ]

    insertTests =
      suite "insert"
        [ test "new node - size" <|
            assertEqual
              (dressUp |> Graph.size |> (+) 1)
              (dressUp |> Graph.insert (noNeighbors (Node 99 "Ring")) |> Graph.size)
        , test "new node - can get it" <|
            assertEqual
              (Just "Ring")
              (dressUp
                 |> Graph.insert (noNeighbors (Node 99 "Ring"))
                 |> Graph.get 99
                 |> Maybe.map (.node >> .label))
        , test "replace node - size" <|
            assertEqual
              (dressUp |> Graph.size)
              (dressUp |> Graph.insert (noNeighbors (Node 0 "Ring")) |> Graph.size)
        , test "replace node - can get it" <|
            assertEqual
              (Just "Ring")
              (dressUp
                 |> Graph.insert (noNeighbors (Node 0 "Ring"))
                 |> Graph.get 0
                 |> Maybe.map (.node >> .label))
        , test "replace node - replaces adjacency" <|
            assertEqual
              (Just True)
              (dressUp
                 |> Graph.insert (noNeighbors (Node 0 "Ring"))
                 |> Graph.get 0
                 |> Maybe.map (\ctx -> IntDict.isEmpty ctx.incoming && IntDict.isEmpty ctx.outgoing))
        ]

    removeTests =
      suite "remove"
        [ test "nonexistent node" <|
            assertEqual
              dressUp
              (dressUp |> Graph.remove 99)
        , test "existing node - size" <|
            assertEqual
              (dressUp |> Graph.size |> flip (-) 1)
              (dressUp |> Graph.remove 0 |> Graph.size)
        , test "existing node - can't get it" <|
            assertEqual
              Nothing
              (dressUp |> Graph.remove 0 |> Graph.get 0)
        ]

    updateTests =
      suite "update"
        [ test "remove outgoing edges" <|
            assertEqual
              (Just True)
              (dressUp
                 |> Graph.update 0 -- "Shorts" has outgoing edges
                      (Maybe.map (Focus.set Graph.outgoing IntDict.empty))
                 |> Graph.get 0
                 |> Maybe.map (.outgoing >> IntDict.isEmpty))
        ]

    inducedSubgraphTests =
      suite "inducedSubgraph"
        [ test "should not have any dangling edges" <|
            assertComparing
              (edgeTriples >> List.sortBy (\(f, t, _) -> (f, t)))
              (Graph.fromNodesAndEdges
                [Node 0 'a', Node 1 'b', Node 4 'e']
                [Edge 0 1 (), Edge 1 4 (), Edge 4 0 ()])
              (Graph.inducedSubgraph [0, 1, 4] connectedComponents)
        ]

    foldTests =
      suite "fold"
        [ test "sum up ids" <|
            assertEqual
              36
              (dressUp
                 |> Graph.fold (\ctx -> (+) ctx.node.id) 0)
        ]

    mapTests =
      suite "map*"
        [ test "mapContexts over id is the id" <|
            assertEqual
              dressUp
              (dressUp |> Graph.mapContexts identity)
        , test "mapNodes over id is the id" <|
            assertEqual
              dressUp
              (dressUp |> Graph.mapNodes identity)
        , test "mapEdges over id is the id" <|
            assertEqual
              dressUp
              (dressUp |> Graph.mapNodes identity)
        -- This should be backed by more tests, but I'm not in the mood for that :/
        ]

    characterizationTests =
      suite "characterization"
        [
        ]

    graphOpsTests =
      suite "Graph ops"
        [ test "symmetricClosure is symmetric" <|
            assert
              (dressUp
                 |> Graph.symmetricClosure (\_ _ e _ -> e)
                 |> Graph.fold (\ctx acc ->
                      ctx.incoming == ctx.outgoing && acc) True)
        , test "reverseEdges" <|
            assertEqual
              (dressUp
                 |> Graph.edges
                 |> List.map (\e -> (e.from, e.to))
                 |> List.sort)
              (dressUp
                 |> Graph.reverseEdges
                 |> Graph.edges
                 |> List.map (\e -> (e.to, e.from))
                 |> List.sort)
        ]

    isValidTopologicalOrderingOf graph ordering =
      ordering
        |> List.foldl
            (\ctx maybeIds ->
              maybeIds `Maybe.andThen` \ids ->
              if List.all (flip IntDict.member ids) (IntDict.keys ctx.incoming)
              then ids |> IntDict.insert ctx.node.id () |> Just
              else Nothing)
            (Just IntDict.empty)
        |> isJust
        |> (&&) (List.length ordering == Graph.size graph)

    topologicalSortTests =
      suite "topologicalSort"
        [ test "topologicalSort" <|
            assert
              (dressUp
                |> Graph.topologicalSort
                |> isValidTopologicalOrderingOf dressUp)
        , test "heightLevels" <|
            assert
              (dressUp
                |> Graph.heightLevels
                |> List.concat
                |> isValidTopologicalOrderingOf dressUp)
        ]


    bfsTests =
      suite "BFS"
        [ test "breadth-first node order" <|
            assertEqual
              [0, 3, 1, 2, 6, 8, 4, 5, 7]
              (dressUp
                |> Graph.bfs (Graph.ignorePath (::)) []
                |> List.map (.node >> .id)
                |> List.reverse)
        ]

    sccTests =
      let
        components =
          Graph.stronglyConnectedComponents connectedComponents

        sg nodeIds =
          connectedComponents
            |> Graph.inducedSubgraph nodeIds
            |> Graph.toString'
      in
        suite "Strongly connected components"
          [ test "The expected SCCs in order" <|
              assertEqual
                [ sg [0, 1, 4] -- "abe"
                , sg [2, 3]    -- "cd"
                , sg [5, 6]    -- "ef"
                , sg [7]       -- "h"
                ]
                (List.map Graph.toString' components)
          ]


    unitTests =
      suite "unit tests"
        [ emptyTests
        , memberTests
        , getTests
        , nodeIdRangeTests
        , listRepTests
        , focusTests
        , insertTests
        , removeTests
        , updateTests
        , inducedSubgraphTests
        , foldTests
        , mapTests
        , characterizationTests
        , graphOpsTests
        , topologicalSortTests
        , bfsTests
        , sccTests
        ]

    examples =
      suite "examples"
        [ test "README - iWantToWearShoes" <|
            assertEqual
              ["Pants", "Undershorts", "Socks", "Shoes"]
              iWantToWearShoes
        , test "insert" <|
            assert insertExample
        , test "fold" <|
            assert foldExample
        , test "mapContexts" <|
            assert mapContextsExample
        ]

  in
    suite "Graph tests"
      [ unitTests
      , examples
      ]


-- EXAMPLE SECTION
-- The code of the more complex examples is exercised here

-- This is from the README
iWantToWearShoes : List String
iWantToWearShoes =
  Graph.guidedDfs
    Graph.alongIncomingEdges            -- which edges to follow
    (Graph.onDiscovery (\ctx list ->    -- append node labels on finish
      ctx.node.label :: list))
    [3 {- "Shoes" NodeId -}]            -- start with the node labelled "Shoes"
    []                                  -- accumulate starting with the empty list
    dressUp                             -- traverse our dressUp graph from above
    |> fst                              -- ignores the untraversed rest of the graph


insertExample : Bool
insertExample =
  let
    graph1 = Graph.fromNodesAndEdges [Node 1 "1"] []
    newNode =
      { node = Node 2 "2"
      , incoming = IntDict.singleton 1 () -- so there will be an edge from 1 to 2
      , outgoing = IntDict.empty
      }
    graph2 = Graph.insert newNode graph1
  in
    Graph.size graph2 == 2


foldExample : Bool
foldExample =
  let
    hasLoop ctx =
      IntDict.member ctx.node.id ctx.incoming
    graph =
      Graph.fromNodesAndEdges [Node 1 "1", Node 2 "2"] [Edge 1 2 "->"]
    -- The graph should not have any loop.
  in
    Graph.fold (\ctx acc -> acc || hasLoop ctx) False graph == False


mapContextsExample : Bool
mapContextsExample =
  let
    flipEdges ctx = { ctx | incoming <- ctx.outgoing, outgoing <- ctx.incoming }
    graph = Graph.fromNodesAndEdges [Node 1 "1", Node 2 "2"] [Edge 1 2 "->"]
  in
    Graph.reverseEdges graph == Graph.mapContexts flipEdges graph


symmetricClosureExample : Bool
symmetricClosureExample =
  let
    graph = Graph.fromNodesAndEdges [Node 1 "1", Node 2 "2"] [Edge 1 2 "->"]
    onlyUndirectedEdges ctx =
      ctx.incoming == ctx.outgoing
    merger from to outgoingLabel incomingLabel =
      outgoingLabel -- quite arbitrary, will not be called for the above graph
  in
    Graph.fold
      (\ctx acc -> acc && onlyUndirectedEdges ctx)
      True
      (Graph.symmetricClosure merger graph)
      == True


onDiscoveryExample : () -- Just let it compile
onDiscoveryExample =
  let
    dfsPostOrder : Graph n e  -> List (NodeContext n e)
    dfsPostOrder graph =
      Graph.dfs (Graph.onDiscovery (::)) [] graph
  in
    dfsPostOrder Graph.empty |> \_ -> ()


onFinishExample : () -- Just let it compile
onFinishExample =
  let
    dfsPreOrder : Graph n e  -> List (NodeContext n e)
    dfsPreOrder graph =
      Graph.dfs (Graph.onFinish (::)) [] graph
  in
    dfsPreOrder Graph.empty |> \_ -> ()


ignorePathExample : () -- Just let it compile
ignorePathExample =
  let
    bfsLevelOrder : Graph n e -> List (NodeContext n e)
    bfsLevelOrder graph =
      graph
        |> Graph.bfs (Graph.ignorePath (::)) []
        |> List.reverse
  in
    bfsLevelOrder Graph.empty |> \_ -> ()
