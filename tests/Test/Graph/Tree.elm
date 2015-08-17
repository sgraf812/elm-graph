module Test.Graph.Tree (tests) where

import String
import Debug
import Graph.Tree as Tree exposing (Tree, Forest)

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)


size : Tree a -> Int
size tree =
  tree
   |> Tree.preOrderList
   |> List.length


tests : Test
tests =
  let
    innerExample1 = Tree.inner 1 [Tree.leaf 2, Tree.leaf 3, Tree.leaf 4]
    innerExample2 = Tree.inner 1 [Tree.leaf 2, Tree.leaf 3, Tree.leaf 4, Tree.empty]

    buildingTests =
      suite "building"
        [ test "empty has no nodes" <| assertEqual 0 (size Tree.empty)
        , test "leaf has one node" <| assertEqual 1 (size (Tree.leaf 42))
        , test "inner with 3 children has 3 nodes" <|
            assertEqual 4 (size innerExample1)
        , test "inner removes empty children" <|
            assertEqual innerExample1 innerExample2
        , test "unfoldTree" <|
            assertEqual
              innerExample1
              (Tree.unfoldTree (\s -> (s, if s == 1 then [2, 3, 4] else [])) 1)
        ]

    queryTests =
      suite "query"
        [ test "empty isEmpty" <| assertEqual True (Tree.isEmpty Tree.empty)
        , test "leaf is not empty" <| assertEqual False (Tree.isEmpty (Tree.leaf 42))
        , test "inner with 2 children is not empty" <|
               assertEqual False (Tree.isEmpty (Tree.leaf ()))
        , test "root of a non-empty tree" <|
            assertEqual (Just (42, [])) (Tree.root (Tree.leaf 42))
        , test "root of an empty tree" <|
            assertEqual Nothing (Tree.root Tree.empty)
        , test "size of a non-empty tree" <|
            assertEqual (Tree.size traversedTree) 7
        , test "height of a non-empty tree" <|
            assertEqual (Tree.height traversedTree) 3
        , test "height of an empty tree" <|
            assertEqual (Tree.height Tree.empty) 0
        ]

    traversedTree =
      Tree.inner 0
        [ Tree.inner 1
            [ Tree.leaf 2, Tree.leaf 3 ]
        , Tree.inner 4
            [ Tree.leaf 5, Tree.leaf 6 ]
        ]

    traversalTests =
      suite "traversal"
        [ test "levelOrderList" <|
            assertEqual
              [0, 1, 4, 2, 3, 5, 6]
              (Tree.levelOrderList traversedTree)
        , test "postOrderList" <|
            assertEqual
              [2, 3, 1, 5, 6, 4, 0]
              (Tree.postOrderList traversedTree)
        , test "preOrderList" <|
            assertEqual
              [0, 1, 2, 3, 4, 5, 6]
              (Tree.preOrderList traversedTree)
        ]

    unitTests =
      suite "unit tests"
        [ buildingTests
        , queryTests
        , traversalTests
        ]
  in
    suite "Tree tests"
      [ unitTests
      ]
