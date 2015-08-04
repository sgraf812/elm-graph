module Test (tests) where


import Test.Graph
import Test.Graph.Tree

import ElmTest.Test exposing (..)


tests : Test
tests =
  suite "elm-graph test suite"
    [ Test.Graph.tests
    , Test.Graph.Tree.tests
    ]
