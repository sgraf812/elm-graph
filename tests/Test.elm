module Test exposing (tests)


import Test.Graph
import Test.Graph.Tree

import ElmTest exposing (..)


tests : Test
tests =
  suite "elm-graph test suite"
    [ Test.Graph.tests
    , Test.Graph.Tree.tests
    ]
