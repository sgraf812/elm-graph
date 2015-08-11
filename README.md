# Graph [![Build Status](https://travis-ci.org/sgraf812/elm-graph.svg)](https://travis-ci.org/sgraf812/elm-graph)
An neat graph library for Elm.

Got confused about what to wear when putting on shoes? This will help you out:

```
dressUp : Graph String () -- node labels are strings, edge labels are empty
dressUp =
  let
    nodes =
      [ Node 0 "Socks"
      , Node 1 "Undershirt"
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


iWantToWearShoes: List String
iWantToWearShoes =
  Graph.guidedDfs
    Graph.alongIncomingEdges            -- which edges to follow
    (Graph.onDiscovery (\ctx list ->    -- append node labels on discovery
      ctx.node.label :: list))
    [3 {- "Shoes" NodeId -}]            -- start with the node labelled "Shoes"
    []                                  -- accumulate starting with the empty list
    dressUp                             -- traverse our dressUp graph from above
    |> fst                              -- ignores the untraversed rest of the graph


iWantToWearShoes == ["Pants", "Undershorts", "Socks", "Shoes"]
```

So better wear pants, undershorts, pants and socks with your shoes.
(In case you wonder: There is also a `topologicalSort` function which can compute
valid linear orderings)

# Credits

I was inspired by Martin Erwig's original idea realized in the
[functional graph library](http://hackage.haskell.org/package/fgl-5.5.2.1), but
I also tried to keep it as simple as possible, bringing the neatness of Elm to
graph libraries.
