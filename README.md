# Graph
An neat graph library for Elm.

Got confused about what to wear when putting on shoes? This will help you out:

```
dressUp : Graph String () -- node labels are strings, edge labels are empty
dressUp =
  let
    nodes =
      [ Node 0 "Shorts"
      , Node 1 "Socks"
      , Node 2 "Pants"
      , Node 3 "Undershirt"
      , Node 4 "Sweater"
      , Node 5 "Coat"
      , Node 6 "Shoes"
      ]

    e from to =
      Edge from to ()

    edges =
      [ e 0 2 -- shorts before pants
      , e 1 6 -- socks before shoes
      , e 2 5 -- pants before coat
      , e 2 6 -- pants before shoes
      , e 3 4 -- underhirt before sweater
      , e 4 5 -- sweater before coat
      ]

  in
    Graph.fromNodesAndEdges nodes edges


iWantToWearShoes: List String
iWantToWearShoes =
  Graph.guidedDfs
    Graph.alongIncomingEdges            -- which edges to follow
    (Graph.onDiscovery (\ctx list ->    -- append node labels on finish
      ctx.node.label :: list))
    [6 {- "Shoes" NodeId -}]            -- start with the node labelled "Shoes"
    []                                  -- accumulate starting with the empty list
    dressUp                             -- traverse our dressUp graph from above
    |> fst                              -- ignores the untraversed rest of the graph


iWantToWearShoes == ["Shorts", "Pants", "Socks", "Shoes"]
```

So better wear shorts, pants and socks with you shoes. Somewhat opinionated.

# Credits

I was inspired by Martin Erwig's original idea realized in the
[functional graph library](http://hackage.haskell.org/package/fgl-5.5.2.1), but
I also tried to keep it as simple as possible, bringing the neatness of Elm to
graph libraries.
