#from https://www.mm218.dev/posts/2022-12-01-sf-in-packages/
delayedAssign("world", local({
  requireNamespace("sf", quietly = TRUE)
  historicalBorders:::world
}))
