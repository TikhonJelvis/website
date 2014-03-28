ghead :: Graph -> Node
ghead graph | Graph.isEmpty graph              = error "Empty graph!"
ghead (matchAny -> (Context _ node _, _graph)) = node
