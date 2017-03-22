getLeafNode <- function(gs, ...){
  nodes <- getNodes(gs, ...)
  isTerminal <- sapply(nodes,function(thisNode){
    length(getChildren(gs,thisNode))==0
  })
  nodes[isTerminal]
}

#' @import flowCore
getfluorescentChannels <- function(fr){

  pd <- pData(parameters(fr))
  pd <- pd[!is.na(pd[["desc"]]),]
  pd[["name"]]
}
