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

#' fast way of getting channel names from fcs file by only reading header
#'
#' This is a convenient wrapper around \link{read.FCSheader} and \code{flowCore:::readFCSgetPar}.
#'
#' @param fileName \code{character}  fcs file name(path)
#' @return a \code{character} vector channels/parameters used in this FCS
#' @export
#' @examples
#'
#' readFCSPar(system.file("extdata/0877408774.B08", package = "flowCore"))
readFCSPar <- function(fileName){
  txt <- flowCore:::read.FCSheader(fileName)[[1]]
  nChannels <- as.integer(txt[["$PAR"]])
  channelNames <- unlist(lapply(1:nChannels,function(i)flowCore:::readFCSgetPar(txt,paste("$P",i,"N",sep=""))))
  unname(channelNames)

}

