#' Exact MFI from populations(or nodes) for all the markers
#'
#' It calculates the MFI for each marker.
#'
#' @param x a GatingSet or GatingHierarchy
#' @param ... other arguments
#'      only.leaf default is TRUE, which only get MFI for the leaf nodes
#'      ... arguments passed to \link{getNodes} method.
#' @return a data.table that contains MFI values for each marker per column along with 'pop' column and 'sample' column (when used on a 'GatingSet')
#' @import flowWorkspace
#' @import flowCore
#' @import ncdfFlow
#' @import data.table
#' @export
#' @rdname getMFI
#' @examples
#' \dontrun{
#' library(cytoUtils)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#' # get MFI of each markers for all the leaf nodes
#' dt <- getMFI(gs)
#'
#' # remove the restriction of leaf node and cutomize the node path length
#' getMFI(gs, only.leaf = FALSE, path = "auto")
#' }
getMFI <- function(x, ...)UseMethod("getMFI")

#' @export
#' @rdname getMFI
getMFI.GatingSetList <- function(x, ...){
  getMFI.GatingSet(x, ...)
}

#' @export
#' @rdname getMFI
getMFI.GatingSet <- function(x, ...){
  res <-  lapply(x, function(gh){
    getMFI(gh, ...)

  })
  rbindlist(res, idcol = "sample")
}

#' @export
#' @rdname getMFI
#' @importFrom  matrixStats colMedians
getMFI.GatingHierarchy <- function(x, only.leaf = TRUE, ...){
  gh <- x
  fr <- getData(gh, use.exprs = FALSE)
  pd <- pData(parameters(fr))
  pd <- data.table(pd)
  pd <- pd[!is.na(desc), ]
  chnls  <- pd[, name]
  markers <- pd[, desc]
  if(only.leaf)
    nodes <- getLeafNode(gh, ...)
  else
    nodes <- getNodes(gh, ...)
  res <- sapply(nodes, function(node){

    fr <- getData(gh, y = node, j = chnls)
    MFI <- colMedians(exprs(fr))
    names(MFI) <- markers
    MFI <- as.data.table(t(MFI))
  }, simplify = FALSE)
  rbindlist(res, idcol = "pop")

}
