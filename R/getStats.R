#' Exact MFI from populations(or nodes) for all the markers
#'
#' It calculates the MFI for each marker.
#'
#' @param x a GatingSet or GatingHierarchy
#' @param ... other arguments
#'      fun a function used to compute population stats. It expects 'fr' argument as a flowFrame and a named vector as return value.
#'          default is \code{pop.count} function.
#'      nodes the character vector specifies the populations of interest. default is all available nodes
#'      ... arguments passed to \link{getNodes} method.
#' @return a data.table that contains MFI values for each marker per column along with 'pop' column and 'sample' column (when used on a 'GatingSet')
#' @import flowWorkspace
#' @import flowCore
#' @import ncdfFlow
#' @import data.table
#' @export
#' @rdname getStats
#' @examples
#' \dontrun{
#' library(flowWorkspace)
#' library(cytoUtils)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#'
#' # get stats all nodes
#' dt <- getStats(gs) #default is "count"
#'
#' # change the stats to another build-in function and specify nodes
#' getStats(gs, fun = pop.MFI, nodes = c("CD4", "CD8"))
#'
#' # supply user-defined stats fun
#' pop.quantiles <- function(fr){
#'    chnls <- colnames(fr)
#'    res <- matrixStats::colQuantiles(exprs(fr), probs = 0.75)
#'    names(res) <- chnls
#'    res
#'    }
#' getStats(gs, fun = pop.quantiles, nodes = c("CD4", "CD8"))
#' }
getStats <- function(x, ...)UseMethod("getStats")

#' @export
#' @rdname getStats
getStats.GatingSetList <- function(x, ...){
  getStats.GatingSet(x, ...)
}

#' @export
#' @rdname getStats
getStats.GatingSet <- function(x, ...){
  res <-  lapply(x, function(gh){
    getStats(gh, ...)

  })
  rbindlist(res, idcol = "sample")
}

#' @export
#' @rdname getStats
getStats.GatingHierarchy <- function(x, fun = pop.count, nodes = NULL, ...){
  gh <- x
  if(is.null(nodes))
    nodes <- getNodes(gh, ...)
  res <- sapply(nodes, function(node){

    fr <- getData(gh, y = node)
    res <- fun(fr)
    as.data.table(t(res))
  }, simplify = FALSE)
  rbindlist(res, idcol = "pop")

}

#' built-in stats functions.
#'
#' pop.count returns the count.
#' pop.MFI computes and returns the median fluorescence intensity for each marker.
#' They are typically used as the arguments passed to \code{getStats} method to perform the sample-wise population stats calculations.
#'
#' @param fr a flowFrame represents a gated population
#' @return a named numeric vector
#'
#' @rdname stats.fun
#' @export
pop.count <- function(fr){
  res <- nrow(fr)
  names(res) <- "count"
  res
}

#' @rdname stats.fun
#' @export
#' @importFrom  matrixStats colMedians
pop.MFI <- function(fr){
  pd <- pData(parameters(fr))
  pd <- data.table(pd)
  pd <- pd[!is.na(desc), ]
  chnls  <- pd[, name]
  markers <- pd[, desc]

  res <- colMedians(exprs(fr)[, chnls, drop = FALSE])
  names(res) <- markers
  res
}
