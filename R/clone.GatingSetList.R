#'  clone a GatingSetList
#'
#'  A wrapper that invokes clone method for each underlying GatingSet.
#'
#' @param x A \code{GatingSetList}
#' @param ... see \code{\link{gs_clone}}
#' @details
#' @return A copy of a given \code{GatingSetList}.
#' @examples
#'   \dontrun{
#'     gslist1<-gslist_clone(gslist)
#'
#'   }
#' @import flowWorkspace
#' @import flowCore
#' @import ncdfFlow
#' @import data.table
#' @export
#' @importClassesFrom flowWorkspace GatingSetList
gslist_clone <- function(x,...){

  x <- lapply(x, gs_clone, ..., level = 1)

  GatingSetList(x)
}