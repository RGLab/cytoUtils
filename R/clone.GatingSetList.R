#'  clone a GatingSetList
#'
#'  A wrapper that invokes clone method for each underlying GatingSet.
#'
#' @param x A \code{GatingSetList}
#' @param ... see \code{\link{clone,GatingSet-method}}
#' @details
#' @return A copy of a given \code{GatingSetList}.
#' @examples
#'   \dontrun{
#'     gslist1<-clone(gslist)
#'
#'   }
#' @export
#' @importFrom flowWorkspace clone GatingSetList
#' @importClassesFrom flowWorkspace GatingSetList
setMethod("clone",c("GatingSetList"),function(x,...){

  x <- lapply(x, clone, ..., level = 1)

  GatingSetList(x)
})