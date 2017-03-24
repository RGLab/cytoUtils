#' Fetch a list of keywords from a GatingSet
#'
#' Return them as a data.table with columns names for the keyword
#' The FIL keyword is renamed to 'name' for sample name consistency
#' No error checking at the moment.
#'
#' @export
#' @importFrom flowWorkspace getKeywords
#' @param obj GatingSet
#' @param y the keywords to be returned
#' @examples
#' library(flowWorkspace)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#' getKeywords(gs, c("TUBE NAME", "EXPERIMENT NAME"))
setMethod("getKeywords",c("GatingSet","character"),function(obj,y){
      gs <- obj
      kv <- y
  if(!"$FIL" %in% kv)
    kv <- c("$FIL", kv)
  r<-as.data.frame(do.call(cbind,lapply(kv,function(k){
    keyword(gs,k)[1]
  })))
  data.table::setnames(r,"$FIL","name")
  r
})
