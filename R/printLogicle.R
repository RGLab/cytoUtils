#' Convenient function to print the parameters of logicle Transformation stored in transformList
#'
#' @param transList a transformList object (typically returned by estimateLogicle) that contains a list if logicle transformation functions
#' @author Phu Van
#' @export
#' @examples
#' data(GvHD)
#' fr <- GvHD[[1]]
#' chnls <- colnames(fr)[3:7]
#' trans <- estimateLogicle(fr, chnls)
#' print.logicle(trans)
print.logicle <- function(transList){
  for (trans in transList@transforms){
    chnl <- trans@input
    env <- environment(trans@f)
    t <- as.numeric(format(as.vector(env[["t"]]), digits = 2))
    m <- as.numeric(format(as.vector(env[["m"]]), digits = 2))
    a <- as.numeric(format(as.vector(env[["a"]]), digits = 2))
    w <- as.numeric(format(as.vector(env[["w"]]), digits = 2))
    print(paste0(chnl, ": T=", t, " M=", m, " A=", a, " W=", w))
  }
}
