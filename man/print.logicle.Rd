% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/printLogicle.R
\name{print.logicle}
\alias{print.logicle}
\title{Convenient function to print the parameters of logicle Transformation stored in transformList}
\usage{
print.logicle(transList)
}
\arguments{
\item{transList}{a transformList object (typically returned by estimateLogicle) that contains a list if logicle transformation functions}
}
\description{
Convenient function to print the parameters of logicle Transformation stored in transformList
}
\examples{
data(GvHD)
fr <- GvHD[[1]]
chnls <- colnames(fr)[3:7]
trans <- estimateLogicle(fr, chnls)
print.logicle(trans)
}
\author{
Phu Van
}
