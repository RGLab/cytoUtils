% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getKeywords.R
\name{keyword,GatingSet,character-method}
\alias{keyword,GatingSet,character-method}
\title{Fetch a list of keywords from a GatingSet}
\usage{
\S4method{keyword}{GatingSet,character}(object, keyword)
}
\arguments{
\item{obj}{GatingSet}

\item{y}{the keywords to be returned}
}
\description{
Return them as a data.table with columns names for the keyword
The FIL keyword is renamed to 'name' for sample name consistency
No error checking at the moment.
}
\examples{
library(flowWorkspace)
dataDir <- system.file("extdata",package="flowWorkspaceData")
suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
keyword(gs, c("TUBE NAME", "EXPERIMENT NAME"))
}
