\name{GraphFactor-package}
\alias{GraphFactor-package}
\alias{GraphFactor}
\docType{package}
\title{
\packageTitle{GraphFactor}
}
\description{
\packageDescription{GraphFactor}
}
\details{
GraphFactor is for exploring the qualitative features of a flat file. In a sense, by clustering like values in each variable, the graphFactor function reduces the degrees of freedom of the data. An edge between two clusters indicates that a threshold criteria is met between two variables. Each node (each clustered set of indices in the variable) shares an intersection of indices in an adjacent node, as defined in the parameters by the useR. Nodes within the same variable are never adjacent: Network construction starts from a variable-wise bipartite graph.

Since smaller clusters may lack the criteria for connectedness to any other node in the graph, they won't show in the visualization. It is because of this that sometimes, though each variable receives a color assignment, an event variable's respective node may be absent. So in that sense, events that are statistical leverage points may be apparent by their absense! Further, an interesting artifact of the graphFactor function is that potentially influential observations may be evident because the point's cluster assignments are uncommon with respect to the graph at large: Nodes of an event may appear yet remain disjoint.

Does this tool add value to the analyst's toolbox? Time will tell. Feedback is appreciated. Thank you for using GraphFactor.
}
\author{
\packageAuthor{GraphFactor}

Maintainer: \packageMaintainer{GraphFactor}
}
\references{
Zadeh, L.A. (1964) _Fuzzy Sets_ Electronics Research Laboratory, 
University of California, Berkeley, Report No. 64-44
}

\keyword{ package }

\examples{
# Subset the data.
# Ensure the data is of data.frame class.

library(GraphFactor)
library(igraph)
library(datasets)

state77 <- state.x77[,c(1,2,3,5,7)]
state77 <- data.frame(state77)	#	# Make sure it's data.frame!

seex <- c(4,5,6,7,8)	# Data set indices to inspect with graphFactor.

graphFactor(dat = state77, datName = 'stateX77', showSequence = TRUE,
	buildSVD = FALSE, plotToFile = FALSE, minimumEventThreshold = 3, seqDex = seex)

}