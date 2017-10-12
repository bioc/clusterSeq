% modification on git from copied files
\name{plotCluster}
\alias{plotCluster}

\title{Plots data from clusterings.}

\description{
Given clusterings and expression data, plots representative expression
data for each clustering.
}
\usage{
plotCluster(cluster, cD, sampleSize = 1000)
}

\arguments{
  \item{cluster}{
    A list object defining the clusters, produced by \code{\link{makeClusters}} or \code{\link{makeClustersFF}}.
}
  \item{cD}{
    The data object used to produce the clusters.
}
  \item{sampleSize}{
    The maximum number of genes that will be ploted.
}
}
\details{
Expression data are normalised and rescaled before plotting.
}
\value{
Plotting function.
}

\author{
Thomas J Hardcastle
}

\seealso{
  \code{\link{makeClusters}}
  \code{\link{makeClustersFF}}
}
\examples{
# load in analysed countData object
data(cD.ratThymus, package = "clusterSeq")

# estimate likelihoods of dissimilarity on reduced set
aM <- associatePosteriors(cD.ratThymus[1:1000,])

# make clusters from dissimilarity data
sX <- makeClusters(aM, cD.ratThymus, threshold = 0.5)

# plot first six clusters
par(mfrow = c(2,3))
plotCluster(sX[1:6], cD.ratThymus)

}

\keyword{plot}
