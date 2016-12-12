\name{makeClusters}
\alias{makeClusters}

\title{Creates clusters from a co-expression minimal linkage data.frame.}

\description{
This function uses minimal linkage data to perform rapid clustering by
singleton agglomeration (i.e., a gene will always cluster with its
nearest neighbours provided the distance to those neighbours does not
exceed some threshold). For alternative (but slower) clustering options,
see the \code{\link{makeClustersFF}} function.
}
\usage{
makeClusters(aM, cD, threshold = 0.5)
}

\arguments{
  \item{aM}{
    A data frame constructed by \code{\link{associatePosteriors}} or
    \code{\link{kCluster}}, defining for each gene the nearest
    neighbour of higher row index and the dissimilarity with that neighbour.
}
  \item{cD}{
    The data given as input to \code{\link{associatePosteriors}} or
    \code{\link{kCluster}} that produced 'aM'.
}
  \item{threshold}{
    A threshold on the maximum dissimilarity at which two genes can
    cluster. Defaults to 0.5.
}
}

\value{
An IntegerList object, each member of whom defines a cluster of co-expressed
genes. The object is ordered decreasingly by the size of each cluster.
}

\author{
Thomas J Hardcastle
}
\seealso{
  \code{\link{makeClustersFF}}
  \code{\link{kCluster}}
  \code{\link{associatePosteriors}}
}
\examples{
#Load in the processed data of observed read counts at each gene for each sample. 
data(ratThymus, package = "clusterSeq")

# Library scaling factors are acquired here using the getLibsizes
# function from the baySeq package.
libsizes <- getLibsizes(data = ratThymus)

# Adjust the data to remove zeros and rescale by the library scaling
# factors. Convert to log scale.
ratThymus[ratThymus == 0] <- 1
normRT <- log2(t(t(ratThymus / libsizes)) * mean(libsizes))

# run kCluster on reduced set.
normRT <- normRT[1:1000,]
kClust <- kCluster(normRT)

# make the clusters from these data.
mkClust <- makeClusters(kClust, normRT, threshold = 1)

}

\keyword{manip}
