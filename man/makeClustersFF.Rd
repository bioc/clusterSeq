\name{makeClustersFF}
\alias{makeClustersFF}

\title{
  Creates clusters from a file containing a full dissimilarity matrix.
}
\description{
  This function uses the complete pairwise dissimilarity scores to
  construct a hierarchical clustering of the genes.
}
\usage{
makeClustersFF(file, method = "complete", cut.height = 5)
}

\arguments{
  \item{file}{
    Filename containing the dissimilarity data.
}
  \item{method}{
    Method to use in \code{\link{hclust}}.
}
  \item{cut.height}{
    Cut height to use in \code{\link{hclust}}.
}
}
\value{
An IntegerList object containing the clusters derived from a cut hierarchical clustering.
}
\author{
Thomas J Hardcastle
}
\seealso{
  \code{\link{makeClusters}}
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

# run kCluster on reduced set. For speed, one thousand bootstraps are
# used, but higher values should be used in real analyses.
# Write full dissimilarity matrix to file "kclust.gz"
normRT <- normRT[1:1000,]
kClust <- kCluster(normRT, B = 1000, matrixFile = "kclust.gz")


# make the clusters from these data.
mkClustR <- makeClustersFF("kclust.gz")
}


\keyword{manip}
