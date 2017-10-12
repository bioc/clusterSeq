% modification on git from copied files
\name{associatePosteriors}
\alias{associatePosteriors}
\title{Associates posterior likelihood to generate co-expression
  dissimilarities between genes}

\description{
  This function aims to find pairwise dissimilarities between genes. It
  does this by comparing the posterior likelihoods of patterns of
  differential expression for each gene, and estimating the likelihood
  that the two genes are not equivalently expressed.
}
\usage{
associatePosteriors(cD, maxsize = 250000, matrixFile = NULL)
}

\arguments{
  \item{cD}{
    A \code{\link[baySeq]{countData}} object containing posterior
  likelihoods of differential expression for each gene.
}
  \item{maxsize}{
    The maximum size (in MB) to use when partitioning the data.
}
  \item{matrixFile}{
    If given, a file to write the complete (gzipped) matrix of pairwise
  distances between genes. Defaults to NULL.
}
}

\details{
  In comparing two genes, we find all patterns of expression considered
  in the '@groups' slot of the 'cD' (\code{\link[baySeq]{countData}})
  object for which the expression of the two genes can be considered
  monotonic. We then subtract the sum the posterior likelihods of these
  patterns of expression from 1 to define a likelihood of dissimilarity
  between the two genes.
}
\value{
A data.frame which for each gene defines its nearest neighbour of higher
  row index and the dissimilarity with that neighbour.
}
\author{
Thomas J. Hardcastle
}

\seealso{
  \code{\link{makeClusters}}
  \code{\link{makeClustersFF}}
  \code{\link{kCluster}}
}
\examples{
# load in analysed countData (baySeq package) object
library(baySeq)
data(cD.ratThymus, package = "clusterSeq")

# estimate likelihoods of dissimilarity on reduced set
aM <- associatePosteriors(cD.ratThymus[1:1000,])
}

\keyword{manip}

