% modification on git from copied files
\name{kCluster}
\alias{kCluster}

\title{Constructs co-expression dissimilarities from k-means analyses.}

\description{
  This function aims to find pairwise distances between genes. It
  does this by constructing k-means clusterings of the observed (log)
  expression for each gene, and for each pair of genes, finding the
  maximum value of k for which the centroids of the clusters are
  monotonic between the genes.
}
\usage{
kCluster(cD, maxK = 100, matrixFile = NULL, replicates =
  NULL, algorithm = "Lloyd", B = 1000, sdm = 1)
}
\arguments{
  \item{cD}{
    A \code{\link[baySeq]{countData}} object containing the raw count
  data for each gene, or a matrix containing the logged and normalised
  values for each gene (rows) and sample (columns).
}
  \item{maxK}{
    The maximum value of k for which k-means clustering will be
  performed. Defaults to 100. 
}
  \item{matrixFile}{
    If given, a file to write the complete (gzipped) matrix of pairwise
  distances between genes. Defaults to NULL.
}
  \item{replicates}{If given, a factor or vector that can be cast to a
  factor that defines the replicate structure of the data. See Details.    
}
  \item{algorithm}{
    The algorithm to be used by the kmeans function.
  }
  \item{B}{
    Number of iterations of bootstrapping algorithm used to establish
  clustering validity
  }
  \item{sdm}{
    Thresholding parameter for validity; see Details.
  }
}
\details{
  In comparing two genes, we find the maximum value of k for which
  separate k-means clusterings of the two genes lead to a monotonic
  relationship between the centroids of the clusters. For this value of
  k, the maximum difference between expression levels observed within a
  cluster of either gene is reported as a measure of the dissimilarity
  between the two genes.

  There is a potential issue in that for genes non-differentially
  expressed across all samples (i.e., the appropriate value of k is 1),
  there will nevertheless exist clusterings for k > 1. For some
  arrangements of data, this leads to misattribution of
  non-differentially expressed genes. We identify these cases by
  adapting Tibshirani's gap statistic; bootstrapping uniformly
  distributed data on the same range as the observed data, calculating
  the dissimilarity score as above, and finding those cases for
  which the gap between the bootstrapped mean dissimilarity and the
  observed dissimilarity for k = 1 exceeds that for k = 2 by more than
  some multiple (sdm) of the standard error of the bootstrapped
  dissimilarities of k = 2. These cases are forced to be treated as
  non-differentially expressed by discarding all dissimilarity data for
  k > 1. 
  
  If the replicates vector is given, or if the replicates slot of a
  \code{\link[baySeq]{countData}} given as the 'cD' variable is
  complete, then the k-means clustering will be done on the median of
  the expression values of each replicate group. Dissimilarity calculations
  will still be made on the full data.
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
head(kClust)

# Alternatively, run on a count data object:
# load in analysed countData (baySeq package) object
library(baySeq)
data(cD.ratThymus, package = "clusterSeq")

# estimate likelihoods of dissimilarity on reduced set
kClust2 <- kCluster(cD.ratThymus[1:1000,])
head(kClust2)

}

\keyword{manip}

