% modification on git from copied files
\name{wallace}
\alias{wallace}

\title{
Computes Wallace scores comparing two clustering methods.
}
\description{
  Given two clusterings A and B we can calculate the likelihood that
  two elements are in the same cluster in B given that they are in the
  same cluster in A, and vice versa.
}
\usage{
wallace(v1, v2)
}

\arguments{
  \item{v1}{
    SimpleIntegerList object (output from makeClusters or makeClustersFF).
}
  \item{v2}{
    SimpleIntegerList object (output from makeClusters or makeClustersFF).
}
}
\value{
Vector of length 2 giving conditional likelihoods.
}
\author{
Thomas J. Hardcastle
}

\examples{

# using likelihood data from a Bayesian analysis of the data

# load in analysed countData object
data(cD.ratThymus, package = "clusterSeq")

# estimate likelihoods of dissimilarity on reduced set
aM <- associatePosteriors(cD.ratThymus[1:1000,])

# make clusters from dissimilarity data
sX <- makeClusters(aM, cD.ratThymus[1:1000,], threshold = 0.5)

# or using k-means clustering on raw count data

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
kClust <- kCluster(normRT, replicates = cD.ratThymus@replicates)

# make the clusters from these data.
mkClust <- makeClusters(kClust, normRT, threshold = 1)


# compare clusterings
wallace(sX, mkClust)

}

\keyword{manip}
