% modification on git from copied files
\name{clusterSeq-package}
\alias{clusterSeq-package}
\alias{clusterSeq}
\docType{package}
\title{
\packageTitle{clusterSeq}
}
\description{
Identification of clusters of co-expressed genes based on their
expression across multiple (replicated) biological samples.}

\details{
The DESCRIPTION file:
\packageDESCRIPTION{clusterSeq}
\packageIndices{clusterSeq}
}
\author{
\packageAuthor{clusterSeq}

Maintainer: \packageMaintainer{clusterSeq}
}

\keyword{package}

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

# or using likelihood data from a Bayesian analysis of the data

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
