# modification on git from copied files
plotCluster <-
function(cluster, cD, sampleSize = 1000) {
    if(inherits(cD, "countData")) {
        dat <- cD@data
        dat[dat == 0] <- 1
        dat <- log2(t(t(dat) / as.vector(libsizes(cD)) * mean(libsizes(cD))))
    } else dat <- cD

    .plotClust <- function(cluster, cD, main) {
        sampclust <- (sample(seq_along(cluster), size = min(sampleSize, length(cluster)), replace = FALSE))
        rdat <- dat[cluster[sampclust],]
        mrdat <- apply(rdat, 1, function(x) {
            y <- x - min(x)
            y / max(c(1, x))
        })
        
        plot(x = NA, y = NA, xlim = c(1,ncol(dat)), ylim = c(min(mrdat), max(mrdat)), main = main, ylab = "row-normalised scale", xlab = "sample")
        apply(mrdat, 2, function(x) lines(x))
    }

    if(inherits(cluster, "SimpleIntegerList")) for(ii in seq_along(cluster)) .plotClust(cluster[[ii]], cD, main = names(cluster)[ii]) else .plotClust(cluster, cD, "")
    return(NULL)
}
