.makeClusters <-
function(assocMat, threshold = 0.5) {
    clM <- assocMat
    diag(clM) <- NA
    clM[clM > threshold] <- NA
    rownames(clM) <- colnames(clM) <- seq_len(nrow(clM))
    mines <- apply(clM, 1, function(x) if(all(is.na(x))) NA else which.min(x))
    clcon <- cbind(seq_len(nrow(clM)), mines)
    clcon <- clcon[!is.na(clcon[,1]) & !is.na(clcon[,2]),]
    clcon <- t(apply(clcon, 1, sort))
    clcon <- clcon[!duplicated(clcon),]

    clusterList <- list()
    while(TRUE) {
        initcon <- clcon[1,1]
        while(TRUE) {
            newcon <- unique(as.vector(clcon[clcon[,1] %in% initcon | clcon[,2] %in% initcon,]))
            if(all(newcon %in% initcon)) break()
            initcon <- newcon
        }
        clcon <- clcon[!(clcon[,1] %in% initcon | clcon[,2] %in% initcon),,drop=FALSE]
        clusterList <- c(clusterList, list(sort(as.numeric(initcon))))
        if(length(clcon) == 0) break()
    }
    clusterList <- clusterList[order(sapply(clusterList, length), decreasing = TRUE)]
    clusterList
}