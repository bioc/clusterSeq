# modification on git from copied files
.nameList <- function(x, cD) {
    groups <- which(colSums(cD@posteriors[x,,drop = FALSE] > log(0.5)) > length(x) * 0.1)
    if(length(groups) == 0) return("")
    groupD <- sapply(groups, function(ii) {
        grp <- cD@groups[[ii]]
        groupDesc <- lapply(levels(grp), function(gg) {
            grprep <- unique(cD@replicates[grp == gg])
            paste(grprep, collapse = "=")
        })
        groupDesc <- lapply(groupDesc, function(x) paste("{", x, "}", sep = ""))
        if(length(groupDesc) == 1) return(groupDesc[[1]])
        orders <- cD@orderings[x,ii][cD@posteriors[x,ii] > log(0.5)]
        lapply(levels(orders)[table(orders) > 0.1 * length(orders)], function(ordering) {                
            paste(as.vector(rbind(unlist(strsplit(as.character(ordering), "[^(>|=)]")), unlist(groupDesc[as.numeric(strsplit(as.character(ordering), ">|=")[[1]])]))), collapse = "")
        })    
    }
                     )
    paste(unlist(groupD), collapse = ";")
}


makeClusters <-
function(aM, cD, threshold = 0.5) {
    greaterThan = FALSE
    if(greaterThan) aM <- aM[aM[,3] > threshold,] else aM <- aM[aM[,3] < threshold,]
    
    x <- aM[,seq_len(2)]
    for(ii in nrow(x):1) {
        rx <- which(x[,1] == x[ii,2])
        if(length(rx) > 0)
            x[ii,2] <- x[rx,2]
    }
    colnames(x) <- c("cl1", "cl2")
    rx <- rbind(x, cbind("cl1" = unique(x[,2]), "cl2" = unique(x[,2])))
    sx <- split(rx[,1], rx[,2])
    sx <- sx[order(sapply(sx, length), decreasing = TRUE)]

    sx <- c(sx, as.list(which(!seq_len(nrow(cD)) %in% unlist(sx))))
    
    if(inherits(cD, "countData")) names(sx) <- sapply(sx, .nameList, cD = cD)
    sx <- as(sx, "IntegerList")
    sx
}


makeClustersFF <- function(file, method = "complete", cut.height = 5)
{
    kmat <- read.table(file, header = TRUE, row.names=1)
    kmat[kmat == Inf] <- 1e6
    dkmat <- as.dist(kmat)

    fit <- hclust(dkmat, method = method)
    fit$height <- round(fit$height, 6)
    
    groups <- cutree(fit, h = cut.height)
    mqGroups <- split(seq_len(nrow(kmat)), groups)
                                        #names(mqGroups) <- sapply(mqGroups, nameList, cD = cD)
    mqGroups <- mqGroups[order(sapply(mqGroups, length), decreasing = TRUE)]
    mqGroups <- as(mqGroups, "IntegerList")
    mqGroups
}

