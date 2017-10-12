# modification on git from copied files
associatePosteriors <-
function(cD, maxsize = 250000, matrixFile = NULL) {    
    matord <- do.call("rbind", lapply(seq_len(ncol(cD@orderings)), function(ii) as.integer(cD@orderings[,ii])))
    posteriors <- t(cD@posteriors)
    
    objsize <- as.numeric(gsub(" Mb", "", format(object.size(posteriors), units = "Mb"))) + as.numeric(gsub(" Mb", "", format(object.size(matord), units = "Mb")))    
    splits <- ceiling(objsize / sqrt(maxsize))
    
                                        #clusterExport(cl, c("matord", "posteriors"), envir = environment())
    if(splits == 1) splosts <- list(seq_len(ncol(posteriors))) else splosts <- split(seq_len(ncol(posteriors)), cut(seq_len(ncol(posteriors)),
           splits, labels = FALSE))
    
    
    aMM <- NULL
    for(ss in seq_along(splosts)) {
        splsub <- splosts[[ss]]
        
        if(!is.null(matrixFile)) {
            if(substr(matrixFile, nchar(matrixFile) -2, nchar(matrixFile)) != ".gz") {
                message("Matrix file will be gzipped; appending '.gz' to filename supplied")
                matrixFile <- paste(matrixFile, ".gz", sep = "")
            }
            if(file.exists(matrixFile)) file.remove(matrixFile)
            gzfile <- gzfile(matrixFile, "w")
        }
        
        saM <- do.call("rbind", lapply(ss:length(splosts), function(jj) {
            message(".", appendLF = FALSE)
            rposts <- posteriors[,splosts[[jj]]];
            rmatord <- matord[,splosts[[jj]]];

            if(!is.null(matrixFile)) lapplyFun <- lapply else lapplyFun <- bplapply
            aM <- cbind(splsub, do.call("rbind", lapplyFun(splsub,
                                                          function(ii) {
                                                              posts <- colSums(exp(rposts + log(rmatord == matord[,ii]) + posteriors[,ii]))
                                                              
                                                              posts[splosts[[jj]] <= ii] <- NA
                                                              if(!is.null(matrixFile))
                                                                  writeLines(paste(posts, collapse = "\t"), gzfile)

                                                              c(splosts[[jj]][which.max(posts)], max(posts, na.rm = TRUE))
                                                              
                                                          })))
            aM[aM[,3] > -Inf,]
        }))

        message("", appendLF = TRUE)

        if(!is.null(matrixFile)) close(gzfile)
        
        aMM <- rbind(aMM, saM)
    }
    
    aMM
    splam <- split(as.data.frame(aMM, stringsAsFactors = FALSE), aMM[,1])
    sAMM <- do.call("rbind", lapply(splam, function(x) x[which.max(x[,3]),]))
    colnames(sAMM) <- c("id", "pair.id", "stat")
    sAMM$stat <- 1 - sAMM$stat
    sAMM
}
