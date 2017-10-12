# modification on git from copied files
.clust2Vect <- function(x) {
    vec <- rep(NA, length(x))
    vec[unlist(x)] <- rep(seq_along(x), sapply(x, length))   
    if(any(is.na(vec))) vec[is.na(vec)] <- seq_len(sum(is.na(vec))) + max(vec,na.rm = TRUE)
    vec
}



wallace <- function(v1, v2) {
    if(inherits(v1, "SimpleIntegerList")) v1 <- .clust2Vect(v1)
    if(inherits(v2, "SimpleIntegerList")) v2 <- .clust2Vect(v2)
    confusion <- table(v1, v2)
    confusion <- confusion[rowSums(confusion) > 1, colSums(confusion) > 1]
    N11 <- sum(confusion * (confusion - 1) / 2)
    n1 <- table(v1); n2 <- table(v2)
    c(P.v1_given_v2 = N11 / sum(n1 * (n1 - 1) / 2), 
      P.v2_given_v1 = N11 / sum(n2 * (n2 - 1) / 2))    
}

.fowlkesMallows <- function(v1, v2) {
    sqrt(prod(wallace(v1, v2)))
}

.mirkin <- function(v1, v2) {
    n1 <- table(v1); n2 <- table(v2)
    confusion <- table(v1, v2)
    sum(n1^2) + sum(n2^2) - 2 * sum(confusion^2)
}
