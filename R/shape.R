aplShape <-
function(a) {
    if (is.vector(a)) return(length(a))
    return(dim(a))
}

