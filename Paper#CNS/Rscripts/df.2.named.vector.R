
df.2.named.vector <- function(df) {
    x <- as.character(df[,2])
    names(x) <- as.character(df[,1])
    
    return(x)
}
