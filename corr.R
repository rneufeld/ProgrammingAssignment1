corr <- function(directory, threshold=0) {
    files_list <- list.files(directory, full.names=TRUE)
    #y is a vector of all the complete cases in the data set
    y <- vector()
    for(i in 1:332) {
        y <- c(y, sum(complete.cases(read.csv(files_list[i]))))
    }
    #id is a vector of the id's of the elements in y whose complete cases is greater than the threshold
    id <- vector()
    for(i in 1:332) {
        if(y[i] > threshold) {
        id <- c(id, i)
        }
    }
    id_length <- length(id)
    z <- rep(0, id_length)
    j <- 1
    for (i in id) {
          x <- read.csv(files_list[i])
          z[j] <- cor(x$sulfate, x$nitrate, use="complete.obs")
          j <- j+1
    }
    return(z)
}
