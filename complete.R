complete <- function(directory, id = 1:332) {
      files_list <- list.files(directory, full.names=TRUE)
      y <- vector()
      for(i in id){
            x <- id
            y <- c(y, sum(complete.cases(read.csv(files_list[i]))))
      }
      df <- data.frame(x, y)
      colnames(df) <- c("id","nobs")
      return(df)
}
