myfunction <- function() {
  x <- rnorm(100)
  mean(x)
}

second <-function(x) {
   x+ rnorm(length(x))
}

above <- function(x,n=10){
  use <- x>n
  x[use]
}

columnmean <- function(y, removeNA=TRUE){
  nc <- ncol(y)
  means <-numeric(nc)
  for(i in 1:nc){
    means[i] <- mean(y[,i], na.rm=removeNA)
  }
  means
}

 weightmedian <- function(directory, day)  {
         files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
         dat <- data.frame()                             #creates an empty data frame
         for (i in 1:5) {                                
                 #loops through the files, rbinding them together 
                     dat <- rbind(dat, read.csv(files_list[i]))
                 }
         dat_subset <- dat[which(dat[, "Day"] == day),]  #subsets the rows that match the 'day' argument
         median(dat_subset[, "Weight"], na.rm=TRUE)      #identifies the median weight 
         #while stripping out the NAs
 }

 
 makeVector <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setmean <- function(mean) m <<- mean
     getmean <- function() m
     list(set = set, get = get,
          setmean = setmean,
          getmean = getmean)
 }
 cachemean <- function(x, ...) {
     m <- x$getmean()
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
     }
     data <- x$get()
     m <- mean(data, ...)
     x$setmean(m)
     m
 }
 
 open.account <- function(total) {
     list(
         deposit = function(amount) {
             if(amount <= 0)
                 stop("Deposits must be positive!\n")
             total <<- total + amount
             cat(amount, "deposited.  Your balance is", total, "\n\n")
         },
         withdraw = function(amount) {
             if(amount > total)
                 stop("You don't have that much money!\n")
             total <<- total - amount
             cat(amount, "withdrawn.  Your balance is", total, "\n\n")
         },
         balance = function() {
             cat("Your balance is", total, "\n\n")
         }
     )
 }