complete <- function(directory, id = 1:332) {
    files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
    tmp <- lapply(files_list[id],read.csv)
    output<-do.call(rbind, tmp)
    NOBS <- vector(length=length(files_list))
    for (i in id){
        NOBS[i] <- sum(complete.cases(subset(output,ID==i)))
    }
    data.frame(ID=id, nobs = NOBS[id]) 
}

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## ID nobs
## 1  117
## 2  1041
## ...
## where 'ID' is the monitor ID number and 'nobs' is the
## number of complete cases

##source("complete.R")
##complete("specdata", 1)

##   ID nobs
## 1  1  117

##complete("specdata", c(2, 4, 8, 10, 12))

##   ID nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96

##complete("specdata", 30:25)

##   ID nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463

##complete("specdata", 3)

##   ID nobs
## 1  3  243
