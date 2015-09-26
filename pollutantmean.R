pollutantmean <- function(directory,  pollutant, id = 1:332)  {
    files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
    tmp<- lapply(files_list[id],read.csv)
    output<-do.call(rbind, tmp)
    mean(output[, pollutant], na.rm=TRUE)      #identifies the mean weight while stripping out the NAs
}

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
## NOTE: Do not round the result!

##source("pollutantmean.R")
##pollutantmean("specdata", "sulfate", 1:10)

## [1] 4.064

##pollutantmean("specdata", "nitrate", 70:72)

## [1] 1.706

##pollutantmean("specdata", "nitrate", 23)

## [1] 1.281
