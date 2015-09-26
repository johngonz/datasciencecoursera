corr <- function(directory, threshold = 0) {
    num_complete_obs <- complete(directory)
    min_thresh_complete_obs <- subset(num_complete_obs, nobs > threshold)
    IDs <- min_thresh_complete_obs$ID
    files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
    tmp <- lapply(files_list[min_thresh_complete_obs$ID], read.csv)
    corr_vec <-vector(length=length(IDs))
    if (length(corr_vec)>0){
        output <- na.omit(do.call(rbind, tmp))  #combine all files and remove incomplete rows
        for (i in 1:length(IDs)){
            corr_vec[i] <- cor(x = subset(output$sulfate, output$ID == IDs[i]), y = subset(output$nitrate, output$ID == IDs[i]))
        }  
    }
    corr_vec
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
}