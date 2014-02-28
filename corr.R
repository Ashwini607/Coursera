corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
  
        list <- list.files(directory)

        a =c()
	
        for(file in list){
        	    data <- read.csv(paste(directory,"/", file, sep="")) 	    
        	    p <- sum(complete.cases(data));
        	    if(p > threshold){
        	    good <- complete.cases(data$sulfate,data$nitrate)	
        	    a <- append(a,cor(data$sulfate[good],data$nitrate[good]))
        	    }
         }
         	if(length(a) == 0)	
         	{
         		a <- vector(mode="numeric", length=0);
         	}
        	
           return(a)
}