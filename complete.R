complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        p = c()
        q = c()
        list <- list.files(directory)

        	for(i in id){
        		    for(file in list){
        	         data <- read.csv(paste(directory,"/", file, sep=""))
        		    if(data$ID[1] ==i){
        			p <- append(p,i) ;
        			q <-append(q,sum(complete.cases(data)));
        		}
        	}
        }
        x <- data.frame(id =p , nobs = q)
        return(x)
        
}