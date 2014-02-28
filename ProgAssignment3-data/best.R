best <- function(state, outcome) {
	 ## Read outcome data
            ## Check that state and outcome are valid
          ## Return hospital name in that state with lowest 30-day death
          a= c()
          b = c("heart attack", "heart failure", "pneumonia")
          data <- read.csv("outcome-of-care-measures.csv")
          
          for(id in data$State){
          	a <- append(a,id)
          if(outcome %in% b){
          if(id == state) {
          #	print("2")
          	if(outcome == "heart attack")
          	{
          		data1 <- subset(data,data$State == state & data[,11]!= "Not Available")
         		disease <- as.numeric(as.character(data1[,11]) )
          		newData <- data1[order(disease),]
          		final <- subset(newData, newData[,11] == newData[,11][1]) 
          	    absFinal <- final[order(final$Hospital.Name),]  
          		  
          	}
          	if(outcome == "heart failure")
          	{
          		data1 <- subset(data,data$State == state & data[,17]!= "Not Available")
          		disease <-as.numeric( as.character(data1[,17]))
          		newData <- data1[order(disease),]  
          	    final <- subset(newData, newData[,17] == newData[,17][1]) 
          	    absFinal <- final[order(final$Hospital.Name),]      		
          	}
          	if(outcome == "pneumonia")
          	{
          		data1 <- subset(data,data$State == state & data[,23]!= "Not Available")
          		disease <- as.numeric(as.character(data1[,23]))
          		newData <- data1[order(disease),]
          		final <- subset(newData, newData[,23] == newData[,23][1]) 
          	    absFinal <- final[order(final$Hospital.Name),] 
          	}  
 
          	 return(as.character(absFinal$Hospital.Name[1]) )     	
          	break
          }
          }
          }
                   
           
          if(all(b != outcome)){
          	stop("invalid outcome")
          }
          
          if(all(a!=state)){
          	stop("invalid state")
          }

}