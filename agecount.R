agecount <- function(age = NULL) {
        ## Check that "age" is non-NULL; else throw error
        ## Read "homicides.txt" data file
       ## Extract ages of victims; ignore records where no age is
      ## given
     ## Return integer containing count of homicides for that age
     
     r <- regexec(" [0-9]+ [Y|y][E|e][A|a][R|r](.*?)[O|o][L|l][D|d]",  homicides)
     m <- regmatches(homicides,  r)
     dates <- sapply(m,  function(x)  x[1])
     p <- gsub("years old| year old", "", dates)
     t <- p[!is.na(p)]
     count <- 0
     s <-  length(t)
   
     for(i in 1:s){
     	if(as.numeric(t[i]) == age){
     	count <- count +1
     }}
     
     return (count)  
  }   
