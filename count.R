count <- function(cause = NULL) {
        ## Check that "cause" is non-NULL; else throw error
        ## Check that specific "cause" is allowed; else throw error
        ## Read "homicides.txt" data file
        ## Extract causes of death
        ## Return integer containing count of homicides for that cause
         b = c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
    
    homicides <- readLines("homicides.txt")
    if(cause %in% b){
    if(cause == "other"){
    	r <- regexec("<dd>[C|c]ause: [O|o][T|t][H|h][[E|e][R|r]</dd>", homicides)
    	}
    	
    	if(cause == "asphyxiation"){
    	r <- regexec("<dd>[C|c]ause: [A|a][S|s][P|p][[H|h][Y|y][X|x][I|i][A|a][T|t][I|i][O|o][N|n]</dd>", homicides)
    	}
    	if(cause == "blunt force"){
    	r <- regexec("<dd>[C|c]ause: [B|b][L|l][U|u][[N|n][T|t][ ][F|f][O|o][R|r][C|c][E|e]</dd>", homicides)
    	}
    	if(cause == "shooting"){
    	r <- regexec("<dd>[C|c]ause: [S|s][H|h][O|o][O|o][[T|t][I|i][N|n][G|g]</dd>", homicides)
    	}
    	if(cause == "stabbing"){
    	r <- regexec("<dd>[C|c]ause: [S|s][T|t][A|a][B|b][B|b][I|i][N|n][G|g]</dd>", homicides)
    	}
    	if(cause == "unknown"){
    	r <- regexec("<dd>[C|c]ause: [U|u][N|n][K|k][[N|n][O|o][W|w][N|n]</dd>", homicides)
    	}
    	
    	m <- regmatches(homicides, r)
    	s <- length(m)
    	count <- 0
    	i <- 0
    	t <- c()
    	for(i in 1:s){
    	  if(length(m[[i]]) > 0)	
    	 {
    	 	count <- count +1
    	 }         
    	}       
    return (count)
    
    }
         
    if(all(b != cause)){
          	stop()
          }     
        
}
