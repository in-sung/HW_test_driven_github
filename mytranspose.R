mytranspose <- function(x) {
  
  if(is.null(x)==TRUE){
    return(NULL)
  }
  
  if(is.vector(x)==TRUE){
    x <- matrix(x,nrow=length(x),ncol=1)
    return(x)
  }else if(is.data.frame(x)==TRUE){
    
    df = data.frame()
    for(i in 1:ncol(x)){
      if(is.factor(x[,i])==TRUE){
        c <- as.character(x[,i])
        df <- rbind(df,c)
      }else{
        c <- x[,i]
        df <- rbind(df,c)
      }
    }
    return(df)
  }else{
    if(nrow(x)==0){
      return(matrix(NA, nrow=0, ncol=0))
    }
    y <- matrix(1, nrow=ncol(x), ncol = nrow(x))
    for(i in 1:nrow(x)) {
      for(j in 1:ncol(x)) {
        y[j,i] <- x[i,j]
      }
    }
    return(y)
  }

}
