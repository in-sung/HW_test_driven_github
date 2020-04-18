equals <- function(A,B){
  #NULL
  if(is.null(A)==TRUE && is.null(B)==TRUE){
    return(TRUE)
  }
  # 0 by 0 matrix
  if(nrow(A)==0 && nrow(B)==0){
    return(TRUE)
  }
  # matrix & dataframe
  if(is.matrix(A) == TRUE || is.data.frame(A)==TRUE){
    re <- 0
    for(i in 1:nrow(A)){
      for(j in 1:ncol(A)){
        if(is.na(A[i,j])==TRUE && is.na(B[j,i])==TRUE){
          re = re+1
        }else{
          r <- A[i,j]==B[j,i]
          #print(r)
          re <- re + r
        }
      }
    }
    return(re == nrow(A)*ncol(A))
  }
  
  #vector
  if(is.vector(A)==TRUE){
    re <- 0
    for(i in 1:length(A)){
      if(is.na(A[i])==TRUE && is.na(B[i,])==TRUE){
        re <- re+ 1
      }else{
        r <- A[i]==B[i,]
        #print(r)
        re <- re+ r
      }
    }
    return(re == length(A))
  }

}

myvar1 <-  matrix(1:10, nrow=5, ncol=2)
equals(myvar1, mytranspose(myvar1))
myvar1 <-  matrix(NA, nrow=0, ncol=0)
equals(myvar1, mytranspose(myvar1))
myvar1 <-  matrix(c(1,2), nrow=1, ncol=2)
equals(myvar1, mytranspose(myvar1))
myvar1 <-  matrix(c(1,2), nrow=2, ncol=1)
equals(myvar1, mytranspose(myvar1))


myvar2 <- c(1,2,NA,3)
equals(myvar2, mytranspose(myvar2))
myvar2 <- c(NA)
equals(myvar2, mytranspose(myvar2))
myvar2 <- c()
equals(myvar2, mytranspose(myvar2))


d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata3 <- data.frame(d,e,f)

equals(mydata3, mytranspose(mydata3))
