######################################################################
#Function to Check of Matrix is a valid Matrix of Photograph matches #
#Function Checks the following for a matrix A                        #
# 1. A is comprised only of 1's and 0's                              #
# 2. The diagonal of A is all 1's                                    #
# 3. A is symmetric ie a_ij =a_ji for all i and j                    #
# 4. If a_ij = a_kj =1 then a_ik=1                                   #   
######################################################################

Matrix.Check<-function(A){
  
  #Checks to see if Matrix
  if(!is(A,"matrix")){
    return('Error: Please give a Matrix')
  } 
  
  #Checks for missing data
  if(sum(is.na(A))>0)
  {
    return('Error: Missing Data')
  }
  
  #Checks to see is Matrix has non 0-1 entries
  if(sum(A>1)>=1)
  {
    return('Error: Matrix has elements that are not 0 or 1')
  }
  
  #Checks to see that A is a square matrix
  n.col<-length(A[1,])
  n.row<-length(A[,1])
  
  if(n.col!=n.row)
  {
    return('Error: Matrix is not a Square Matrix')
  }
  
  #Checks to see that the diagonal of A is all 1's
  if(sum(diag(A))!=n.col)
  {
    return('Error: Diagonal of Matrix is not all 1s')
  }
  
  #Checks to see if A is symmetric
  if(isSymmetric(A)==FALSE)
  {
    return('Error: A is not a symmetric Matrix')
  }
  
  #Check to see that If a_ij = a_ik =1 then a_jk=1 is satisfied
  temp<- rep(0,length=n.row)      #This vector will be used in the loop to store the set of 1's from a given column
  
  for(j in 3:(n.col)){            
    for(i in 1:(j-1)){            #Find the entries of a column that are equal to one, store them in temp
      if(A[i,j]==1){temp[i]=i}
    }
    
    non.zero<-subset(temp,temp!=0) #Removes the zeros to give only the entries with 1's 
    
    for(i in non.zero){            #Checks that corresponding entries are 1
      for(k in non.zero){
        if(m[i,k]!=1)
        {
          return(cat('Matrix does not satisfy requirement: a_',i,j,'= a_',k,j,'=1 then a_',i,k,'=1'))
        }
      }
    }
    temp<- rep(0,length=n.row)
  }
  
  #If no errors returns true
  return(TRUE)
}