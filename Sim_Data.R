#function for simulating data for model M_0 with t capture occassions
# paramters is a list of parameters need to generate the data described below
# output is the desired data to output, can return W, Wp C or S

parameters<-list(t=3,                     # t denotes number of capute occasions
                 p=.5,                     # p denotes probability of capture at each occasion
                 N=200,                     # N is the total population size
                 lambda=1,                # lambda: The number of photos per individual is modeled using a poisson dist
                 alpha.match=2,           # alpha.match and beta.match are the parameters in the beta distribution for true matches
                 beta.match=2,
                 alpha.non.match=2,      # alpha.non.match and beta.non.match are the parameters in the beta distribution for true non-matches
                 beta.non.match=2)

sim.data.M0<-function(parameters,output){
  t=parameters$t
  p=parameters$p                   
  N=parameters$N                     
  lambda=parameters$lambda                
  alpha.match=parameters$alpha.match            
  beta.match=parameters$beta.match 
  alpha.non.match=parameters$alpha.non.match       
  beta.non.match=parameters$beta.non.match 
  
  #Simulate W, where W is the observed capture history matrix
  #First simulate W with both observed and unobserved individuals. W is simulated under model M0
  W<-matrix(rbinom(n=(N*t),size=1,prob=p),nrow=N,ncol=t) 
  remove<-apply(W,1,sum)==0  #Removed unobserved individuals
  W<-W[!remove,]
  
  if(output=='W'){return(W)} #Returns observed capture history matrix
  
  N.obs<-length(W[,1])      #Number of observed individuals
  
  #Simulate the number of photos at each occasion using a translated poisson
  #Independent poissons with minimum value 1 are generated for each possible capture
  #Then multiplied by W, those animals animals not captured are multipled by 0 and 
  #those captured are multiplied by 1
  Y<-matrix((rpois(n=(N.obs*t),lambda=lambda)+1),nrow=N.obs,ncol=t)*W
   
  if(output=='Y'){return(Y)} #Returns matrix with number of photos per ind per capture occasion
  
  #Simulate X, where X is the array of photos for each capture history.
  N.photo<-sum(Y)                                 #Total Number of photos
  photo.id<-c(1:N.photo)                          #List of photo IDs
  max.photo<-max(Y)                               #Maximum number of photos for an individual in a capture occasion
  X<-array(NA,dim=c(N.obs,t,max.photo))
  photo.occasion<-matrix(NA,nrow=N.photo,ncol=2)            #Matrix that gives the capture occasion for each photo ID
  photo.occasion[,1]<-photo.id
                         
  
  sample.photo.id<-photo.id                                 #Vector of Photo IDs used in the construction of X
  for(i in 1:N.obs){
    for(j in 1:t){
      if(Y[i,j]>0){
        for(k in 1:Y[i,j]){
          if(length(sample.photo.id)>1){                    #Sample function does not work as expected when =1 this is a work around
          current.sample<-sample(sample.photo.id,size=1)
          }else{
          current.sample<-sample.photo.id  
          }
          X[i,j,k]<-current.sample
          photo.occasion[current.sample,2]<-j
          sample.photo.id<-sample.photo.id[sample.photo.id!=current.sample]
        }
      }
    }  
  }
  
  if(output=='X'){return(X)}
  
  indiv.photos<-vector("list",N.obs)           #List photos for each individual
  for(i in 1:N.obs){
    indiv.photos[[i]]<-as.vector(X[i,,])[!is.na(as.vector(X[i,,]))]
  }
  
  #Computes C matrix based off of X matrix
  c<-matrix(NA,nrow=N.photo,ncol=N.photo)
  
  for(i in 1:N.photo){                     #Sets the diagonal of C equal to 1
    c[i,i]<-1
  }
  
  for(i in 1:N.obs){                           #Looks at the photos for each individual sets all possible pairs per individual to 1
    current.indiv<-as.vector(indiv.photos[[i]])
    if(length(current.indiv)>1){
      pairs<-combn(current.indiv,m=2)
      for(j in 1:length(pairs[1,])){
        c[pairs[1,j],pairs[2,j]]<-1
        c[pairs[2,j],pairs[1,j]]<-1
      }
    }
  }
  
  c[is.na(c)]<-0                           #Changes the rest of c to zeros
  if(output=='c'){return(c)}
  
  #Simulate the score values
  S<-matrix(rbeta(n=(N.photo*N.photo),shape1=alpha.match,shape2=beta.match),nrow=N.photo,ncol=N.photo)*c #Simulates the true match scores
  S[which(S==0)]<-rbeta(n=length(which(S==0)),shape1=alpha.non.match,shape2=beta.non.match)
  if(output=='S'){return(S)}
  if(output=='ALL'){return(list(S,photo.occasion,W,Y,X,c))}
}