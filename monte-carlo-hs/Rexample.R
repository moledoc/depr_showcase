f <- function(x){
  return(x**2 + x**4+ sin(x) + cos(x) +x**25)
}

analytical <- 613/390 + sin(1) - cos(1)

powers <- 3:7

iterations <- rep(c(1,5),each=length(powers))*10**(powers)

null <- lapply(iterations,function(n){
    start <- Sys.time()
    i <- runif(n,0,1)
    EX <- mean(f(i))
    end <- Sys.time()
    time <- difftime(end,start)
    error <- EX - analytical
    write.table(file = "results.csv",x = paste(n,EX,analytical,error,"R serial",time,sep = ","),append = TRUE,col.names=F,row.names=F,quote=F)
  }
)

library(parallel)

null <- sapply(iterations,function(n){
  start <- Sys.time()
  # Calculate the number of cores
  no_cores <- detectCores()
  start <- Sys.time()
  
  # Initiate cluster
  cl <- makeCluster(no_cores)
  
  intermean <- parSapply(cl, rep(n/no_cores,no_cores),function(n,f){
    i <- runif(n,0,1)
    EX <- mean(f(i))
    
  },f
  )
  
  stopCluster(cl)
  EX <- mean(intermean)
  
  end <- Sys.time()
  time <- difftime(end,start)
  error <- EX - analytical
    write.csv(file = "results.csv",x = paste(n,EX,analytical,error,"R serial",time,sep = ","),append = T)
})
