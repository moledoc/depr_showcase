library(dplyr)
library(data.table)
library(parallel)
library(foreach)
source("Setup.R")

# package 'parallel'
MCpar1 <- sapply(n,function(n){
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
  result <- c(n,EX,analytical,error,time)
  return(result)
}) %>% data.table()

# package 'parallel' v2
start <- Sys.time()
# Calculate the number of cores
no_cores <- detectCores()
cl <- makeCluster(no_cores)
dt <- matrix(runif(n,0,1),ncol = no_cores)
intermean <- parSapply(cl, 1:1000,function(n,f){
    i <- runif(n,0,1)
    EX <- mean(f(i))
  },f
)
stopCluster(cl)
EX <- mean(intermean)
end <- Sys.time()
error <- EX - analytical
time <- difftime(end,start)
MCpar2 <- c(n,EX,analytical,error,time) %>% data.table()
MCpar <- bind_cols(MCpar1,MCpar2)
print(MCpar)


# foreach
# <TODO>

