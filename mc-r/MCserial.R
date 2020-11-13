
library(data.table)
library(dplyr)

source("Setup.R")

## apply implementation
MC1 <- function(n){
    start <- Sys.time()
    i <- runif(n,0,1)
    EX <- mean(f(i))
    end <- Sys.time()
    time <- difftime(end,start)
    error <- EX - analytical
    return(c(n,EX,analytical,error,time))
  }
result <- MC1(n) %>% data.table()
print(result)


# Attempt nr2
start <- Sys.time()
dt <- matrix(runif(n,0,1),ncol = 1000)
MC2 <- sapply(1:1000,function(i,dt){
    EX <- dt[,i] %>% f %>% mean
  },dt) %>% mean
end <- Sys.time()
error <- MC2 - analytical
time <- difftime(end,start)
result2 <- c(n,MC2,analytical,error,time) %>% data.table
result <- bind_cols(result,result2)
print(result)


