

fizzbuzz <- function(n=100){
  sapply(1:n,function(i){
    fizz <- ifelse(i%%3==0,"Fizz","")
    buzz <- ifelse(i%%5==0,"Buzz","")
    fizzbuzz <- paste0(fizz,buzz)
    return(ifelse(fizzbuzz=="",i,fizzbuzz))
  })
}
