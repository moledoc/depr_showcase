
options(scipen=1000)

# define number of iterations
n <- 10000000
# header
header <- c("n","computational","analytical","error","walltime","type")

# Define example function.
f <- function(x){
  return(x**2 + x**4+ sin(x) + cos(x) +x**25)
}

# analytical solution to the function,
# when random values are generated from 
# uniform distribution U(0,1)
# analytical <- 613/390 + sin(1) - cos(1)
analytical <- integrate(f,0,1)$value

