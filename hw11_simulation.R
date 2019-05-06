generate_data <- function(n,p){
  infoForMatrix <- rnorm(n*p)
  covariates <- matrix(data = infoForMatrix, nrow = n, ncol = p)
  responses <- rnorm(n)
  return(c(covariates, responses))
}
