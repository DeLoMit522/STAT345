generate_data = function(n, p){
  whatToReturn = list()
  covariates = matrix(nrow = n, ncol = p)
  for(i in 1:p){
    covariates[,i] = rnorm(p)
  }
  responses = rnorm(n)
  whatToReturn$covariates = covariates
  whatToReturn$responses = responses
  return(whatToReturn)
}

model_select = function(covariates, responses, cutoff){
  firstModel = lm(responses ~ covariates, summary(lm(responses~covariates))[[4]][2,4] <= cutoff)
  secondModel = lm(responses ~ covariates[, summary(lm(responses~covariates))[[4]][2,4] <= cutoff])
  return(summary(secondModel)[[4]][2,4])
}