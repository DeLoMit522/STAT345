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

run_simulations = function(n_trials, n, p, cutoff){
  for(i in 1:n_trials){
    stuff <- generate_data(n, p)
    #hist(model_select(stuff$covariates, stuff$responses, cutoff))
  }
}

make_plot(datapath){
  data <- readLines(datapath)
  hist(data)
}

for(nn in c(100, 1000, 10000)){
  for (pp in c(10, 20, 50)){
    run_simulations(1, nn, pp, cutoff = 0.05)
  }
}
