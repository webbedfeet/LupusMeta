## Template file for running JAGS on the Moving Average MCMC

#library(random)
require(rjags)
load.module('glm')
load.module('lecuyer')
source('fullmodel.mixed.txt')
if(data.mixed$N2 > 0){
  inits <- with(data.mixed, 
                list(lambda = rep(1,J),
                     nu = rep(1, J),
                     .RNG.name = "lecuyer::RngStream",
                     .RNG.seed = sample(1:10000,1),
                     td = ifelse(isCensored1==1, 100, NA), # init for censored td
                     Y = ifelse(isCensored2==1, n, NA)# init for censored Y
                     #                      ypred14 = 100,
                     #                      ypred24 = 100
                )
  )
  
  parameters <- c('lambda','nu','pr5','pr10','pr15')
  
  mod <- jags.model("fullmodelcts.bug",
                    data = data.mixed,
                    inits = inits,
                    n.chains = 4, # Change to 4 after testing
                    n.adapt = 1000)
  #update(mod, n.iter=1000) # Burn-in
  codaSamples <- coda.samples(mod, variable.names=parameters, n.iter=1000, thin=1)
} else {
  inits <- with(data.mixed, 
                list(lambda = rep(1,J),
                     nu = rep(1, J),
                     .RNG.name = "lecuyer::RngStream",
                     .RNG.seed = sample(1:10000,1),
                     td = ifelse(isCensored1==1, 100, NA) # init for censored td
                     #Y = ifelse(isCensored2==1, n, NA)# init for censored Y
                     #                      ypred14 = 100,
                     #                      ypred24 = 100
                )
  )
  
  parameters <- c('lambda','nu','pr5','pr10','pr15')
  
  mod <- jags.model("fullmodelcts2.bug",
                    data = data.mixed,
                    inits = inits,
                    n.chains = 4, # Change to 4 after testing
                    n.adapt = 1000)
  #update(mod, n.iter=1000) # Burn-in
  codaSamples <- coda.samples(mod, variable.names=parameters, n.iter=1000, thin=1)
}
save(codaSamples, file='Rfile.rda',compress=T)
