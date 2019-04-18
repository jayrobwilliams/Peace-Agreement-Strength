/*
  
  author: Rob Williams
  contact: jayrobwilliams@gmail.com
  project: peace agreement strength
  created: June 28, 2017
  updated: June 28, 2017
  
*/

/* linear regression model */

data {
  
  /* dimensionality of data and model */
  int<lower=0> N; // number of observations
  int<lower=0> K; // number of predictors (p + 1)
  int<lower=1> C; // number of conflicts
  int<lower=1> confs[N]; // conflict ID indexing vector
  
  /* response and explanatory variables */
  real y[N]; // response variable
  matrix[N,K] X; // design matrix
  
}

parameters {
  
  /* parameters to be estimated by model */
  vector[K] beta; // regression coefficeients
  real<lower=0> sigma; // error
  
  /* hyperpriors on delta */
  real mu_delta;
  real<lower=.001> sigma_delta;
  
  vector[C] delta; // conflict-random intercept
  
}

model {
  
  /* conflict-random intercept */
  mu_delta ~ normal(0, 25); // hyperpriors on conflict-random intercept
  sigma_delta ~ cauchy(0, 5);
  delta ~ normal(mu_delta, sigma_delta);
  
  /* the model itself */
  beta ~ normal(0, 5); // prior on coefficients
  sigma ~ gamma(7.5, 10); // prior on error
  y ~ normal(delta[confs] + X * beta, sigma); // response variable
  
}
