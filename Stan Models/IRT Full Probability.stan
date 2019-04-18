/*
  
  author: Rob Williams
  contact: jayrobwilliams@gmail.com
  project: peace agreement strength
  created: June 8, 2017
  updated: November 27, 2017
  
*/

/* hierarchical IRT measurement and regression model */

data {
  
  /* dimensionality */
  int<lower=1> N; // length of indicator vector
  int<lower=1> O; // number of observed indicators
  int<lower=1> M; // number of agreements
  int<lower=1> K; // number of agreement covariates
  int<lower=1> C; // number of conflicts
  int<lower=1> agmt[N]; // agreement indexing vector
  int<lower=1> inds[N]; // observed indicator indexing vector
  int<lower=1> confs[M-2]; // conflict ID indexing vector
  
  /* observed indicators */
  int<lower=0,upper=1> X[N]; // - 2 for 2 agreements fixed
  
  /* hierarchical predictors */
  matrix[M-2,K] Z; // agreement covariate mean predictors
  
  /* strong and weak agreement strength constraints */
  real theta_lo; // theta value for weak agreement 
  real theta_hi; // theta value for strong agreement 
  int<lower=0> id_lo; // row number for weak agreement
  int<lower=0> id_hi; // row number for strong agreement
  
}

parameters {
  
  /* hyperpriors */
  real mu_alpha;
  real mu_gamma;
  real mu_delta;
  real<lower=.001> sigma_alpha;
  real<lower=.001> sigma_gamma;
  real<lower=.001> sigma_delta;
  real<lower=.001> sigma_theta;
  
  /* IRT parameters */
  vector[K] beta; // coefficients on agreement level predictors
  vector[O] alpha; // difficulty parameter
  vector<lower=0>[O] gamma; // discrimination parameters
  vector[C] delta; // conflict-random intercepts
  vector[M-2] theta_free; // latent agreement strength
  
}

transformed parameters {
  
  vector[M] theta; // latent agreement strength
  
  theta[1:(id_lo - 1)] = theta_free[1:(id_lo - 1)];
  theta[id_lo] = theta_lo;
  theta[(id_lo + 1):(id_hi - 1)] = theta_free[id_lo:(id_hi - 2)];
  theta[id_hi] = theta_hi;
  theta[(id_hi + 1):M] = theta_free[(id_hi - 1):(M - 2)];
  
}

model {
  
  /* hyperpriors and priors */
  beta ~ student_t(4, 0, 1); // coefficients on agreement mean predictors
  
  mu_alpha ~ normal(0, 25); // hyperpriors on difficulty
  sigma_alpha ~ cauchy(0, 5);
  alpha ~ normal(mu_alpha, sigma_alpha);
  
  mu_gamma ~ normal(0, 25); // hyperpriors on discrimination
  sigma_gamma ~ cauchy(0, 5);
  gamma ~ normal(mu_gamma, sigma_gamma);
  
  mu_delta ~ normal(0, 5); // hyperpriors on conflict-random intercept
  sigma_delta ~ cauchy(0, 5);
  delta ~ normal(mu_delta, sigma_delta);
  
  sigma_theta ~ cauchy(0, 5);
  theta_free ~ normal(delta[confs] + Z*beta, sigma_theta); // hyperpriors on ideal point
  
  /* observed indicators */
  X ~ bernoulli_logit(gamma[inds] .* theta[agmt] - alpha[inds]);
  
}
