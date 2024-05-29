data {
  int<lower=0> n;  // Total number of samples
  int<lower=0> y;  // Number of samples with high bacter X level
}

parameters {
  real<lower=0, upper=1> p;  // Probability of high bacter X level
}

model {
  p ~ beta(1, 10);  // Beta(1, 10) prior for p
  y ~ binomial(n, p);  // Likelihood function
}
