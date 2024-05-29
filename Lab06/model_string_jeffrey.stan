data {
  int<lower=0> N;
  array[N] int y;
}
parameters {
  real<lower=0, upper=1> lambda;
}
model {
  lambda ~ gamma(0.5, 0.5); // Jeffrey's prior: gamma(0.5, 0.5)
  target += -log(lambda);   // Adding the log of Jacobian adjustment
  y ~ poisson(lambda);
}
