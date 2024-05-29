data {
  int<lower=0> N;
  array[N] int y;
}
parameters {
  real<lower=0, upper=1> lambda;
}
model {
  lambda ~ beta(1, 1);
  y ~ poisson(lambda);
}
