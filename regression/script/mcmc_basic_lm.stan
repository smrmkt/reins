data {
    int<lower=1> N;
    int<lower=1> M;
    matrix[N, M] X;
    int<lower=0, upper=200> Y[N];
}
parameters {
    real alpha;
    vector[M] beta;
    real<lower=0> sigma;
}
model {
    for (i in 1:N)
        Y[i] ~ normal(alpha + dot_product(beta, X[i]), sigma);
}
