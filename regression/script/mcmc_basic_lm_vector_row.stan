data {
    int<lower=1> N;
    int<lower=1> M;
    matrix[N, M] X;
    vector[N]    Y;
}
parameters {
    real          alpha;
    vector[M]     beta;
    real<lower=0> sigma;
}
model {
    for(i in 1:N)
        Y[i] ~ normal(alpha + X[i] * beta, sigma);
}
