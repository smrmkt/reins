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
    Y ~ normal(alpha + X * beta, sigma);
}
