data {
    int<lower=1> N;
    int<lower=1> M;
    matrix[N, M] X;
    vector[N]    Y;
}
parameters {
    real             a;
    vector[M]        b;
    real             r[N];
    real<lower=0>    s;
    real<lower=0>    s_r;
}
model {
    # regresion model with random effect
    for (i in 1:N)
        Y[i] ~ normal(a+X[i]*b+r[i], s);
    # prior distributions
    s ~ uniform(0, 1.0e+4);
    a ~ normal(39, 1.0e+4);
    for (i in 1:M)
        b[i] ~ normal(0, 1.0e+4);
    for (i in 1:N)
        r[i] ~ normal(0, 1/(s_r*s_r));
    # hierarchical prior distribution
    s_r ~ uniform(0, 1.0e+4);
}

