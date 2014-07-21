data {
    int<lower=1>            N;    # sample num
    int<lower=1>            M;    # independents' num
    int<lower=1>            N_T;  # train num
    int<lower=1>            N_S;  # station num
    matrix[N, M]            X;    # independents
    vector[N]               Y;    # dependent
    int<lower=1, upper=N_T> T[N]; # train
    int<lower=1, upper=N_S> S[N]; # station
}
parameters {
    real             a;
    vector[M]        b;
    real             r_t[N_T];
    real             r_s[N_S];
    real<lower=0>    s;
    real<lower=0>    s_rs;
    real<lower=0>    s_rt;
}
model {
    # regresion model with random effect
    for (i in 1:N)
        Y[i] ~ normal(a+X[i]*b+r_t[T[i]]+r_s[S[i]], s);
    # prior distributions
    s ~ uniform(0, 1.0e+4);
    a ~ normal(39, 1.0e+4);
    for (i in 1:M)
        b[i] ~ normal(0, 1.0e+4);
    for (i in 1:N_T)
        r_t[i] ~ normal(0, s_rt);
    for (i in 1:N_S)
        r_s[i] ~ normal(0, s_rs);
    # hierarchical prior distribution
    s_rt ~ uniform(0, 1.0e+4);
    s_rs ~ uniform(0, 1.0e+4);
}

