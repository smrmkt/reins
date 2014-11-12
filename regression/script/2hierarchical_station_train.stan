data {
    int<lower=1>            N;      # sample num
    int<lower=1>            M;      # independents' num
    int<lower=1>            N_T;    # train num
    int<lower=1>            N_S;    # station num
    matrix[N, M]            X;      # independents
    vector[N]               Y;      # dependent
    matrix[N_S, N_T]        ST;     # station-train matrix
    int<lower=1, upper=N_S> S[N];   # station
}
parameters {
    real             a;
    vector[M]        b;
    vector[N_T]      r_t;
    real             r_s[N_S];
    real<lower=0>    s;
    real<lower=0>    s_rs;
    real<lower=0>    s_rt;
}
model {
    # regresion model with random effect
    for (i in 1:N)
        Y[i] ~ normal(a+X[i]*b+r_s[S[i]], s);
    # prior distributions
    s ~ uniform(0, 1.0e+4);
    a ~ normal(39, 1.0e+4);
    for (i in 1:M)
        b[i] ~ normal(0, 1.0e+4);
    for (i in 1:N_S)
        r_s[i] ~ normal(ST[i]*r_t, s_rs);
    # hierarchical prior distribution
    s_rs ~ uniform(0, 1.0e+4);
    for (i in 1:N_T)
        r_t[i] ~ normal(0, s_rt);
    # 2 hierarchical prior distibution
    s_rt ~ uniform(0, 1.0e+4);
}

