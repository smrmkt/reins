data {
    int<lower=1> N;
    int<lower=1, upper=5> d_d[N];
    int<lower=1900, upper=2014> d_f[N];
    int<lower=1, upper=20> d_r[N];
    int<lower=1, upper=20> d_s[N];
    int<lower=0, upper=200> d_p[N];
}
parameters {
    real a;
    real b_d;
    real b_f;
    real b_r;
    real b_s;
    real b_p;
    real<lower=0> s;
}
model {
    for (i in 1:N)
        d_p[i] ~ normal(a + b_d*d_d[i] + b_f*d_f[i] + b_r*d_r[i] + b_s*d_s[i], s);
}
