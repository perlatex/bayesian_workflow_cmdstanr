data {
  int<lower=1> N;                         // 样本量
  int<lower=1> K;                         // 协变量个数
  matrix[N, K] X;                         // 设计矩阵
  array[N] int<lower=0, upper=1> y;       // 二分类结局
}

parameters {
  real alpha;                             // 截距
  vector[K] beta;                         // 回归系数
}

transformed parameters {
  vector[N] eta;
  vector[N] p;

  eta = alpha + X * beta;
  p   = inv_logit(eta);
}

model {
  // priors
  alpha ~ normal(0, 2.5);
  beta  ~ normal(0, 1.5);

  // likelihood
  // 写法一：普通写法
  y ~ bernoulli_logit(eta);

  // 写法二：更快的 GLM 写法，可替代上面这一行
  // target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);
}

generated quantities {
  array[N] int y_rep;
  vector[N] log_lik;

  for (i in 1:N) {
    y_rep[i]  = bernoulli_rng(p[i]);
    log_lik[i] = bernoulli_lpmf(y[i] | p[i]);
  }
}

