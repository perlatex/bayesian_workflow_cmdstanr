library(tidyverse) # 数据整理
library(cmdstanr)  # 更快地跑 Stan
library(posterior) # 优雅地整理后验
library(tidybayes) # 优雅地整理后验
library(bayesplot) # 诊断与模型比较
library(loo)       # 诊断与模型比较




######################################################################
# 1. 构造 Stan 数据

d <- readxl::read_excel("./data/simdata.xlsx")

stan_data <- list(
  N = nrow(d),
  K = 3,
  X = d %>% select(x1, x2, x3) %>% as.matrix(),
  y = d$y
)
######################################################################





######################################################################
# 2. 编译 
model <- cmdstan_model("./stan/logistics.stan")
######################################################################






######################################################################
# 3. 抽样 

fit <- model$sample(
  data            = stan_data,
  seed            = 1024,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 2000,
  iter_sampling   = 2000,
  adapt_delta     = 0.99,
  max_treedepth   = 12,
  refresh         = 500
)
######################################################################




######################################################################
# 4. 基本汇总 
fit$print()
fit$summary()

fit$print(
  variables = c("alpha", "beta")
)
######################################################################





######################################################################
# 5. HMC 诊断

fit$diagnostic_summary()

fit$cmdstan_diagnose()

fit$summary() %>%
  filter(rhat > 1.01 | ess_bulk < 400 | ess_tail < 400)

sampler_diag <- fit$sampler_diagnostics(format = "df")
sampler_diag
######################################################################





######################################################################
# 6. posterior 提取 draws

draws_df <- fit$draws(format = "df")

draws_df %>%
  select(.chain, .iteration, .draw, alpha, starts_with("beta")) %>%
  head()
######################################################################




######################################################################
# 7. tidybayes 提取参数

beta_draws <- fit %>%
  tidybayes::spread_draws(beta[k])

beta_draws


# 给 beta[k] 加上变量名：
X <- d %>% select(x1, x2, x3) 

beta_draws <- fit %>%
  tidybayes::spread_draws(beta[k]) %>%
  mutate(term = colnames(X)[k])

beta_draws %>%
  group_by(term) %>%
  summarise(
    mean = mean(beta),
    q05  = quantile(beta, 0.05),
    q50  = quantile(beta, 0.50),
    q95  = quantile(beta, 0.95),
    .groups = "drop"
  )

beta_draws %>%
  ggplot(aes(x = beta, y = term)) +
  ggdist::stat_halfeye() +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_minimal(base_size = 14)
######################################################################





######################################################################
# 8. bayesplot 轨迹图
draws_array <- fit$draws(
  variables = c("alpha", "beta")
)
bayesplot::mcmc_trace(draws_array)


bayesplot::mcmc_intervals(
  draws_array,
  pars = c("alpha", "beta[1]", "beta[2]", "beta[3]")
)
######################################################################




######################################################################
# 9. LOO 
loo_fit <- fit$loo(variables = "log_lik")
loo_fit
######################################################################




######################################################################
# 10. 后验预测检查
yrep <- fit$draws("y_rep", format = "draws_matrix")

dim(yrep)

# bayesplot::ppc_dens_overlay(
#   y    = d$y,
#   yrep = yrep[1:100, ]
# )

bayesplot::ppc_bars(
  y    = d$y,
  yrep = yrep[1:100, ]
)
######################################################################




######################################################################
# 11. 保存 

fit$save_object("fits/fit_logistics.rds")
fit$save_output_files(dir = "outputs", basename = "model")

# 下次读取使用：
fit2 <- readRDS("fits/fit_logistics.rds")

fit2$summary(
  variables = c("alpha", "beta")
)
######################################################################
