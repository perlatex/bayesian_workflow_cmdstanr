# ------------------------------------------------------------
#  模拟一份 Logistic 回归数据
# ------------------------------------------------------------

library(tidyverse)
library(writexl)


set.seed(1024)

N <- 500
K <- 3

X <- matrix(rnorm(N * K), nrow = N, ncol = K)

colnames(X) <- c("x1", "x2", "x3")

alpha_true <- -0.5
beta_true  <- c(1.2, -0.8, 0.5)

eta_true <- alpha_true + X %*% beta_true
p_true   <- plogis(eta_true)

y <- rbinom(N, size = 1, prob = p_true)



# ------------------------------------------------------------
# 保存
# ------------------------------------------------------------

d <- as_tibble(X) %>%
  mutate(
    id = row_number(),
    y  = y,
    .before = 1
  ) %>%
  select(id, x1, x2, x3, y)

d %>% write_xlsx("simulated_data.xlsx")
