library(tidyverse)

dat <- read_csv("datasets/dat_sum_comma_3.csv") %>%
  rename(estimate = beta)


make_egm_plot(data = dat, factors_n = "three", es = FALSE, xlabel = "", ylabel = "", shapelabel = "")
