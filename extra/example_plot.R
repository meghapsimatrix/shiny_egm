library(tidyverse)
library(ggstance)
library(plotly)


source("tidy_meta.R")
load("datasets/dat_egm_prg.Rdata")


dat <- dat_egm_prg %>%
  rename(es = g_yi_f,
         var = g_vi,
         study_id = StudyID,
         factor_1 = program,
         factor_2 = outcome, 
         factor_3 = Cn_aid_EGM_typ)


dat_sum <-
  dat %>%
  group_by(factor_1, factor_2, factor_3) %>%
  group_modify(~ tidy_meta(.x)) %>%
  ungroup()

suppressWarnings(mod <- rma.uni(yi = es,
                                vi = var,
                                data = dat))

res <- tidy(mod) %>%
  dplyr::select(estimate, SE = std.error) %>%
  mutate(df = NA,
         CI_L = mod$ci.lb,
         CI_U = mod$ci.ub) %>%
  mutate(method = "Univariate Random Effects")


#write_csv(dat_sum, "dat_sum_comma_3.csv")

p <- ggplot(dat_sum, aes(x = factor_1, y = factor_2, color = factor_3, size = n_studies)) +
  #geom_jitter(alpha = 0.5, position = position_dodge(width = 0.5)) + 
  geom_point(alpha = 0.5, aes(group = factor_3), 
             position = position_dodge(width= 0.5)) + 
  scale_size_identity() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  geom_text(aes(label = as.character(n_studies), group = factor_3), 
            size = 2.5, color = "black",
            position = position_dodge(width = .5)) +
  labs(caption = "Number of studies per combination of factors are overlaid.") +
  theme_minimal() +
  theme(legend.position = "bottom")


ggplotly(p, height = 800, width = 800, tooltip = c("x", "y", "factor_3","n_studies")) %>% 
  layout(legend = list(orientation = "h", x = 0.1, y = -0.2))


dat <- dropoutPrevention



dat <- dat %>%
  rename(es = g_yi_f,
         var = g_vi,
         es_id = ES_ID,
         study_id = StudyID,
         factor_1 = program,
         factor_2 = outcome, 
         factor_3 = Cn_aid_EGM_typ)


glimpse(dat)
