# input: cfscore estimates (baseline and robustness)
# output: validation measures for cfscore estimates
library(tidyverse)
library(data.table)

gg_summary <- function(data, x, y, fun = 'mean', size = 2, geom = 'point', color = "steelblue3", smooth = T, ...){
  plot <- data %>% 
    ggplot(
      aes(
        !!enquo(x),
        !!enquo(y)
      )
    ) +
    stat_summary_bin(
      fun = fun,
      size = size,
      geom = geom,
      color = color,
      ...
    )
  
  if(smooth == T){
    plot <- plot +
      geom_smooth(
        method = 'gam',
        formula = y ~ splines::bs(x, 1)
      ) 
  }
  
  return(plot)
}

# ---------------------------------------------------------------------------- #
# federal candidates
cfscore_fed <- fread("data/ideology/candidate_ideology_fed_state.csv") %>%
    select(
        cpf_candidate,
        cfscore_original = cfscore
    )

cfscore_fed_robust <- fread(
    "data/ideology/candidate_ideology_fed_state_robustness.csv"
    ) %>%
    select(
        cpf_candidate,
        cfscore_robust = cfscore
    )

cfscore_fed_validation <- cfscore_fed %>%
    left_join(
        cfscore_fed_robust,
        by = "cpf_candidate"
    )

cfscore_fed_validation %>%
    summarise(
        correlation = cor(cfscore_original, cfscore_robust, use = "complete.obs")
    )

cfscore_fed_validation %>%
    mutate(cfscore_original = round(cfscore_original, digits = 2)) %>%
    group_by(cfscore_original) %>%
    summarise(
        cfscore_robust = mean(cfscore_robust, na.rm = T),
        count = n()
    ) %>%
    ggplot(
        aes(cfscore_original, cfscore_robust)
    ) + 
    geom_point(aes(size = count), alpha = 0.05) +
    geom_smooth(col = "red3", method = "lm", alpha = 0.1) +
    theme_minimal() +
    ggsave(
        here::here("figs/robustness/cfscore_robustness.png")
    )