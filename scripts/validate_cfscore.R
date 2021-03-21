# input: cfscore estimates (baseline and robustness)
# output: validation measures for cfscore estimates
# also generate some intuition regarding the distribution of policy preferences
library(tidyverse)
library(data.table)
library(here)

gg_summary <- function(data, x, y, fun = "mean", size = 2, geom = "point", color = "steelblue3", smooth = T, ...) {
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

  if (smooth == T) {
    plot <- plot +
      geom_smooth(
        method = "gam",
        formula = y ~ splines::bs(x, 1)
      )
  }

  return(plot)
}

# ---------------------------------------------------------------------------- #
# federal candidates
cfscore_fed <- fread(
  "data/ideology/candidate_ideology_pooled_fed_state.csv",
  integer64 = "character"
) %>%
  select(
    cpf_candidate,
    cfscore_original = cfscore
  )

robustness_files <- sprintf(
  here("data/ideology/candidate_ideology_pooled_%s_percentile.csv"),
  c("5", "10")
) %>%
  set_names(c("5_percentile", "10_percentile"))

cfscore_fed_robust <- map_dfr(
  robustness_files,
  ~ fread(
    ., integer64 = "character"
  ) %>%
    select(
      cpf_candidate,
      cfscore_robust = cfscore
    ),
  .id = "percentile"
)

# demonstrate intuition of the cfscores
candidate_cpf <- list.files(
  here("data/candidate/fed_state"),
  pattern = "^candidate",
  full.names = TRUE
) %>%
  map_dfr(
    fread,
    integer64 = "character"
  )

# join with ideology scores
candidate_cfscore <- candidate_cpf %>%
  left_join(
    cfscore_fed,
    by = c("cpf_candidate")
  )

# ---------------------------------------------------------------------------- #
# generate visualization for robustness scores
cfscore_fed_validation <- cfscore_fed %>%
  left_join(
    cfscore_fed_robust,
    by = "cpf_candidate"
  )

cfscore_fed_validation %>%
  group_by(percentile) %>%
  summarise(
    correlation = cor(cfscore_original, cfscore_robust, use = "complete.obs")
  )

cfscore_fed_validation %>%
  mutate(cfscore_original = round(cfscore_original, digits = 2)) %>%
  group_by(percentile, cfscore_original) %>%
  summarise(
    cfscore_robust = mean(cfscore_robust, na.rm = T),
    count = n()
  ) %>%
  mutate(
    percentile = recode(
      percentile, 
      "5_percentile" = "5th percentile", 
      "10_percentile" = "10th percentile"
      )
  ) %>%
  ggplot(
    aes(cfscore_original, cfscore_robust)
  ) +
  geom_point(aes(size = count), alpha = 0.05) +
  geom_smooth(col = "red3", method = "lm", alpha = 0.1) +
  theme_minimal() +
  facet_wrap(percentile ~ .) +
  labs(x = "Full Sample Policy Position", "Subset Policy Position") +
  ggsave(
    here::here("../Presentation/figs/ideology/cfscore_robustness.pdf"),
    height = 4,
    width = 6
  )

# ---------------------------------------------------------------------------- #
election <- list.files(
  here("data/election/"),
  "^election",
  full.names = T
) %>% 
  map_dfr(
    . %>% 
      fread(., integer64 = "character")
  ) %>%
  filter(
    str_detect(candidate_status, "^deferido")
  )

# votes by candidate
candidate_vote <- election %>% 
  group_by(
    cpf_candidate,
    election_year
  ) %>% 
  summarise(
    vote = sum(vote, na.rm = T)
  ) %>%
  ungroup()

candidate_cfscore %>%
  ggplot() +
  geom_boxplot(
    aes(x = election_year, y = cfscore_original, group = election_year),
    outlier.shape = NA
  ) +
  coord_cartesian(
    ylim = c(-0.5, 0.1)
  )

candidate_cfscore %>% 
  mutate(
    highlight = if_else(
      party %in% c("pt", "pmdb", "psdb"), T, F
    )
  ) %>%
  ggplot() +
  geom_hline(
    yintercept = 0,
    col = "grey35",
    linetype = "dashed"
  ) +
  geom_point(
    data = . %>% 
      filter(highlight),
    aes(
      cfscore_original,
      0
    ),
    col = "darkorange",
    alpha = 0.4,
    position = position_jitter(width = 0, height = 0.005)
  ) +
  geom_text(
    data = . %>% 
      filter(highlight),
    aes(
      cfscore_original,
      0,
      label = party
    ),
    col = "brown3",
    alpha = 0.9,
    position = position_jitter(width = 0, height = 0.005)
  ) +
  scale_size(
    range = c(1, 12)
  ) +
  facet_grid(election_year ~ .) +
  coord_cartesian(
    ylim = c(-0.025, 0.025),
    xlim = c(-0.5, 0.2)
  ) +
  theme_minimal()

# ideology
party_ideology_candidate <- candidate_cfscore %>%
  filter(
    election_year > 2002
  ) %>% 
  left_join(
    candidate_vote,
    by = c("cpf_candidate", "election_year")
  ) %>% 
  mutate(
    highlight = if_else(
      party %in% c("pt", "pmdb", "psdb"), T, F
    )
  ) %>% 
  # group_by(
  #   election_year
  # ) %>% 
  mutate(
    total_vote = sum(vote, na.rm = T)
  ) %>% 
  group_by(
    party,
    highlight,
    add = T
  ) %>% 
  summarise(
    ideology = mean(cfscore_original, na.rm = T),
    party_share = sum(vote, na.rm = T)/unique(total_vote)
  )

party_ideology_candidate %>% 
  ggplot() +
  geom_hline(
    yintercept = 0,
    col = "grey35",
    linetype = "dashed"
  ) +
  geom_point(
    data = . %>% 
      filter(!highlight),
    aes(
      ideology,
      0,
      size = party_share
    ),
    col = "darkorange",
    alpha = 0.4,
    position = position_jitter(width = 0, height = 0.005)
  ) +
  geom_text(
    data = . %>% 
      filter(highlight),
    aes(
      ideology,
      0,
      label = party,
      size = party_share
    ),
    col = "brown3",
    alpha = 0.9,
    position = position_jitter(width = 0, height = 0.005)
  ) +
  scale_size(
    range = c(1, 12)
  ) +
  facet_grid(election_year ~ .) +
  coord_cartesian(
    ylim = c(-0.025, 0.025),
    xlim = c(-0.5, 0.2)
  ) +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  ) 
  # ggsave(
  #   here("Presentation/figs/ideology/party_ideology_pelotas.pdf")
  # )
