library(here)
library(data.table)
library(scales)
library(tidyverse)

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
        method = "lm"
      )
  }

  return(plot)
}

standardize_chars <- function(data) {
  data <- data %>%
    mutate_if(
      is.character,
      ~ stringi::stri_trans_general(., "latin-ascii") %>%
        str_to_lower()
    )

  return(data)
}

# ---------------------------------------------------------------------------- #
campaign <- fread(
  here("data/contribution/campaign_fed_state.csv.gz")
)

candidate_federal_2006 <- fread(
  here("data/candidate/fed_state/candidate_2006.csv")
) %>%
  select(
    uf = state,
    party,
    name = candidate_name,
    cpf_candidate
  )

# subset to individual donors, non partisan
# individual donors only use cpf (11 digits)
# and year 2010
campaign <- campaign %>%
  filter(
    str_count(cpf_cnpj_donor) == 11,
    election_year == 2010
  )

# aggregate campaign contribution by candidate
campaign_by_candidate <- campaign %>%
  group_by(cpf_candidate) %>%
  summarise(
    total_contribution = sum(value_receipt)
  )

load(
  here::here("data/spoils_of_victory/spoils_of_victory_replication_data.RData")
)

# extract deputados federais and
# 1) total contracts
# 2) total contracts for public works
candidate <- depfed_data %>%
  transmute(
    uf = as.character(uf),
    party = as.character(party),
    name,
    donations,
    contracts = contracts.0810,
    public_work_contracts = pw.con.0810
  ) %>%
  standardize_chars()

candidate <- candidate %>%
  inner_join(
    candidate_federal_2006,
    by = c("uf", "party", "name")
  )

# join contribution by candidate
contribution_by_candidate_2010 <- campaign_by_candidate %>%
  inner_join(
    candidate,
    by = "cpf_candidate"
  )

contribution_by_candidate_2010 %>%
  ggplot(
    aes(contracts, total_contribution)
  ) +
  geom_point(color = "steelblue3") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(x = "total contracts (log-scale)", y = "total donations (log-scale)") +
  coord_cartesian(ylim = c(10^2, 10^7)) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  ggsave(
    here("../Presentation/figs/ideology/validation_contracts_contribution.pdf")
  )