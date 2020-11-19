library(here)
library(data.table)

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

# ---------------------------------------------------------------------------- #
campaign <- fread(
    here("data/contribution/campaign_fed_state.csv.gz")
)

load(
    here::here("data/spoils_of_victory/spoils_of_victory_replication_data.RData")
)

# extract deputados federais and
# 1) total contracts
# 2) total contracts for public works
candidate <- depfed_data %>%
    select(
        uf,
        party,
        name,
        donations,
        contracts = contracts.0810,
        public_work_contracts = pw.con.0810
    )


candidate %>% 
    gg_summary(
        log(contracts),
        donations
    )