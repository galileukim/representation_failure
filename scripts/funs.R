# import map --------------------------------------------------------------
library(sf)
library(tidyverse)
library(RColorBrewer)
library(scales)
library(here)
library(broom)
library(dplyr)

# functions ---------------------------------------------------------------
# clean theme
theme_clean <- theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "grey60"),
  text = element_text(
    family = "Roboto",
    color = "grey60",
    size = 10
  ),
  plot.title = element_text(size = 10),
  axis.text = element_text(color = "grey60"),
  axis.ticks = element_blank(),
  axis.line.x = element_blank(),
  axis.line.y = element_blank(),
  plot.margin = unit(rep(0.25, 4), "cm")
)

# tidy and ggcoef
tidycoef <- function(fit){
  tidy(fit) %>%
    filter(
      !str_detect(term, "Intercept|as.factor|Observation.Residual")
    ) %>%
    mutate(
      conf.low = estimate - 1.96*std.error,
      conf.high = estimate + 1.96*std.error
    ) %>%
    ggcoef(
      vline_color = "grey85",
      vline_linetype = "dotted",
      color = "steelblue3",
      sort = "ascending",
      errorbar_color = "steelblue3"
    ) +
    xlab("") +
    theme_clean
}

# add mandate year vertical lines
mandate_year <- geom_vline(
  xintercept = seq(2005, 2013, 4),
  linetype = "dotted",
  color = "grey65"
)

# summarize props with c.i.
summarise_mun <- function(data, var){
  summarise(
    .data = data,
    prop = mean(get(var), na.rm = T),
    n = n(),
    se = sqrt(prop*(1 - prop)/n),
    upper = prop + 1.96*se,
    lower = prop - 1.96*se
  )
}

# plot map
plot_map <- function(data, var, palette = "RdYlBu", limits = c(-100, 100)){
  ggplot() +
    geom_sf(
      data = data,
      aes(
        fill = .data[[var]]
      ),
      color = NA
    ) +
    geom_sf(
      data = map_state,
      color = "grey35",
      fill = NA
    ) +
    scale_fill_distiller(
      palette = palette,
      limits = limits,
      direction = -1,
      guide = "colourbar",
      breaks = pretty_breaks(),
      na.value = "grey80"
    ) +
    theme_void() +
    coord_sf() +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      text = element_text(family = "Roboto", size = 14)
    )
}

# ols
fit_felm <- function(
  repo,
  dv,
  predictor = c("coalition_share"),
  control = c("mayor_age", "as.factor(mayor_party)", "mayor_coalition_size", "mayor_campaign", "rais_mun_wage", "rais_mun_size", "effective_parties"),
  cluster = c("state + year"),
  data
){
  repo <- list()
  
  repo[["ols_basic"]] <- data %>%
    felm(
      formula = formula(
        paste(
          dv, "~", predictor,
          "|", cluster, "| 0"
        ),
        data = .
      )
    )
  
  repo[["ols_full"]] <- data %>%
    felm(
      formula = formula(
        paste(
          dv,
          "~",
          predictor,
          "+",
          str_c(control, collapse = "+"),
          "|",
          cluster,
          "| 0"
        )
      ),
      data = .
    )
  
  return(repo)
}

# logit
fit_logit <- function(
  repo,
  dv,
  predictor = c("coalition_share"),
  control = c("rais_edu", "rais_wage", "censo_log_pop", "mayor_reelected", "censo_median_wage", "mayor_campaign", "mayor_coalition_size", "as.factor(year)", "as.factor(state)"),
  data
){
  repo <- list()
  
  # repo[["logit_basic"]] <- data %>%
  #   glm(
  #     formula = formula(
  #       paste(dv, "~", predictor)
  #     ),
  #     family = binomial(),
  #     data = .
  #   )
  # 
  repo[["logit_full"]] <- data %>%
    glm(
      formula = formula(
        paste(dv, "~", predictor, "+", str_c(control, collapse = "+"))
      ),
      family = binomial(),
      data = .
    )
  
  return(repo)
}

fix_coalition <- function(data) {
  data <- data |>
    mutate(
      office = str_remove(position, "^vice-|^.*suplente ")
    ) |>
    left_join(
      coalition_lookup,
      by = c("election_year", "state", "office", "party")
    )

  # parties absent from the tallies (e.g. without votes) keep the raw label,
  # minus the parties that the tallies place in a coalition without them
  coalition_fallback <- data |>
    filter(is.na(coalition_fix) & !is.na(coalition)) |>
    distinct(election_year, state, office, party, coalition) |>
    mutate(member = str_split(coalition, " / ")) |>
    unnest(member) |>
    left_join(
      coalition_lookup |> rename(member = party, coalition_member = coalition_fix),
      by = c("election_year", "state", "office", "member")
    ) |>
    filter(
      is.na(coalition_member) |
      str_detect(
        paste0(" / ", coalition_member, " / "), fixed(paste0(" / ", party, " / "))
      )
    ) |>
    summarise(
      coalition_fallback = paste(member, collapse = " / "),
      .by = c(election_year, state, office, party, coalition)
    )

  # some raw labels are blank (e.g. unidos por goias, go 2014, whose registration
  # was denied). a party running alone takes its own name, and a party in a
  # named coalition takes the parties registered under that name in its
  # electoral unit
  data <- data |>
    mutate(
      unit = if_else(
        position %in% c("prefeito", "vereador"), as.character(cod_tse), state
      )
    )

  coalition_named <- data |>
    filter(!is.na(coalition_name) & coalition_name != "") |>
    summarise(
      coalition_named = paste(sort(unique(party)), collapse = " / "),
      .by = c(election_year, unit, office, coalition_name)
    )

  data |>
    left_join(
      coalition_fallback,
      by = c("election_year", "state", "office", "party", "coalition")
    ) |>
    left_join(
      coalition_named,
      by = c("election_year", "unit", "office", "coalition_name")
    ) |>
    mutate(
      coalition_fix = coalesce(coalition_fix, na_if(coalition_fallback, "")),
      coalition = case_when(
        !is.na(coalition_fix) ~ coalition_fix,
        coalition_type == "partido isolado" ~ party,
        !is.na(coalition_named) ~ coalition_named,
        T ~ coalition
      )
    ) |>
    select(-office, -unit, -coalition_fix, -coalition_fallback, -coalition_named)
}
