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
tidycoef <- function(fit, theme){
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
    theme
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
plot_map <- function(data, var, map_state, palette = "RdYlBu", limits = c(-100, 100)){
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

# electoral data cleaning -------------------------------------------------
# set malformed cpfs (not 11 digits, or all zeros) and placeholder electoral
# titles to missing
clean_ids <- function(data) {
  data |>
    mutate(
      cpf_candidate = if_else(
        nchar(cpf_candidate) == 11 & cpf_candidate != strrep("0", 11),
        cpf_candidate,
        NA
      ),
      elec_title = if_else(elec_title %in% c("", "#ni#", "-1"), NA, elec_title)
    )
}

# remove stray quotes from names
# e.g. """"carlos fernando d""""aguiar silva palacio"""" (2010)
clean_name <- function(name) {
  name |>
    str_remove_all('^"+|"+$') |>
    str_replace_all('"+', "'")
}

# tse code 91065 (boa esperanca do norte, mt) has no ibge code in 2002
fix_ibge <- function(data) {
  data |>
    mutate(
      cod_ibge_6 = if_else(cod_tse %in% 91065, 510183L, cod_ibge_6)
    )
}

# supplementary elections (e.g. for governor of amazonas in 2017) are recorded
# under the election year of the regular election they replace
is_regular_election <- function(election_type) {
  is.na(election_type) |
    election_type == "" |
    str_detect(election_type, "^eleic(oes|ao)( gerais| municipal)? [0-9]{4}$")
}

# electoral unit: municipal offices are contested by municipality, the
# presidency nationally, and the remaining offices by state
add_unit <- function(data) {
  data |>
    mutate(
      unit = case_when(
        position %in% c("prefeito", "vice-prefeito", "vereador") ~
          str_remove(as.character(cod_tse), "^0+"),
        position == "presidente" ~ "br",
        T ~ state
      )
    )
}

# coalition labels: the label in the party-level election data
# (`coalition_lookup`) when the party is there. otherwise, a party running alone
# takes its own name, other parties keep the raw label, and a blank label is
# rebuilt from the parties registered under the same coalition name in the
# electoral unit (e.g. unidos por goias, go 2014). running mates and senate
# substitutes take the label of the ticket's office
fix_coalition <- function(data, coalition_lookup) {
  data <- data |>
    add_unit() |>
    mutate(
      office = str_remove(position, "^vice-|^.*suplente ")
    )

  coalition_named <- data |>
    filter(!is.na(coalition_name) & coalition_name != "") |>
    summarise(
      coalition_named = paste(sort(unique(party)), collapse = " / "),
      .by = c(election_year, unit, office, coalition_name)
    )

  data |>
    left_join(
      coalition_lookup,
      by = c("election_year", "unit", "office", "party")
    ) |>
    left_join(
      coalition_named,
      by = c("election_year", "unit", "office", "coalition_name")
    ) |>
    mutate(
      coalition = case_when(
        !is.na(coalition_fix) ~ coalition_fix,
        coalition_type == "partido isolado" ~ party,
        !is.na(coalition) & coalition != "" ~ coalition,
        T ~ coalition_named
      )
    ) |>
    select(-unit, -office, -coalition_fix, -coalition_named)
}

# match candidacies to the election data on the columns in `by`, keeping only
# keys that the election data pair with a single outcome and status
match_election <- function(candidate, election, by) {
  election <- election |>
    filter(if_all(all_of(by), \(x) !is.na(x))) |>
    distinct(across(all_of(by)), outcome_election, status_election) |>
    filter(n() == 1, .by = all_of(by))

  candidate |>
    inner_join(election, by = by) |>
    select(row_id, outcome_election, status_election)
}

# write one file per election year, with the year in place of %s in `path`
write_by_year <- function(data, path) {
  for (year in unique(data$election_year)) {
    data |>
      filter(election_year == year) |>
      data.table::fwrite(sprintf(path, year))
  }
}
