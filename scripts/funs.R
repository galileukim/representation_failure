# import map --------------------------------------------------------------
lapply(
  c(
    "rgdal",
    "tidyverse",
    "RColorBrewer",
    "scales",
    "here",
    "broom",
    "dplyr"
  ),
  require, character.only = T
)

# prep map
map_br <- readOGR(
  here("data/maps/"),
  "municipio"
)

map_br@data <- map_br@data %>%
  mutate(id = row.names(map_br))

# fortify
map_br <- map_br %>% 
  fortify %>% 
  left_join(., map_br@data, by = "id") %>%
  transmute(
    long, 
    lat, 
    group, 
    cod_ibge_6 = MUNICÍPI0 %>% as.character %>% as.integer
  ) 

# state boundaries
map_state <- readOGR(
  here("data/maps/"),
  "estados_2010"
) %>% 
  fortify
  
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
    geom_polygon(
      data = data, 
      aes(
        x = long, 
        y = lat, 
        group = group, 
        fill = get(var)
      ),
      color = NA
    ) +
    geom_polygon(
      data = map_state,
      aes(
        x = long, 
        y = lat, 
        group = group
      ),
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
    coord_map() +
    xlim(range(data$long)) + ylim(range(data$lat)) +
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
          dv, "~", predictor, "+", str_c(control, collapse = "+"),
          "|", cluster, "| 0"
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