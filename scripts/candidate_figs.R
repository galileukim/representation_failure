# set-up ------------------------------------------------------------------
pacman::p_load(
  "tidyverse",
  "data.table",
  "here",
  "GGally",
  "stargazer",
  "extrafont",
  "gghighlight",
  "ggridges",
  "kableExtra"
)

source(
  here("scripts/funs.R")
)

theme_set(theme_minimal())

# read-in -----------------------------------------------------------------
candidate <- list.files(
  here("data/candidate/fed_state"),
  pattern = "^candidate",
  full.names = T
) %>% 
  map_dfr(
    . %>% 
      fread(
        integer64 = "character",
        colClasses = c("birthyear" = "character")
      )
  ) %>% 
  filter(
    str_detect(candidate_status, "^deferido")
  )

# note: filter candidates who have been deferred

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

# municipal census
censo_mun <- fread(
  here("data/municipal/municipal_data.csv")
) %>% 
  filter(
    year == 2010
  )

# state census
censo_state <- fread(
  here("data/state/census_state.csv")
)

ideology <- fread(
  here("data/ideology/candidate_ideology_fed_state.csv"),
  integer64 = "character"
)

# join
candidate <- candidate %>% 
  left_join(
    ideology %>% 
      select(
        cpf_candidate,
        cfscore
      ),
    by = c("cpf_candidate")
  )

candidate_blp <- fread(
  here("../estimation/stacking.csv")
)

# cfscore validation
ideology_local <- fread(
  here("data/ideology/candidate_ideology_local.csv"),
  integer64 = "character"
)

ideology_fed_state <- fread(
  here("data/ideology/candidate_ideology_fed_state.csv"),
  integer64 = "character"
)

candidate_local <- list.files(
  here("data/candidate/local"),
  pattern = "^candidate",
  full.names = T
) %>% 
  map_dfr(
    . %>% 
      fread(integer64 = "character") %>% 
      mutate(birthyear = as.integer(birthyear))
  ) %>% 
  filter(
    str_detect(position, "prefeito|vereador")
  )

candidate_fed_state <- list.files(
  here("data/candidate/fed_state"),
  pattern = "^candidate",
  full.names = T
) %>% 
  map_dfr(
    . %>% 
      fread(integer64 = "character") %>% 
      mutate(birthyear = as.integer(birthyear))
  )

# join dataset
candidate_local <- candidate_local %>% 
  left_join(
    ideology_local %>% 
      select(cpf_candidate, cfscore),
    by = c("cpf_candidate")
  )

candidate_fed_state <- candidate_fed_state %>% 
  left_join(
    ideology_fed_state %>% 
      select(cpf_candidate, cfscore),
    by = c("cpf_candidate")
  )

# note we subset to mayors
party_local <- candidate_local %>% 
  filter(
    position == "prefeito"
  ) %>% 
  group_by(party, election_year) %>% 
  summarise(
    ideology_local = mean(cfscore, na.rm = T),
    n = n()
  ) %>% 
  filter(n > 100) %>% 
  ungroup() %>% 
  mutate(
    election_year = election_year + 2
  )

# note we subset to federal deputies
party_fed_state <- candidate_fed_state %>% 
  group_by(party, election_year) %>% 
  summarise(
    ideology_fed_state = mean(cfscore, na.rm = T),
    n = n()
  ) %>% 
  filter(
    n > 100
  ) %>% 
  ungroup()

party_ideology <- inner_join(
  party_local,
  party_fed_state,
  by = c("party", "election_year")
) %>% 
  mutate_at(
    vars(starts_with("ideology")),
    scale
  )

# campaign
campaign <- list.files(
  here("data/contribution/"),
  "^campaign",
  full.names = T
) %>% 
  map(
    . %>% 
      fread(
        colClasses = c("cpf_candidate" = "character"),
        nThread = parallel::detectCores() - 1
      )
  ) %>% 
  rbindlist(
    fill = T
  )

# latinobarometer
latinobar <- fread(
  here("data/ideology/latinobarometer_ideology.csv")
)

latinobar <- latinobar %>% 
  melt(
    id.vars = "year",
    variable.name = "ideology_level",
    variable.factor = F
  ) %>% 
  mutate(
    ideology_level = as.integer(ideology_level) - 5
  ) %>% 
  filter(
    !is.na(ideology_level),
    year >= 1995
  ) %>% 
  arrange(
    year,
    ideology_level
  )

states <- fread(
  here("data/identifiers/state_names.csv")
)

# obtain legislative ideal points from Zucco and Lauderdale (2011)
# Ideal Point Estimates of Brazilian Legislators (2003-2010)
legislative <- fread(
  here("data/ideology/legislative_ideology.csv")
) %>% 
  rename(
    name = candidate_name
  )

# fix deputado names
deputado <- fread(
  here("data/candidate/deputados.csv")
) 

# fix cols for deputado
deputado <- deputado %>% 
  transmute(
    candidate_name = nomeCivil,
    birthyear = str_extract(dataNascimento, "[0-9]+") %>% 
      as.numeric,
    mun_birth_name = municipioNascimento,
    gender = if_else(siglaSexo == "M", 2, 4),
    name = nome,
    state_birth = ufNascimento
  ) %>% 
  mutate_if(
    is.character,
    str_to_lower
  ) %>% 
  mutate_if(
    is.character,
    funs(stringi::stri_trans_general(., id ="latin-ascii"))
  )

# elected candidates
cands_elected <- list.files(
   here("data/candidate/fed_state/"),
   pattern = "200[2-6].csv$",
   full.names = T
) %>% 
  map_dfr(
    function(x) fread(x, integer64 = "character")
  ) %>% 
  filter(
    str_detect(outcome, "^eleito|^media")
  ) %>% 
  mutate(
    birthyear = as.numeric(election_year - age)
  ) %>% 
  left_join(
    candidate_fed_state %>% 
      select(
        cpf_candidate,
        election_year,
        cfscore
      ),
    by = c("cpf_candidate", "election_year")
  )

# link data
link_deputado <- fastLink::fastLink(
  cands_elected,
  deputado,
  varnames = c("candidate_name", "mun_birth_name", "birthyear"),
  stringdist.match = c("candidate_name", "mun_birth_name"),
  numeric.match = c("birthyear")
)

# join cols
cands_elected <- bind_cols(
  cands_elected %>% 
    slice(
      link_deputado$matches$inds.a
    ) %>% 
    select(
      "candidate_name", "party", "state", "cfscore", "election_year"
    ),
  deputado %>% 
    slice(
      link_deputado$matches$inds.b
    ) %>% 
    select(name)
)

# join with legislative ideal points data
link_ideal <- fastLink::fastLink(
  cands_elected,
  legislative,
  varnames = c("name", "state", "party", "election_year"),
  stringdist.match = c("name")
)
  
# join cols
cands_ideal <- bind_cols(
  cands_elected %>% 
    slice(
      link_ideal$matches$inds.a
    ) %>% 
    select(
      "candidate_name", "party", "state", "cfscore", "election_year"
    ),
  legislative %>% 
    slice(
      link_ideal$matches$inds.b
    ) %>% 
    select(
      ideology
    )
)

# mapping demographics ----------------------------------------------------
map_br <- map_br %>% 
  left_join(
    censo_mun,
    by = c("cod_ibge_6")
  )

# plot national map
map_state %>% 
  ggplot() +
  geom_polygon(
    aes(
      long,
      lat,
      group = group,
      fill = id
    ),
    color = "grey45",
    size = 0.6
  ) +
  scale_fill_hue(
    l = 63,
    c = 68,
    h = c(0, 360)
  ) +
  theme_void() +
  coord_map() +
  xlim(range(map_state$long)) +
  ylim(range(map_state$lat)) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    text = element_text(family = "Roboto", size = 14)
  ) +
  ggsave("Presentation/figs/demo/map_br.png")

# plot maps
vars <- c("median_wage", "lit_rate", "rural", "sanitation")
map <- list()
for(i in 1:length(vars)){
  map[[i]] <- plot_map(
    map_br,
    vars[i],
    limits = NULL
  ) +
    ggtitle(
      vars[i]
    )
  
  ggsave(
    here(
      paste0("Presentation/figs/demo/map_", vars[i], ".png")
    ),
    map[[i]]
  )
}

# campaign ----------------------------------------------------------------
campaign %>%
  filter(
    election_year > 2002 & election_year < 2016
  ) %>% 
  group_by(election_year) %>% 
  summarise(
    total = sum(value_receipt)/10^8
  ) %>% 
  ggplot(
    aes(
      election_year,
      total
    )
  ) +
  geom_line(
    col = "steelblue3"
  ) +
  geom_point(
    col = "steelblue3",
    size = 3
  ) +
  xlab(
    "Year"
  ) +
  ylab(
    "Total (millions R$)"
  ) +
  theme(
    panel.grid = element_blank()
  ) +
  ggsave(
    here("Presentation/figs/campaign/campaign_total.png")
  )

campaign %>%
  filter(
    election_year > 2002 & election_year < 2016
  ) %>%
  ggplot() +
  geom_density_ridges(
    aes(
      x = log(value_receipt + 0.1),
      y = as.factor(election_year)
    ),
    bins = 50,
    stat = "binline",
    fill = "darkorange",
    col = "steelblue3"
  ) +
  xlab("Logged donations") +
  ylab("Year") +
  theme(
    panel.grid = element_blank()
  ) +
  ggsave(
    here("Presentation/figs/campaign/campaign_distribution.png")
  )

# party system ------------------------------------------------------------
# party vote share by electoral cycle
election_party <- election %>% 
  group_by(
    election_year
  ) %>% 
  mutate(
    total_vote = sum(vote, na.rm = T)
  ) %>% 
  group_by(
    party,
    election_year
  ) %>% 
  summarise(
    num_candidates = length(unique(cpf_candidate)),
    party_share = sum(vote, na.rm = T)/mean(total_vote) * 100
  ) %>% 
  ungroup()

election_party %>% 
  filter(
    election_year > 2002
  ) %>% 
  group_by(election_year) %>% 
  top_n(5, party_share) %>% 
  ggplot() +
  geom_text(
    aes(
      election_year,
      party_share,
      label = party,
      col = party
    ),
    position= position_jitter(
      width = 0,
      height = 3
    ),
    size = 5
  ) +
  scale_color_brewer(
    palette = "Dark2"
  ) +
  labs(
    x = "Year",
    y = "Party vote share"
  ) +
  scale_x_continuous(
    breaks = seq(2006, 2014, 4),
    labels = seq(2006, 2014, 4),
    minor_breaks = NULL
  ) +
  theme_minimal(
    base_size = 10
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  ggsave(
    here("Presentation/figs/party/party_vote_share.png")
  )

# party vote share by state
# party vote share by electoral cycle
election_party_state <- election %>% 
  group_by(
    election_year,
    state
  ) %>% 
  mutate(
    total_vote = sum(vote, na.rm = T)
  ) %>% 
  group_by(
    party,
    election_year,
    state
  ) %>% 
  summarise(
    num_candidates = length(unique(cpf_candidate)),
    party_share = sum(vote, na.rm = T)/mean(total_vote, na.rm = T) * 100
  )

# create herfindahl index of electoral competitiveness
herfin <- election_party_state %>% 
  mutate(
    party_share = party_share/100
  ) %>% 
  group_by(
    state,
    election_year
  ) %>% 
  summarise(
    h_index = sum(party_share^2)
  )

# join with state demographics
herfin <- herfin %>% 
  filter(
    election_year < 2010
  ) %>% 
  left_join(
    censo_state %>% 
      filter(year == 2000),
    by = c("state")
  ) %>% 
  bind_rows(
    herfin %>% 
      filter(
        election_year >= 2010
      ) %>% 
      left_join(
        censo_state %>% 
          filter(year == 2010),
        by = c("state")
      )
  )

# plot
herfin %>% 
  ggplot(
    aes(
      log(median_wage),
      h_index
    )
  ) +
  geom_smooth(
    col = "steelblue3",
    method = "lm"
  ) +
  geom_point(
    col = "steelblue3",
    alpha = 0.2
  ) +
  coord_cartesian(
    ylim = c(0.05, 0.2)
  ) +
  theme_minimal() +
  ggsave(
    here("Presentation/figs/party/herfindahl_median_wage.pdf")
  )

herfin %>% 
  mutate(
    region = str_sub(uf, 1, 1)
  ) %>% 
  lm(
    h_index ~ log(median_wage) + log(pop) + lit_rate + rural + as.factor(region),
    data = .
  ) %>% 
  tidycoef +
  ggsave(
    here("Presentation/figs/party/herfindahl_lm.pdf")
  )

# party vote share by electoral cycle and state
election_party_state %>%
  filter(
    election_year == 2014
  ) %>% 
  group_by(
    state
  ) %>% 
  top_n(
    5,
    party_share
  ) %>% 
  ggplot() +
  geom_text(
    aes(
      party_share,
      reorder(state, desc(state)),
      label = party,
      col = party
    ),
    size = 3,
    alpha = 0.8
  ) +
  labs(
    x = "Party vote share",
    y = "State"
  ) +
  scale_x_continuous(
    breaks = seq(0, 30, 5),
    labels = seq(0, 30, 5),
    minor_breaks = NULL
  ) +
  scale_color_brewer(
    palette = "Set1"
  ) +
  theme_minimal(
    base_size = 10
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  ) +
  ggtitle(
    "Party vote share by state (2014)"
  ) +
  geom_vline(
    xintercept = 20,
    linetype = "dashed",
    color = "grey65"
  ) +
  ggsave(
    here("Presentation/figs/party_vote_share_state.pdf"),
    height = 3.5,
    width = 4
  )

# party vote share by mun
election_party_mun <- election %>% 
  group_by(
    election_year,
    state,
    cod_ibge_6
  ) %>% 
  mutate(
    total_vote = sum(vote, na.rm = T)
  ) %>% 
  group_by(
    party,
    add = T
  ) %>% 
  summarise(
    party_share = sum(vote, na.rm = T)/mean(total_vote, na.rm = T) * 100
  ) %>% 
  left_join(
    censo_mun,
    by = c("cod_ibge_6", "state")
  ) %>% 
  ungroup() %>% 
  mutate(
    state = as.factor(state),
    election_year = as.factor(election_year),
    party = as.factor(party)
  )

# run regression separately for each major party
fit_party <- election_party_mun %>% 
  filter(
    party %in% c("pt", "psdb", "pmdb")
  ) %>% 
  mutate_if(
    is.numeric,
    scale
  ) %>% 
  nest(
    -party
  ) %>% 
  mutate(
    fit = map(
      data,
      ~ lm(
        party_share ~ as.factor(state) + as.factor(election_year) + 
          lit_rate + median_wage + age + female + rural + high_school, 
        data = .
      )
    ),
    tidied = map(fit, tidy, conf.int = T)
  ) %>% 
  unnest(tidied)

# plot the coefficients with confidence intervals
fit_party %>% 
  mutate(
    term = as.factor(term)
  ) %>%
  filter(
    !str_detect(term, "Intercept|state|election")
  ) %>% 
  ggplot(
    aes(
      term,
      estimate,
      group = party,
      color = party
    )
  ) +
  geom_point(
    position = position_dodge(0.4),
    size = 2
  ) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    linetype = "solid",
    position = position_dodge(0.4)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    col = "grey65"
  ) +
  scale_color_brewer(
    palette = "Set1"
  ) +
  theme_clean +
  ggsave(
    here("Presentation/figs/party/party_share_fit.pdf")
  )
# note how much the pmdb benefits from poor and uneducated voters! it's insane.

# ideology ----------------------------------------------------------------
# votes by candidate
candidate_vote <- election %>% 
  group_by(
    cpf_candidate,
    election_year
  ) %>% 
  summarise(
    vote = sum(vote, na.rm = T)
  )

# ideology
party_ideology_candidate <- candidate %>%
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
  group_by(
    election_year
  ) %>% 
  mutate(
    total_vote = sum(vote, na.rm = T)
  ) %>% 
  group_by(
    party,
    highlight,
    add = T
  ) %>% 
  summarise(
    ideology = mean(cfscore, na.rm = T),
    party_share = sum(vote, na.rm = T)/unique(total_vote)
  )

party_ideology_candidate %>% 
  filter(election_year == 2014) %>%
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
  # geom_text(
  #   data = . %>% 
  #     filter(highlight),
  #   aes(
  #     ideology,
  #     0,
  #     label = party,
  #     size = party_share
  #   ),
  #   col = "brown3",
  #   alpha = 0.9,
  #   position = position_jitter(width = 0, height = 0.005)
  # ) +
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

# party_ideology %>% 
#   filter(election_year == 2014) %>%
#   ggplot() +
#   geom_hline(
#     yintercept = 0,
#     col = "grey35",
#     linetype = "dashed"
#   ) +
#   geom_text(
#     data = . %>% 
#       filter(!highlight),
#     aes(
#       ideology,
#       0,
#       label = party,
#       size = party_share
#     ),
#     col = "grey35",
#     alpha = 0.4,
#     position = position_jitter(width = 0, height = 0.005)
#   ) +
#   geom_text(
#     data = . %>% 
#       filter(highlight),
#     aes(
#       ideology,
#       0,
#       label = party,
#       size = party_share
#     ),
#     col = "brown3",
#     alpha = 0.9,
#     position = position_jitter(width = 0, height = 0.008)
#   ) +
#   scale_size(
#     range = c(1, 10)
#   ) +
#   facet_grid(election_year ~ .) +
#   coord_cartesian(
#     ylim = c(-0.025, 0.025),
#     xlim = c(-0.5, 0.2)
#   ) +
#   theme(
#     legend.position = "none",
#     axis.line.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     panel.grid = element_blank()
#   ) +
#   ggsave(
#     here("Presentation/figs/ideology/party_ideology_labels.pdf")
#   )

# party ideology vs. district income
candidate %>% 
  filter(
    party == "pmdb"|party == "psdb"|party == "pt"
  ) %>% 
  mutate(
    year = if_else(
      election_year < 2010, 2000, 2010
    )
  ) %>% 
  group_by(
    party,
    state
  ) %>% 
  summarise(
    ideology = mean(cfscore, na.rm = T)
  ) %>% 
  left_join(
    censo_state,
    by = c("state")
  ) %>% 
  ggplot(
    aes(
      log(median_wage),
      ideology
    )
  ) +
  geom_smooth(
    method = "lm",
    formula = formula(y ~ x + I(x^2)),
    col = "steelblue3"
  ) +
  geom_point(
    alpha = 0.5,
    col = "steelblue3"
  ) +
  facet_grid(. ~ party) +
  coord_cartesian(
    ylim = c(-1, 0.5)
  ) +
  ggsave(
    here("Presentation/figs/ideology/party_ideology_median_wage.pdf")
  )

# party ideology vs. district rural
candidate %>% 
  filter(
    party == "pmdb"|party == "psdb"|party == "pt"
  ) %>% 
  mutate(
    year = if_else(
      election_year < 2010, 2000, 2010
    )
  ) %>% 
  group_by(
    party,
    state
  ) %>% 
  summarise(
    ideology = mean(cfscore, na.rm = T)
  ) %>% 
  left_join(
    censo_state,
    by = c("state")
  ) %>% 
  ggplot(
    aes(
      rural,
      ideology
    )
  ) +
  geom_smooth(
    method = "lm",
    formula = formula(y ~ x),
    col = "steelblue3"
  ) +
  geom_point(
    alpha = 0.5,
    col = "steelblue3"
  ) +
  facet_grid(. ~ party) +
  coord_cartesian(
    ylim = c(-1, 0.5)
  ) +
  ggsave(
    here("Presentation/figs/ideology/party_ideology_rural.pdf")
  )

# party ideology vs. district education
candidate %>% 
  filter(
    party == "pmdb"|party == "psdb"|party == "pt"
  ) %>% 
  mutate(
    year = if_else(
      election_year < 2010, 2000, 2010
    )
  ) %>% 
  group_by(
    party,
    state
  ) %>% 
  summarise(
    ideology = mean(cfscore, na.rm = T)
  ) %>% 
  left_join(
    censo_state,
    by = c("state")
  ) %>% 
  ggplot(
    aes(
      lower_school,
      ideology
    )
  ) +
  geom_smooth(
    method = "lm",
    formula = formula(y ~ x + I(x^2)),
    col = "steelblue3"
  ) +
  geom_point(
    alpha = 0.5,
    col = "steelblue3"
  ) +
  facet_grid(. ~ party) +
  coord_cartesian(
    ylim = c(-1, 0.5)
  ) +
  ggsave(
    here("Presentation/figs/ideology/party_ideology_lower_school.pdf")
  )

# interparty heterogeneity
candidate %>%
  group_by(
    party
  ) %>% 
  mutate(
    num_candidate = n()
  ) %>% 
  ungroup() %>% 
  ggplot(
    aes(
      y = cfscore,
      x = reorder(party, num_candidate)
    )
  ) +
  geom_boxplot(
    outlier.shape = NA,
    col = "steelblue3"
  ) +
  gghighlight(
    str_detect(party, "\\bpmdb\\b|\\bpsdb\\b|^pt$")
  ) +
  coord_flip() +
  theme(
    panel.grid = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  ) + 
  ggsave(
    "Presentation/figs/ideology/party_ideology.pdf"
  )

# elected vs. non-elected candidates
candidate %>% 
  mutate(
    elected = if_else(
      str_detect(outcome, "^eleito"),
      1, 0
    )
  ) %>% 
  ggplot() +
  geom_boxplot(
    aes(
      as.factor(elected),
      cfscore
    ),
    outlier.shape = NA
  ) +
  coord_cartesian(
    ylim = c(-1.25, 1.25)
  ) +
  scale_x_discrete(
    labels = c("non-elected", "elected")
  ) +
  theme(
    axis.title.x = element_blank()
  ) +
  ggtitle(
    "Ideology of elected and non-elected candidates"
  ) +
  ggsave(
    here("Presentation/figs/ideology/candidates_elected_ideology.pdf")
  )

# local vs. federal ideology
party_ideology %>%
  filter(
    election_year > 2002
  ) %>% 
  ggplot() + 
  geom_text(
    aes(
      ideology_local,
      ideology_fed_state,
      label = party,
      col = as.factor(election_year),
      group = as.factor(election_year)
    ),
    size = 4
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    size = 0.5,
    col = "grey75"
  ) +
  theme_minimal() +
  coord_cartesian(
    xlim = c(-1, 1),
    ylim = c(-1, 1)
  ) +
  xlab("Ideology (local)") +
  ylab("Ideology (federal)") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  scale_color_brewer(
    palette = "Set2"
  ) +
  facet_grid(
    election_year ~ .
  ) 
  # ggsave(
  #   here("Presentation/figs/ideology/party_ideology_deputy_mayor.png")
  # )

party_ideology %>% 
  ggplot(
    aes(
      ideology_local,
      ideology_fed_state
    )
  ) +
  geom_smooth(
    method = "lm",
    size = 0.5,
    fullrange = T,
    col = "cadetblue3"
  ) +
  geom_point(
    aes(
      group = as.factor(election_year),
      col = as.factor(election_year)
    ),
    alpha = 0.8,
    size = 2
  ) +
  coord_cartesian(
    xlim = c(-2, 2),
    ylim = c(-2, 2)
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_color_brewer(
    palette = "Dark2"
  ) +
  ylab("Federal ideology") +
  xlab("Local ideology") +
  ggtitle(
    "Linear regression: federal ideology scores vs. "
  ) +
  coord_cartesian(
    xlim = c(-1, 1),
    ylim = c(-1, 1)
  )
  ggsave(
    here("../Presentation/figs/ideology/reg_ideology.png")
  )

# latinobarometer
latinobar %>% 
  group_by(year) %>% 
  summarise(
    mean_ideology = weighted.mean(ideology_level, value)
  ) %>% 
  ggplot(
    aes(
      x = year,
      y = mean_ideology
    )
  ) +
  geom_smooth(
    method = "lm",
    col = "coral2"
  ) +
  geom_point(
    col = "cadetblue3",
    size = 3,
    alpha = 0.8
  ) +
  theme(
    panel.grid = element_blank()
  ) +
  xlab("Year") +
  ylab("Ideology") +
  ggtitle(
    "Mean ideological score for Latinobarometer respondents"
  ) +
  ggsave(
    here("Presentation/figs/ideology/reg_electorate_ideology.png")
  )
  

latinobar %>% 
  filter(
    year %in% seq(2000, 2015, 5)
  ) %>% 
  ggplot() +
   geom_col(
     aes(
       x = ideology_level,
       y = value,
       col = as.factor(year),
       fill = as.factor(year)
     )
   ) +
  scale_color_brewer(
    palette = "Set2"
  ) +
  scale_fill_brewer(
    palette = "Set2"
  ) +
  facet_wrap(
    ~ year,
    ncol = 2
  ) +
  theme(
    panel.grid  = element_blank(),
    legend.position = "none"
  ) +
  ggsave(
    here("Presentation/figs/ideology/electorate_ideology.png")
  )

# party map
cands_ideal %>%
  group_by(
    party
  ) %>% 
  summarise(
    cfscore = mean(cfscore, na.rm = T),
    ideology = mean(ideology, na.rm = T),
    n = n()
  ) %>% 
  ungroup() %>% 
  filter(
    n > 10
  ) %>% 
  ggplot() +
  geom_text(
    aes(
      cfscore, 
      ideology, 
      group = party,
      label = party
    ),
    size = 6,
    col = "grey50"
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  theme_minimal() +
  coord_cartesian(
    xlim = c(-1, 1),
    ylim = c(-1, 1)
  ) +
  theme(
    legend.position = "bottom"
  ) +
  ggtitle(
    "Ideology vs. CF-score: Legislative Parties (2003-2010)"
  ) +
  ggsave(
    here("Presentation/figs/ideology/ideology_cfscore.png")
  )

# valence -----------------------------------------------------------------
# create candidate state table
candidate_state <- candidate %>% 
  transmute(
    election_year,
    state,
    higher_edu = if_else(edu == "higher education", 1, 0),
    female = if_else(gender == 4, 1, 0),
    incumbent,
    politician = if_else(occupation == "politician", 1, 0),
    business = if_else(occupation == "business", 1, 0),
    white_collar = if_else(occupation == "white-collar", 1, 0)
  ) %>% 
  group_by(
    election_year,
    state
  ) %>%
  summarise_all(
    mean, na.rm = T
  ) %>% 
  ungroup()

candidate_state %>% 
  gather(
    term, value, 
    -state, -election_year,
  ) %>%
  ggplot(
    aes(
      reorder(term, desc(term)),
      value
    )
  ) +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.35
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    col = "grey55"
  ) +
  coord_flip() +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  ggtitle(
    "Distribution of valence characteristics across states (pooled)"
  ) +
  ggsave(
    here("Presentation/figs/candidate/candidate_valence_states.pdf")
  )

# candidate valence characteristics
candidate_valence_state <- candidate %>% 
  filter(
    election_year == 2014
  ) %>% 
  transmute(
    election_year,
    state,
    higher_edu = if_else(edu == "higher education", 1, 0),
    female = if_else(gender == 4, 1, 0),
    incumbent,
    politician = if_else(occupation == "politician", 1, 0),
    government = if_else(occupation == "government", 1, 0),
    business = if_else(occupation == "business", 1, 0),
    white_collar = if_else(occupation == "white-collar", 1, 0),
    elected = if_else(str_detect(outcome, "^eleito"), 1, 0)
  ) %>% 
  group_by(
    state
  ) %>% 
  summarise_all(
    mean, na.rm = T
  ) %>% 
  ungroup()

candidate_valence_state %>% 
  gather(
    term, value, 
    -state, -election_year, -elected
  ) %>% 
  filter(
    term != "time_legis"
  ) %>% 
  ggplot(
    aes(
      reorder(term, desc(term)),
      value
    )
  ) +
  geom_point(
    size = 3,
    alpha = 0.25
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    col = "grey55"
  ) +
  coord_flip() +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  ggtitle(
    "Proportion of valence characteristics across\n states (2014)"
  )  +
  ggsave(
    here("Presentation/figs/candidate/valence_state.pdf"),
    device = cairo_pdf
  )

# candidate elected valence
candidate_elected <- candidate %>% 
  transmute(
    election_year,
    state,
    higher_edu = if_else(edu == "higher education", 1, 0),
    female = if_else(gender == 4, 1, 0),
    incumbent,
    politician = if_else(occupation == "politician", 1, 0),
    government = if_else(occupation == "government", 1, 0),
    business = if_else(occupation == "business", 1, 0),
    white_collar = if_else(occupation == "white-collar", 1, 0),
    elected = if_else(str_detect(outcome, "^eleito"), "elected", "non-elected") %>% 
      as.character
  ) %>% 
  group_by(
    election_year,
    state
  ) %>% 
  group_by(
    elected,
    add = T
  ) %>% 
  summarise_all(
    mean, na.rm = T
  ) %>% 
  ungroup()

candidate_elected %>% 
  gather(
    term, value, 
    -state, -election_year, -elected
  ) %>% 
  filter(
    term != "time_legis"
  ) %>% 
  ggplot(
    aes(
      reorder(term, desc(term)),
      value
    )
  ) +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.35
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    col = "grey55"
  ) +
  coord_flip() +
  facet_grid(
    . ~ elected
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  ggtitle(
    "Proportion of individual characteristics across\n elected and non-elected candidates"
  )  +
  ggsave(
    here("Presentation/figs/candidate/valence_elected_nonelected.pdf"),
    device = cairo_pdf
  )

candidate_elected %>%
  arrange(
    election_year,
    state,
    elected
  ) %>% 
  group_by(
    election_year,
    state
  ) %>% 
  summarise_at(
    .funs = list(difference = ~ .[elected == "elected"] - .[elected == "non-elected"]),
    .vars = vars(higher_edu:white_collar)
  ) %>% 
  ungroup() %>% 
  gather(
    term, value, 
    -state, -election_year
  ) %>% 
  ggplot(
    aes(
      reorder(term, desc(term)),
      value
    )
  ) +
  geom_point(
    alpha = 0.25
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    col = "grey55"
  ) +
  coord_flip() +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  ggtitle(
    "Difference in proportion of valence characteristics across\n elected and non-elected candidates"
  )  +
  ggsave(
    here("Presentation/figs/candidate/valence_selection.pdf"),
    device = cairo_pdf
  )

# # linear fit candidate vote share
# election %>% 
#   group_by(
#     state,
#     election_year
#   ) %>% 
#   mutate(
#     total_vote = sum(vote, na.rm = T)
#   ) %>% 
#   group_by(
#     cpf_candidate,
#     add = T
#   ) %>% 
#   summarise(
#     vote_share = sum(vote, na.rm = T)/mean(total_vote)
#   ) %>% 
#   left_join(
#     candidate %>% 
#       filter(
#         position == "deputado federal"
#       ) %>% 
#       transmute(
#         cpf_candidate,
#         election_year,
#         age,
#         higher_edu = if_else(edu == "higher education", 1, 0),
#         female = if_else(gender == 4, 1, 0),
#         times_legis,
#         incumbent,
#         politician = if_else(occupation == "politician", 1, 0),
#         cfscore = cfscore/10
#       ) %>% 
#       mutate_if(
#         is.double,
#         scale
#       ),
#     by = c("cpf_candidate", "election_year")
#   ) %>% 
#   lm(
#     log(vote_share) ~ as.factor(state) + as.factor(election_year) +
#       cfscore + female + higher_edu + politician +
#       incumbent + age,
#     data = .
#   ) %>%
#   tidycoef +
#   ggtitle(
#     "Logged vote share of candidates vs. valence"
#   ) +
#   theme( 
#     axis.title.y = element_blank()
#   ) +
#   ggsave(
#     "Presentation/figs/candidate/candidate_valence_fit.pdf",
#     device = cairo_pdf
#   )

# state demographics vs. valence
candidate_state_demo <- candidate_state %>% 
  mutate(
    year = if_else(election_year < 2010, 2000, 2010)
  ) %>% 
  left_join(
    censo_state %>% 
      rename(
        female_pop = female
      ),
    by = c("state", "year")
  ) 

# edu vs. edu
candidate_state_demo %>% 
  ggplot(
    aes(
      higher_education,
      higher_edu
    )
  ) +
  geom_smooth(
    method = "lm",
    col = "darkorange2"
  ) +
  geom_point(
    col = "steelblue3",
    alpha = 0.35
  ) +
  xlab("education (state)") +
  ylab("education (candidates)") +
  coord_cartesian(
    xlim = c(0.025, 0.18),
    ylim = c(0.33, 0.65)
  ) +
  theme(
    panel.grid = element_blank()
  ) +
  ggtitle(
    "Proportion of candidates with higher education vs.\n state proportion of higher education"
  ) +
  ggsave(
    here("Presentation/figs/candidate/valence_education.pdf"),
    device = cairo_pdf
  )

# rural vs. edu
candidate_state_demo %>% 
  ggplot(
    aes(
      rural,
      higher_edu
    )
  ) +
  geom_smooth(
    method = "lm",
    col = "darkorange2"
  ) +
  geom_point(
    col = "steelblue3",
    alpha = 0.35
  ) +
  xlab("rural (state)") +
  ylab("education (candidates)") +
  coord_cartesian(
    ylim = c(0.33, 0.66)
  ) +
  theme(
    panel.grid = element_blank()
  ) +
  ggtitle(
    "Proportion of candidates with higher education vs.\n state rural"
  ) +
  ggsave(
    here("Presentation/figs/candidate/valence_rural_edu.pdf"),
    device = cairo_pdf
  )

# incumbents vs literacy rate
candidate_state_demo %>% 
  ggplot(
    aes(
      lit_rate,
      incumbent
    )
  ) +
  geom_smooth(
    method = "lm",
    col = "darkorange2"
  ) +
  geom_point(
    col = "steelblue3",
    alpha = 0.35
  ) +
  xlab("literacy rate (state)") +
  ylab("incumbent (candidates)") +
  coord_cartesian(
    ylim = c(0.02, 0.14)
  ) +
  theme(
    panel.grid = element_blank()
  ) +
  ggtitle(
    "Proportion of incumbent candidates vs.\n state rural"
  ) +
  ggsave(
    here("Presentation/figs/candidate/valence_lit_incumbent.pdf"),
    device = cairo_pdf
  )

# valence by party
candidate_party <- candidate %>% 
  transmute(
    election_year,
    party,
    higher_edu = if_else(edu == "higher education", 1, 0),
    female = if_else(gender == 4, 1, 0),
    incumbent,
    politician = if_else(occupation == "politician", 1, 0),
    government = if_else(occupation == "government", 1, 0),
    business = if_else(occupation == "business", 1, 0),
    white_collar = if_else(occupation == "white-collar", 1, 0)
  ) %>% 
  group_by(
    election_year,
    party
  ) %>%
  summarise_all(
    mean, na.rm = T
  ) %>% 
  ungroup()

candidate_party %>% 
  mutate(
    party = str_to_upper(party)
  ) %>% 
  mutate(
    party = if_else(party %in% c("PT", "PMDB", "PSDB"), party, "Minor Parties"),
    party = factor(party, levels = c("PT", "PSDB", "PMDB", "Minor Parties"))
  ) %>%
  gather(
    term, value, 
    -party, -election_year,
  ) %>%
  ggplot(
    aes(
      reorder(term, desc(term)),
      value,
      col = party
    )
  ) +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.35
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    col = "grey55"
  ) +
  coord_flip() +
  facet_wrap(
    party ~ ., nrow = 2
  ) + 
  scale_color_manual(
    values = c("PT" = "red3", "PMDB" = "orchid4", "Minor Parties" = "chartreuse4", "PSDB" = "steelblue3")
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  ggsave(
    "Presentation/figs/candidate/valence_candidate_party.pdf"
  )

# education vs. ideology
candidate %>% 
  mutate(
    edu = factor(edu, levels = c("higher education", "high school", "middle school", "lower school"))
  ) %>%
  filter(
    !is.na(edu)
  ) %>% 
  ggplot() +
  geom_density(
    aes(
      cfscore,
      col = edu,
    )
  ) +
  facet_wrap(
    edu ~ .,
    ncol = 2
  ) +
  scale_color_manual(
    values = c("higher education" = "red3", "high school" = "orchid4", "middle school" = "chartreuse4", "lower school" = "steelblue3")
  ) +
  coord_cartesian(
    xlim = c(-2, 2)
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  ggsave(
    "Presentation/figs/ideology/candidate_edu_ideology.png"
  )

# occupation vs. ideology
candidate %>% 
  filter(
    !is.na(occupation),
    occupation %in% c("business", "white-collar", "politician", "other")
  ) %>% 
  mutate(
    occupation = factor(occupation, levels = c("politician", "business", "white-collar", "other"))
  ) %>% 
  ggplot() +
  geom_density(
    aes(
      cfscore,
      col = occupation,
    )
  ) +
  facet_wrap(
    occupation ~ .,
    ncol = 2
  ) +
  scale_color_manual(
    values = c("politician" = "red3", "business" = "orchid4", "other" = "chartreuse4", "white-collar" = "steelblue3")
  ) +
  coord_cartesian(
    xlim = c(-2, 2)
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  ggsave(
    "Presentation/figs/ideology/candidate_occupation_ideology.png"
  )

# candidate table
table_full <- candidate %>% 
  count(
    state,
    election_year
  ) %>% 
  spread(
    election_year, n
  )

table_full <- table_full %>% 
  add_row(
    state = "total", 
    `2006`= 4943,
    `2010` = 4869,
    `2014` = 5868
  ) %>% 
  rename_at(
    vars(as.character(seq(2006, 2014, 4))),
    ~ paste(seq(2006, 2014, 4), "full", sep = "_")
  )

table_blp <- candidate_blp %>% 
  count(
    state,
    year
  ) %>% 
  spread(
    year, n
  )

table_blp <- table_blp %>% 
  add_row(
    state = "total", 
    `2006`= 3282,
    `2010` = 3308,
    `2014` = 4162
  ) %>% 
  rename_at(
    vars(as.character(seq(2006, 2014, 4))),
    ~ paste(seq(2006, 2014, 4), "sample", sep = "_")
  )

table_cand <- table_full %>% 
  left_join(
    table_blp,
    by= c("state")
  ) %>%  
  left_join(
    states,
    by = c('state')
  ) %>% 
  mutate(
    `2006_tpercent` = `2006_sample`/`2006_full`*100,
    `2010_tpercent` = `2010_sample`/`2010_full`*100,
    `2014_tpercent` = `2014_sample`/`2014_full`*100
  ) %>% 
  mutate_if(
    is.numeric,
    list(~round(., 2))
  ) %>% 
  select(
    order(colnames(.))
  ) %>% 
  select(
    state_name,
    everything(),
    -state
  )

table_cand %>% 
  kable(
    format = "latex",
    booktabs  = T,
    col.names = c(
      "State",
      rep(
        c("Full", "Sample", "Percent"), 3
      )
    )
  ) %>% 
  add_header_above(
    c(
      " ",
      "2006" = 3,
      "2010" = 3,
      "2014" = 3
    )
  ) %>% 
  kable_styling(
    font_size = 8
  )

# candidate vs party
election_cand <- election %>% 
  group_by(
    election_year,
    state,
    party,
    cpf_candidate,
    candidate_name,
    
  )
