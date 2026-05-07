# =========================================
# maternal-homicides/code/104-mmr.R

# ----- setup

pacman::p_load(here, readr, dplyr)

args <- list(deaths = here::here("code/output/deaths-icdmm.csv"),
             deaths_allwomen = here::here("code/output/deaths-icdmm-allwomen.csv"),
             births = here::here("code/output/MX-births.csv.gz"),
             states = here::here("data/state-codes.csv"),
             mmr = here::here("code/output/mmr.csv"),
             mmr_by_state = here::here("code/output/mmr-by-state.csv"))

# ----- main

deaths <- read_csv(args$deaths) # only pregnant/postpartum women
deaths_allwomen <- read_csv(args$deaths_allwomen)
births <- read_csv(args$births, col_names = TRUE)
states <- read_csv(args$states, col_names = TRUE)

deaths_sum <- deaths %>%
    filter(between(year, 1998, 2024)) %>%
    group_by(year) %>%
    summarise(preg_related = n(),
              maternal = sum(maternal == 1, na.rm = TRUE),
              self_inflicted_n = sum(self_inflicted == 1, na.rm = TRUE),
              external_n = sum(external_violence == 1, na.rm = TRUE),
              .groups = "drop")

births_sum <- births %>%
    group_by(year) %>%
    summarise(births = sum(births, na.rm = TRUE), .groups = "drop")

mmr <- left_join(deaths_sum, births_sum, by = "year")

mmr <- mmr %>%
    mutate(prmr = (preg_related / births) * 100000,
           MMR = (maternal / births) * 100000,
           MMR_suicide = ((maternal + self_inflicted_n) / births) * 100000,
           MMR_homicide = ((maternal + external_n) / births) * 100000,
           MMR_violence_all = ((maternal + self_inflicted_n + external_n) / births) * 100000) %>%
    mutate(across(where(is.numeric), ~ round (.x, 1)))

write_csv(mmr, args$mmr)

# restricting to 2008 onwards when violence is a bigger issue
deaths_state <- deaths %>%
    filter(year >= 2008) %>%
    group_by(cod_ent_resid) %>% # using state of residence to match births data
    summarise(preg_related = n(),
        maternal = sum(maternal == 1, na.rm = TRUE),
        self_inflicted_n = sum(self_inflicted == 1, na.rm = TRUE),
        external_n = sum(external_violence == 1, na.rm = TRUE),
        .groups = "drop")  %>%
    mutate(cod_ent_resid = as.numeric(cod_ent_resid))

births_state <- births %>%
    filter(year >= 2008)%>%
    group_by(cod_ent_resid) %>%
    summarise(births = sum(births, na.rm = TRUE), .groups = "drop")

mmr_state <- left_join(deaths_state, births_state)
mmr_state <- left_join(mmr_state, states, by = c("cod_ent_resid" = "id"))
mmr_state <- mmr_state %>%
    filter(!is.na(name)) # drop missing - outside of MX, and obs with missing cod_ent_resid

mmr_state <- mmr_state %>%
    mutate(prmr =  (preg_related / births) * 100000,
           MMR = (maternal / births) * 100000,
           MMR_suicide = ((maternal + self_inflicted_n) / births) * 100000,
           MMR_homicide = ((maternal + external_n) / births) * 100000,
           MMR_violence_all = ((maternal + self_inflicted_n + external_n) / births) * 100000) %>%
    mutate(across(where(is.numeric), ~ round (.x, 1)))

write_csv(mmr_state, args$mmr_by_state)

# done.
