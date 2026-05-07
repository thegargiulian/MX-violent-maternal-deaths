# =========================================
# maternal-homicides/code/999-in-text-statistics.R

# ----- setup

pacman::p_load(here, readr, dplyr, stringr, writexl)

all_deaths_stats <- read_rds(here::here("code/output/total-deaths-statistics.rds"))
deaths <- read_csv(here::here("code/output/MX-female-deaths.csv.gz"))
icdmm <- read_csv(here::here("code/output/deaths-icdmm.csv"))
mmr <- read_csv(here::here("code/output/mmr.csv"))
results <- here::here("code/output/inline-statistics.xlsx")

icdmm <- icdmm %>%
    filter(between(year, 1998, 2024) & between(age, 10, 54))

preg_related <- tibble(N = nrow(icdmm),
                       pct = NA_real_,
                       den = NA_character_,
                       desc = "Total pregnancy-related deaths")

total_violence <- tibble(N = sum(icdmm$self_inflicted) + sum(icdmm$external_violence),
                         pct = NA_real_,
                         den = NA_character_,
                         desc = "Total violent pregnacy-related deaths")

total_external <- tibble(N = sum(icdmm$external_violence),
                         pct = sum(icdmm$external_violence) / (sum(icdmm$self_inflicted) + sum(icdmm$external_violence)) * 100,
                         den = "All violent pregnancy-related deaths",
                         desc = "Total external violence pregnancy-related deaths")

total_undetermined <- tibble(N = sum(icdmm$undetermined_intent),
                             pct = sum(icdmm$undetermined_intent) / sum(icdmm$external_violence) * 100,
                             den = "All external violence pregnancy-related deaths",
                             desc = "Total undetermined intent pregnancy-related deaths")

total_self <- tibble(N = sum(icdmm$self_inflicted),
                     pct = sum(icdmm$self_inflicted) / (sum(icdmm$self_inflicted) + sum(icdmm$external_violence)) * 100,
                     den = "All violent pregnancy-related deaths",
                     desc = "Total self-inflicted violence pregnancy-related deaths")

by_cause <- icdmm %>%
    mutate(icd_mm_final = if_else(any_violence == 1,
                                  "Violence",
                                  icd_mm_final)) %>%
    count(icd_mm_final, name = "N") %>%
    filter(!is.na(icd_mm_final)) %>%
    mutate(pct = N / sum(N) * 100,
           den = "All obstetric deaths (excludes coincidental deaths)",
           desc = "Total deaths by expanded ICD-MM group")

external_firearms <- tibble(N = icdmm %>% filter(lead_base_code %in% c("X93", "X94", "X95", "Y22", "Y23", "Y24")) %>% nrow(),
                            den = "Total external violence pregnancy-related deaths",
                            desc = "Total external firearm deaths") %>%
    mutate(pct = N / total_external$N * 100)

external_hanging <- tibble(N = icdmm %>% filter(lead_base_code %in% c("X91", "Y20")) %>% nrow(),
                           den = "Total external violence pregnancy-related deaths",
                           desc = "Total external hanging deaths") %>%
    mutate(pct = N / total_external$N * 100)


self_hanging <- tibble(N = icdmm %>% filter(lead_base_code == "X70") %>% nrow(),
                       den = "Total self-inflicted violence pregnancy-related deaths",
                       desc = "Total self-inflicted hanging deaths") %>%
    mutate(pct = N / total_self$N * 100)

self_poisoning <- tibble(N = icdmm %>% filter(lead_base_code %in% c("X60", "X61", "X62", "X63", "X64", "X65", "X66", "X67", "X68", "X69")) %>% nrow(),
                         den = "Total self-inflicted violence pregnancy-related deaths",
                         desc = "Total self-inflicted poisoning deaths") %>%
    mutate(pct = N / total_self$N * 100)

wra <- tibble(N = deaths %>% filter(between(age, 10, 54)) %>% nrow(),
              den = NA_character_,
              pct = NA_real_,
              desc = "Total deaths of women 10-54")

miss_preg <- tibble(N = deaths %>% filter(between(age, 10, 54)) %>% filter(is.na(pregnancy_status)) %>% nrow(),
                    den = "WRA deaths",
                    desc = "Total WRA deaths missing pregnancy checkbox") %>%
    mutate(pct = N / wra$N * 100)

Ocodes_missing_preg <- tibble(N = deaths %>%
                                  filter(between(age, 10, 54) & is.na(pregnancy_status)) %>%
                                  mutate(group = str_sub(lead_base_code, 1, 1)) %>%
                                  filter(group == "O") %>% nrow(),
                              den = NA_character_,
                              pct = NA_real_,
                              desc = "Deaths coded with O codes missing pregnancy checkbox")

self_inflicted <- paste0("X", 60:84)
self_inflicted_full_codes <- "Y870"
assault <- c(paste0("X", 85:99), paste0("Y0", 0:9))
assault_full_codes <- "Y871"
undetermined_intent <- paste0("Y", 10:34)
undetermined_intent_full_codes <- "Y872"
legal_intervention_war <- c("Y35", "Y36")
legal_intervention_war_full_codes <- c("Y890", "Y891")

violent_missing <- tibble(N = deaths %>%
                              filter(between(age, 10, 54) & is.na(pregnancy_status)) %>%
                              filter(lead_base_code %in% c(self_inflicted, assault, undetermined_intent, legal_intervention_war) |
                                         lead_cause %in% c(self_inflicted_full_codes, assault_full_codes, undetermined_intent_full_codes, legal_intervention_war_full_codes)) %>%
                              nrow(),
                          den = "WRA deaths missing pregnancy checkbox",
                          desc = "Violent deaths missing pregnancy checkbox") %>%
    mutate(pct = N / miss_preg$N * 100)

avg_age <- tibble(N = icdmm %>%
                      filter(between(age, 10, 54)) %>%
                      pull(age) %>%
                      mean(),
                  pct = NA_real_,
                  den = NA_character_,
                  desc = "Average age WRA")

sd_age <- tibble(N = icdmm %>%
                     filter(between(age, 10, 54)) %>%
                     pull(age) %>%
                     sd(),
                 pct = NA_real_,
                 den = NA_character_,
                 desc = "SD age WRA")

missing_sex <- tibble(N = all_deaths_stats$Nmiss_sex,
                      den = "All deaths 1998-2024",
                      pct = all_deaths_stats$Nmiss_sex / all_deaths_stats$N_deaths * 100,
                      desc = "Total deaths missing sex")

missing_age <- tibble(N = all_deaths_stats$Nmiss_age,
                      den = "All deaths 1998-2024",
                      pct = all_deaths_stats$Nmiss_age / all_deaths_stats$N_deaths * 100,
                      desc = "Total deaths missing age")

assault <- c(paste0("X", 85:99), paste0("Y0", 0:9))
assault_full_codes <- "Y871"
undetermined_intent <- paste0("Y", 10:34)
undetermined_intent_full_codes <- "Y872"
legal_intervention_war <- c("Y35", "Y36")
legal_intervention_war_full_codes <- c("Y890", "Y891")

external_by_type <- tibble(N = c(icdmm %>%
                                     select(lead_base_code, lead_cause) %>%
                                     filter(lead_base_code %in% assault | lead_cause %in% assault_full_codes) %>%
                                     nrow(),
                                 icdmm %>%
                                     select(lead_base_code, lead_cause) %>%
                                     filter(lead_base_code %in% undetermined_intent | lead_cause %in% undetermined_intent_full_codes) %>%
                                     nrow(),
                                 icdmm %>%
                                     select(lead_base_code, lead_cause) %>%
                                     filter(lead_base_code %in% legal_intervention_war | lead_cause %in% legal_intervention_war_full_codes) %>%
                                     nrow()),
                           den = rep("all external deaths", 3),
                           desc = c("external violence - assault",
                                    "external violence - undetermined intent",
                                    "external violence - legal intervention, operations of war")) %>%
    mutate(pct = N / sum(N) * 100)

maternal_deaths <- tibble(N = icdmm %>%
                              filter(!icd_mm_final %in% c(NA_character_,
                                                          "Late maternal and sequelae",
                                                          "Sequelae",
                                                          "Contributory")) %>%
                              nrow(),
                          den = NA_character_,
                          pct = NA_real_,
                          desc = "Maternal deaths - excludes violence, late maternal, sequelae, and contributory")

late_maternal_deaths <- tibble(N = icdmm %>%
                                   filter(icd_mm_final %in% c("Late maternal and sequelae")) %>%
                                   nrow(),
                               den = NA_character_,
                               pct = NA_real_,
                               desc = "Late maternal deaths according to ICD-MM")

mmr_2024 <- tibble(N = mmr %>%
                       filter(year == 2024) %>%
                       pull(MMR),
                   den = NA_character_,
                   pct = NA_real_,
                   desc = "2024 MMR")

mmr_2024_suicide <- tibble(N = mmr %>%
                               filter(year == 2024) %>%
                               pull(MMR_suicide),
                           den = NA_character_,
                           pct = NA_real_,
                           desc = "2024 MMR + suicides")

mmr_2024_homicide <- tibble(N = mmr %>%
                                filter(year == 2024) %>%
                                pull(MMR_homicide),
                            den = NA_character_,
                            pct = NA_real_,
                            desc = "2024 MMR + homicides")

mmr_2024_all_violence <- tibble(N = mmr %>%
                                    filter(year == 2024) %>%
                                    pull(MMR_violence_all),
                                den = NA_character_,
                                pct = NA_real_,
                                desc = "2024 MMR + all violent deaths")

mmr_changes <- tibble(N = c(mmr_2024_suicide$N - mmr_2024$N,
                            mmr_2024_homicide$N - mmr_2024$N,
                            mmr_2024_all_violence$N - mmr_2024$N),
                      den = rep("relative to 2024 MMR", 3),
                      pct = c((mmr_2024_suicide$N - mmr_2024$N) / mmr_2024$N * 100,
                              (mmr_2024_homicide$N - mmr_2024$N) / mmr_2024$N * 100,
                              (mmr_2024_all_violence$N - mmr_2024$N) / mmr_2024$N * 100),
                      desc = c("diff relative to 2024 MMR - suicide",
                               "diff relative to 2024 MMR - homicide",
                               "diff relative to 2024 MMR - all violence"))

all_statistics <- bind_rows(avg_age,
                            by_cause,
                            external_by_type,
                            external_firearms,
                            external_hanging,
                            late_maternal_deaths,
                            maternal_deaths,
                            miss_preg,
                            missing_age,
                            missing_sex,
                            mmr_2024,
                            mmr_2024_all_violence,
                            mmr_2024_homicide,
                            mmr_2024_suicide,
                            mmr_changes,
                            Ocodes_missing_preg,
                            preg_related,
                            sd_age,
                            self_hanging,
                            self_poisoning,
                            total_external,
                            total_self,
                            total_undetermined,
                            total_violence,
                            violent_missing,
                            wra)

write_xlsx(all_statistics, results)

# done.
