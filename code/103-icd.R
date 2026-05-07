# =========================================
# maternal-homicides/code/103-icd.R

# ----- setup

pacman::p_load(readr, tidyverse, forcats)

args <- list(deaths = here::here("code/output/MX-female-deaths.csv.gz"),
             icdmm_output = here::here("code/output/deaths-icdmm.csv"),
             icdmm_all_output = here::here("code/output/deaths-icdmm-allwomen.csv"))

deaths <- read_csv(args$deaths)

# limit data to 1998 on (implementation of ICD-10)
deaths <- deaths %>%
    filter(year >= 1998) %>%
    mutate(id = row_number())

# create ICD-MM groups
preg_abortive <- paste0("O0", 0:7)
htn_disorders <- paste0("O", 11:16)
ob_haemorrhage <- paste0("O", c(20, 45, 46, 67, 72))
ob_haemorrhage_full_codes <- c("O432", "O441", "O710", "O711", "O713", "O714",
                               "O717")
preg_infection <- c(paste0("O", c(23, 85, 86, 91)), "A34")
preg_infection_full_codes <- c("O411", "O753")
other_obstetric <- c(paste0("O", c(73, 88, 90, 64:66)), "F53")
other_obstetric_full_codes <- c("O211", "O212", "O223", "O225", "O228", "O229",
                                "O244", "O266", "O269", "O440", "O712", "O715",
                                "O716", "O718", "O719", "O754", "O758", "O759",
                                "O871", "O873", "O879")
complications_mgmt_full_codes <- c("O290", "O291", "O292", "O293", "O295", "O296",
                                   "O298", "O299", "O740", "O741", "O742", "O743",
                                   "O744", "O746", "O747", "O748", "O749", "O890",
                                   "O891", "O892", "O893", "O895", "O896", "O898",
                                   "O899")
non_obstetric <- c("O10", "O98", "O99", "C58")
non_obstetric_full_codes <- c("O240", "O241", "O242", "O243", "O249", "D392")
unspecified <- "O95"
contrib_conditions <- c("O08", paste0("O", c(25, 26, 28, 30:36, 40, 42, 47, 48, 60:63,
                                             68:70, 80:84, 92, 94)))
contrib_conditions_full_codes <- c("O210", "O218", "O219", "O220", "O221", "O222",
                                   "O224", "O294", "O410", "O418", "O419", "O430",
                                   "O431", "O438", "O439", "O745", "O750", "O751",
                                   "O752", "O755", "O756", "O757", "O870", "O872",
                                   "O878", "O894")

deaths_icdmm_all <- deaths %>%
    mutate(icd_mm_final = case_when(lead_base_code %in% preg_abortive ~ "PREG_ABORTIVE",
                                    lead_base_code %in% htn_disorders ~ "HTN",
                                    lead_base_code %in% ob_haemorrhage | lead_cause %in% ob_haemorrhage_full_codes ~ "HAEM",
                                    lead_base_code %in% preg_infection | lead_cause %in% preg_infection_full_codes ~ "INFECTION",
                                    lead_base_code %in% other_obstetric | lead_cause %in% other_obstetric_full_codes ~ "OTHER_OBSTETRIC",
                                    lead_cause %in% complications_mgmt_full_codes ~ "COMPLICATIONS_MGMT",
                                    lead_base_code %in% non_obstetric | lead_cause %in% non_obstetric_full_codes ~ "NON_OBSTETRIC",
                                    lead_base_code %in% contrib_conditions | lead_cause %in% contrib_conditions_full_codes ~ "CONTRIB_CONDITIONS",
                                    lead_base_code %in% unspecified ~ "UNSPECIFIED",
                                    lead_base_code %in% c("O96", "O97") ~ "LATE + SEQ",
                                    TRUE ~ NA_character_ ))

# relabel for plotting
deaths_icdmm_all <- deaths_icdmm_all %>%
    mutate(icd_mm_final = fct_recode(icd_mm_final,
                                     "G1: Abortive outcome"            = "PREG_ABORTIVE",
                                     "G2: Hypertensive disorders"      = "HTN",
                                     "G3: Obstetric haemorrhage"       = "HAEM",
                                     "G4: Pregnancy-related infection" = "INFECTION",
                                     "G5: Other obstetric"             = "OTHER_OBSTETRIC",
                                     "G6: Complications of management" = "COMPLICATIONS_MGMT",
                                     "G7: Non-obstetric"               = "NON_OBSTETRIC",
                                     "G8: Undetermined"                = "UNSPECIFIED",
                                     "Late maternal and sequelae"      = "LATE + SEQ",
                                     "Contributory"                    = "CONTRIB_CONDITIONS"),
           icd_mm_final = fct_relevel(icd_mm_final,
                                      "G1: Abortive outcome",
                                      "G2: Hypertensive disorders",
                                      "G3: Obstetric haemorrhage",
                                      "G4: Pregnancy-related infection",
                                      "G5: Other obstetric",
                                      "G6: Complications of management",
                                      "G7: Non-obstetric",
                                      "G8: Undetermined",
                                      "Late maternal and sequelae",
                                      "Contributory"))

# create indicator variables for maternal deaths
# choosing to leave contributory causes in
deaths_icdmm_all <- deaths_icdmm_all %>%
    mutate(maternal = if_else(is.na(icd_mm_final) | icd_mm_final %in% c("Late maternal and sequelae"),
                              0L,
                              1L),
           late_maternal = if_else(icd_mm_final %in% c("Late maternal and sequelae"),
                                   1L,
                                   0L))

# check non-obstetric in 2020/2021
check <- deaths_icdmm_all %>%
    filter(icd_mm_final == "G7: Non-obstetric" & year %in% c(2008, 2020, 2021))

# now code violent causes of death
self_inflicted <- paste0("X", 60:84)
self_inflicted_full_codes <- "Y870"
assault <- c(paste0("X", 85:99), paste0("Y0", 0:9))
assault_full_codes <- "Y871"
undetermined_intent <- paste0("Y", 10:34)
undetermined_intent_full_codes <- "Y872"
legal_intervention_war <- c("Y35", "Y36")
legal_intervention_war_full_codes <- c("Y890", "Y891")

deaths_icdmm_all <- deaths_icdmm_all %>%
    mutate(self_inflicted     = as.integer(lead_base_code %in% self_inflicted | lead_cause %in% self_inflicted_full_codes),
           external_violence  = as.integer(lead_base_code %in% c(assault, undetermined_intent, legal_intervention_war) |
                                               lead_cause %in% c(assault_full_codes, undetermined_intent_full_codes, legal_intervention_war_full_codes)),
           undetermined_intent = as.integer(lead_base_code %in% undetermined_intent | lead_cause %in% undetermined_intent_full_codes), # separate for tracking
           any_violence       = as.integer(self_inflicted == 1 | external_violence == 1))

# limit to just pregnant or postpartum women
deaths_icdmm <- deaths_icdmm_all %>%
    filter(pregnancy_status %in% c("Pregnant or <43 days postpartum", "43 days-11 months postpartum"))

# write to file
write_csv(deaths_icdmm, args$icdmm_output)
write_csv(deaths_icdmm_all, args$icdmm_all_output)

# checking coding practices for self harm.
# deaths from suicide are coded as unrelated to pregnancy. should have an obstetric code (ICD-MM)
# ICD-MM other direct causes. Especially given we know they are temporal to pregnancy
check <- deaths_icdmm %>%
    filter(self_inflicted == 1)

# done.
