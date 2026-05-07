# =========================================
# maternal-homicides/code/101-deaths.R

# ----- setup

pacman::p_load(here, readr, dplyr, purrr, glue, janitor, stringr, foreign, ggplot2)

args <- list(import_stub = here::here("data"),
             age_groups = here::here("hand/age-groups.csv"),
             output = here::here("code/output/MX-female-deaths.csv.gz"),
             magic_numbers = here::here("code/output/total-deaths-statistics.rds"))

# ----- functions


# input is the file path of one year of death certificate data
read_file <- function(input_file) {

    def_data <- read.dbf(input_file, as.is = TRUE) %>%
        janitor::clean_names() %>%
        mutate(cod_ent_ocurr = str_pad(ent_ocurr, 2, "left", "0"),
               cod_ent_resid = str_pad(ent_resid, 2, "left", "0"),
               year = anio_ocur,
               month = mes_ocurr,
               year_reg = anio_regis,
               sex = sexo,
               age = edad,
               lead_cause = causa_def,
               pregnancy_status = embarazo,
               maternal_causes = maternas,
               pregnancy_related_conditions = rel_emba,
               autopsy = as.numeric(necropsia),
               who_certified = cond_cert)

    if ("complicaro" %in% names(def_data)) {

        def_data <- def_data %>%
            mutate(pregnancy_complications = complicaro)

    } else {

        def_data <- def_data %>%
            mutate(pregnancy_complications = NA_real_)

    }

    if ("razon_m" %in% names(def_data)) {

        def_data <- def_data %>%
            mutate(included_mm_calc = razon_m)

    } else {

        def_data <- def_data %>%
            mutate(included_mm_calc = NA_real_)

    }


    def_data <- def_data %>%
        select(cod_ent_ocurr, cod_ent_resid,
               year, month,
               year_reg,
               sex, age,
               lead_cause,
               pregnancy_status, maternal_causes,
               pregnancy_related_conditions, pregnancy_complications,
               included_mm_calc,
               autopsy, who_certified)

    return(def_data)

}


# ----- main

# collect all death certificate file paths
years <- str_pad(c(98, 99, 0:24), 2, "left", 0)
# assumes all file names standardized to all upper (MG did this manually)
input_files <- glue("{args$import_stub}/DEFUN{years}.dbf")

# read in and concatenate records from all files
deaths_data <- map_dfr(input_files, read_file)

N_deaths <- nrow(deaths_data)
Nmiss_sex <- sum(deaths_data$sex == 9)
Nmiss_age <- sum(is.na(deaths_data$age) | deaths_data$age == 4998)

magic_numbers <- list(N_deaths = N_deaths,
                      Nmiss_sex = Nmiss_sex,
                      Nmiss_age = Nmiss_age)

write_rds(magic_numbers, args$magic_numbers)

age_groups <- read_csv(args$age_groups)

female_deaths <- deaths_data %>%
    filter(sex == 2 & !cod_ent_ocurr %in% c(33, 34, 35)) %>% # filter deaths outside MX (33-35)
    mutate(cod_ent_ocurr = na_if(cod_ent_ocurr, "99"),
           cod_ent_resid = na_if(cod_ent_resid, "99"),
           age = case_when(is.na(age) | age == 4998 ~ NA_real_,
                           age < 4000 ~ 0,
                           age >= 4000 ~ age - 4000),
           floor = if_else(age >= 85, 85, age %/% 5 * 5),
           lead_base_code = str_sub(lead_cause, 1, 3),
           autopsy = case_when(autopsy == 1 ~ "Yes",
                               autopsy == 2 ~ "No",
                               autopsy == 8 ~ "Not applicable for natural death",
                               autopsy == 9 ~ NA_character_)) %>%
    # different coding practices used at different times for pregnancy check box
    mutate(pregnancy_label = case_when(between(year_reg, 1998, 2003) & pregnancy_status == 1 ~ "Pregnant or <43 days postpartum",
                                       between(year_reg, 1998, 2003) & pregnancy_status == 2 ~ "43 days-11 months postpartum",
                                       between(year_reg, 1998, 2003) & pregnancy_status == 3 ~ "1+ years postpartum",
                                       between(year_reg, 1998, 2003) & pregnancy_status == 9 ~ "Not pregnant",
                                       year_reg > 2003 & pregnancy_status == 1 ~ "Pregnant",
                                       year_reg > 2003 & pregnancy_status == 2 ~ "Birth",
                                       year_reg > 2003 & pregnancy_status == 3 ~ "<43 days postpartum",
                                       year_reg > 2003 & pregnancy_status == 4 ~ "43 days-11 months postpartum",
                                       year_reg > 2003 & pregnancy_status == 5 ~ "Not pregnant in the last 11 months",
                                       year_reg > 2003 & pregnancy_status == 6 ~ "1+ years postpartum",
                                       year_reg > 2003 & pregnancy_status == 8 ~ "Does not apply",
                                       year_reg > 2003 & pregnancy_status == 9 ~ NA_character_),
           pregnancy_status = case_when(pregnancy_label %in% c("Pregnant or <43 days postpartum", "Pregnant", "Birth", "<43 days postpartum") ~ "Pregnant or <43 days postpartum",
                                        pregnancy_label == "43 days-11 months postpartum" ~ "43 days-11 months postpartum",
                                        pregnancy_label == "1+ years postpartum" ~ "1+ years postpartum",
                                        pregnancy_label %in% c("Not pregnant", "Not pregnant in the last 11 months") ~ "Not pregnant",
                                        pregnancy_label == "Does not apply" ~ "Does not apply",
                                        is.na(pregnancy_label) ~ NA_character_),
           pregnancy_related_conditions = case_when(pregnancy_related_conditions == 1 ~ "Related",
                                                    pregnancy_related_conditions == 2 ~ "Unrelated",
                                                    pregnancy_related_conditions == 8 ~ "Not applicable",
                                                    pregnancy_related_conditions == 9 ~ NA_character_),
           pregnancy_complications = case_when(pregnancy_complications == 1 ~ "Yes",
                                               pregnancy_complications == 2 ~ "No",
                                               pregnancy_complications == 8 ~ "Not applicable",
                                               pregnancy_complications == 9 ~ NA_character_,
                                               TRUE ~ NA_character_)) %>%
    left_join(age_groups, by = "floor")

female_deaths %>%
    glimpse() %>%
    write_csv(args$output)

# free up space if replicating everything directly in interactive session
rm(deaths_data); gc()

# done.
