# =========================================
# maternal-homicides/code/102-births.R

# ----- setup

pacman::p_load(here, readr, dplyr, purrr, glue, janitor, stringr, foreign)

args <- list(import_stub = here::here("data"),
             output = here::here("code/output/MX-births.csv.gz"))

# ----- functions


# input is the file path of one year of death certificate data
read_file <- function(input_file) {

    nac_data <- read.dbf(input_file, as.is = TRUE) %>%
        janitor::clean_names()

}


clean_births <- function(input_file) {

    births <- read_file(input_file)

    births <- births %>%
        # use year born, not year registered
        select(year = ano_nac,
               cod_ent_resid = ent_resid) %>%
        mutate(cod_ent_resid = as.numeric(cod_ent_resid)) %>%
        # filter births to moms residing outside of MX; keep only births for
        # study years
        filter(!cod_ent_resid %in% 33:35 & between(year, 1998, 2024)) %>%
        count(cod_ent_resid, year)

    return(births)

}

# ----- main

# collect all death certificate file paths
years <- str_pad(c(98, 99, 0:24), 2, "left", 0)
# assumes all file names standardized to all upper (MG did this manually)
input_files <- glue("{args$import_stub}/NACIM{years}.dbf")

# read in and clean and concatenate birth totals records from all files
births_data <- map_dfr(input_files, clean_births)

births_data %>%
    group_by(cod_ent_resid, year) %>%
    summarize(births = sum(n), .groups = "drop") %>%
    glimpse() %>%
    write_csv(args$output)

# done.
