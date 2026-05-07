# =========================================
# maternal-homicides/code/000-main.R

# ----- setup

# pacman for package management throughout
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(here)

# create output folder
if (!dir.exists(here::here("code/output"))) {dir.create(here::here("code/output"))}

# run scripts - the data is large, so clear the environment between each
# script
source(here::here("code/101-deaths.R"))
rm(list = ls())

source(here::here("code/102-births.R"))
rm(list = ls())

source(here::here("code/103-icd.R"))
rm(list = ls())

source(here::here("code/104-mmr.R"))
rm(list = ls())

source(here::here("code/999-visualization.R"))
rm(list = ls())

source(here::here("code/999-in-text-statistics.R"))
rm(list = ls())

# done.
