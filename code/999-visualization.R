# =========================================
# maternal-homicides/code/999-visualization.R

# ----- setup

pacman::p_load(here, readr, dplyr, forcats, stringr, tidyr, assertr, stringi,
               ggplot2, scales, patchwork, RColorBrewer, sf, rnaturalearth,
               rnaturalearthdata, viridis, tidytext, ggrepel, gt)

if (!dir.exists(here::here("code/output"))) {dir.create(here::here("code/output"))}

args <- list(female_deaths = here::here("code/output/MX-female-deaths.csv.gz"),
             deaths_icdmm = here::here("code/output/deaths-icdmm.csv"),
             deaths_icdmm_all = here::here("code/output/deaths-icdmm-allwomen.csv"),
             mmr = here::here("code/output/mmr.csv"),
             mmr_state = here::here("code/output/mmr-by-state.csv"),
             figS1 = here::here("code/output/figS1-pct-pregnancy-status-missing.jpg"),
             figS2 = here::here("code/output/figS2-age-distribution.jpg"),
             figS3 = here::here("code/output/figS3-violence-disag.jpg"),
             tabS2 = here::here("code/output/tabS2-violence-disag.html"),
             fig1 = here::here("code/output/fig1-violence-preg-wra.jpg"),
             tabS3 = here::here("code/output/tabS3-violence-preg-wra.html"),
             fig2 = here::here("code/output/fig2-deaths-icd-group.jpg"),
             tabS4 = here::here("code/output/tabS4-deaths-icd-group.html"),
             figS4 = here::here("code/output/figS4-contrib.jpg"),
             fig3 = here::here("code/output/fig3-modalities.jpg"),
             figS5 = here::here("code/output/figS5-modalities-external-by-status.jpg"),
             figS6 = here::here("code/output/figS6-modalities-self-inflicted-by-status.jpg"),
             figS7 = here::here("code/output/figS7-modalities-time.jpg"),
             figS8 = here::here("code/output/figS8-modalities-preg-status.jpg"),
             fig4 = here::here("code/output/fig4-mmr.jpg"),
             fig5 = here::here("code/output/fig5-mmr-changes-state.jpg"),
             figS9 = here::here("code/output/figS9-correlation-state-time.jpg"),
             figS10 = here::here("code/output/figS10-correlation-preg-status.jpg"))

# ----- main


female_deaths <- read_csv(args$female_deaths) %>%
    mutate(pregnancy_status = if_else(pregnancy_status %in% "43 days-11 months postpartum",
                                      "43 days-1 year postpartum",
                                      pregnancy_status))
deaths_icdmm <- read_csv(args$deaths_icdmm) %>%
    mutate(pregnancy_status = if_else(pregnancy_status %in% "43 days-11 months postpartum",
                                      "43 days-1 year postpartum",
                                      pregnancy_status),
           pregnancy_label = if_else(pregnancy_label %in% "43 days-11 months postpartum",
                                     "43 days-1 year postpartum",
                                     pregnancy_label))
deaths_icdmm_all <- read_csv(args$deaths_icdmm_all) %>%
    mutate(pregnancy_status = if_else(pregnancy_status %in% "43 days-11 months postpartum",
                                      "43 days-1 year postpartum",
                                      pregnancy_status),
           pregnancy_label = if_else(pregnancy_label %in% "43 days-11 months postpartum",
                                     "43 days-1 year postpartum",
                                     pregnancy_label))
mmr <- read_csv(args$mmr)
mmr_state <- read_csv(args$mmr_state)

# figures, in order of reference in the manuscript

# figure S1
figS1 <- female_deaths %>%
    filter(between(year, 1998, 2024),
           between(age, 10, 54)) %>%
    mutate(missing_status = is.na(pregnancy_status)) %>%
    count(year, missing_status) %>%
    group_by(year) %>%
    mutate(total = sum(n)) %>%
    filter(missing_status) %>%
    mutate(pct = n / total * 100) %>%
    ggplot(aes(x = year, y = pct)) +
    geom_line(color = "#1f78b4", linewidth = 1.1) +
    geom_point(color = "#1f78b4", size = 2) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    labs(x = "Year", y = "% of records missing pregnancy checkbox information") +
    theme_minimal(base_size = 15) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.key.size = unit(0.55, "cm"),
          legend.box = "vertical",
          legend.box.just = "left",
          legend.margin = margin(t = 2, b = 2),
          axis.text.y = element_text(size = 16, color = "black"),
          axis.text.x = element_text(size = 16, angle = 45, hjust = 1, color = "black")) +
    guides(fill  = guide_legend(nrow = 2, byrow = TRUE))

ggsave(args$figS1, plot = figS1, width = 10, height = 8, dpi = 320, bg = "white")

# figure S2
figS2 <- female_deaths %>%
    filter(pregnancy_status %in% c("Pregnant or <43 days postpartum", "43 days-1 year postpartum") &
               between(age, 10, 54) & between(year, 1998, 2024)) %>% # remove instances where age appears to be incorrectly recorded or is missing
    mutate(age_group = str_replace(age_group, "-", "\u2013")) %>%
    count(age_group) %>%
    ggplot(aes(x = age_group, y = n)) +
    geom_col(fill = "#1f78b4") +
    scale_y_continuous(labels = comma) +
    labs(x = "Age group", y = "Number of pregnancy-related deaths") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14, color = "black"),
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1, color = "black"))


ggsave(args$figS2, plot = figS2, width = 10, height = 8, dpi = 320, bg = "white")

# figure S3
deaths_summary <- deaths_icdmm %>%
    filter(!is.na(year) & between(year, 1998, 2024) & between(age, 10, 54)) %>%
    mutate(year = as.integer(year),
           group = case_when(
               icd_mm_final == "Contributory" ~ "Contributory",
               self_inflicted == 1       ~ "Self-inflicted violence",
               external_violence == 1    ~ "External violence",
               maternal == 1 ~ "Maternal",
               late_maternal == 1 ~"Late maternal",
               TRUE ~ NA_character_)) %>%
    filter(!is.na(group)) %>%
    group_by(year, group) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(group = factor(group,
                          levels = c("Maternal",
                                     "Late maternal",
                                     "Self-inflicted violence",
                                     "External violence",
                                     "Contributory")))

all_years <- sort(unique(deaths_summary$year))
breaks_5   <- all_years[all_years %% 5 == 0]

fill_cols <- c("Maternal"                        = "#1f78b4",
               "Late maternal"                   = "#a6cee3",
               "Self-inflicted violence"         = "#e31a1c",
               "External violence"               = "darkorchid4",
               "Contributory"                    = "#D9D9D9")

figS3_violence_disag <- ggplot(deaths_summary,
                               aes(x = factor(year), y = n, fill = group)) +
    geom_col(position = "stack", color = "black", linewidth = 0.1) +
    scale_x_discrete(breaks = as.character(breaks_5), labels = breaks_5) +
    scale_y_continuous(labels = comma) +
    scale_fill_manual(values = fill_cols, name = "Cause group") +
    labs(x = "Year",
         y = "Number of deaths") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14, color = "black"),
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1, color = "black"))

ggsave(args$figS3, plot = figS3_violence_disag,
       width = 12, height = 8, dpi = 320, bg = "white")

# table S2
deaths_summary <- deaths_summary %>%
    mutate(year = as.character(year))

cause_wide <- deaths_summary %>%
    pivot_wider(names_from = group,
                values_from = n,
                values_fill = 0)

# just inspecting results
cause_wide %>%
    summarize(`External violence` = sum(`External violence`),
              `Late maternal` = sum(`Late maternal`),
              Maternal = sum(Maternal),
              `Self-inflicted violence` = sum(`Self-inflicted violence`),
              Contributory = sum(Contributory))

grand_total <- deaths_summary %>%
    group_by(group) %>%
    summarize(n = sum(n)) %>%
    pivot_wider(names_from = group,
                values_from = n) %>%
    mutate(year = "Total",
           year_total = `External violence` + `Late maternal` + Maternal + `Self-inflicted violence` + Contributory)

cause_wide <- cause_wide %>%
    mutate(year_total = `External violence` + `Late maternal` + Maternal + `Self-inflicted violence` + Contributory) %>%
    bind_rows(grand_total)

cause_wide_pct <- cause_wide %>%
    mutate(across(-c(year, year_total),
                  ~ifelse(year != "Total",
                          str_squish(paste0(format(.x, big.mark = ","), " (", percent(.x[year != "Total"] / sum(.x[year != "Total"]), accuracy = 0.1), ")")),
                          str_squish(format(.x, big.mark = ",")))))

tabS2 <- cause_wide_pct %>%
    mutate(year_total = format(year_total, big.mark = ",")) %>%
    rename(`Year total` = year_total) %>%
    select(year, Maternal, `Late maternal`, `Self-inflicted violence`, `External violence`, Contributory, `Year total`) %>%
    gt(rowname_col = "year") %>%
    tab_header(title = "Deaths by cause group and year") %>%
    tab_options(table.font.size = px(14)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(rows = year == "Total")) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = c(`Year total`)))

gtsave(data = tabS2, filename = args$tabS2)

# figure 1
deaths_by_age <- deaths_icdmm_all %>%
    filter(!is.na(age_group) & between(age, 10, 54) & between(year, 1998, 2024)) %>%
    select(age_group, pregnancy_status, self_inflicted, external_violence, undetermined_intent) %>%
    mutate(label = case_when(pregnancy_status %in% c("43 days-1 year postpartum", "Pregnant or <43 days postpartum") ~ "Pregnant or <1 year postpartum",
                             pregnancy_status %in% c("Does not apply", "Not pregnant", "1+ years postpartum") ~ "Not pregnant or <1 year postpartum",
                             is.na(pregnancy_status) ~ NA)) %>%
    filter(!is.na(label)) %>%
    select(-pregnancy_status) %>%
    mutate(type = case_when(self_inflicted == 1 ~ "Self-inflicted",
                            external_violence == 1 | undetermined_intent == 1 ~ "Externally inflicted",
                            TRUE ~ "Other")) %>%
    count(age_group, label, type)

death_age_proportion <- bind_rows(deaths_by_age %>%
                                      group_by(age_group, type) %>%
                                      summarize(n = sum(n)) %>%
                                      group_by(age_group) %>%
                                      mutate(prop = n / sum(n) * 100) %>%
                                      ungroup() %>%
                                      filter(type != "Other") %>%
                                      mutate(label = "Not pregnant or <1 year postpartum"),
                                  deaths_by_age %>%
                                      filter(label == "Pregnant or <1 year postpartum") %>%
                                      group_by(age_group) %>%
                                      mutate(prop = n / sum(n) * 100) %>%
                                      ungroup() %>%
                                      filter(type != "Other")) %>%
    mutate(label = factor(label, levels = c("Not pregnant or <1 year postpartum",
                                            "Pregnant or <1 year postpartum")),
           age_group = str_replace(age_group, "-", "\u2013"))

fill_values <- c("Pregnant or <1 year postpartum" = "#1f78b4",
                 "Not pregnant or <1 year postpartum" = "#a6cee3")

fig1a_external <- death_age_proportion %>%
    filter(type == "Externally inflicted") %>%
    ggplot(aes(x = age_group, y = prop, fill = label)) +
    geom_col(position = "dodge", color = "black", linewidth = 0.1) +
    labs(
        x = "Age group",
        y = "% of deaths due to violence",
        tag = "Panel A: External violence",
        fill = NULL
    ) +
    scale_fill_manual(values = fill_values, name = "") +
    theme_minimal(base_size = 16) +
    theme(
        axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 14, color = "black"),
        plot.tag.position = "top",
        plot.tag = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

guides(fill = guide_legend(nrow = 2, byrow = TRUE))

fig1b_self <- death_age_proportion %>%
    filter(type == "Self-inflicted") %>%
    ggplot(aes(x = age_group, y = prop, fill = label)) +
    geom_col(position = "dodge", color = "black", linewidth = 0.1) +
    labs(
        x = "Age group",
        y = "% of deaths due to violence",
        tag = "Panel B: Self-inflicted violence",
        fill = NULL
    ) +
    scale_fill_manual(values = fill_values, name = "") +
    theme_minimal(base_size = 16) +
    theme(
        axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 14, color = "black", angle = 45, hjust = 1),
        plot.tag.position = "top",
        plot.tag = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.key.size = unit(0.55, "cm"))

fig1_combined <- fig1a_external / fig1b_self

ggsave(args$fig1, plot = fig1_combined,
       width = 10, height = 8, dpi = 320, bg = "white")

# Table S3
tabS3 <- death_age_proportion %>%
    mutate(numeric_label = str_squish(paste0(format(n, big.mark = ","), " (", round(prop, 1), "%)")),
           group_label = paste0(type, "_", label)) %>%
    select(age_group, numeric_label, group_label) %>%
    pivot_wider(names_from = "group_label",
                values_from = "numeric_label") %>%
    select(age_group, starts_with("Externally"), starts_with("Self")) %>%
    gt(rowname_col = "age_group") %>%
    tab_spanner_delim(delim = "_")

gtsave(tabS3, filename = args$tabS3)

# fig 2
# original version
# icd_levels <- c("G1: Abortive outcome",
#                 "G2: Hypertensive disorders",
#                 "G3: Obstetric haemorrhage",
#                 "G4: Pregnancy-related infection",
#                 "G5: Other obstetric",
#                 "G6: Complications of management",
#                 "G7: Non-obstetric",
#                 "G8: Undetermined",
#                 "Contributory",
#                 "Late maternal and sequelae")
#
# violence_data <- deaths_summary %>% filter(group == "Violence")
#
# maternal_data <- deaths_summary %>%
#     filter(group != "Violence") %>%
#     mutate(group = fct_relevel(group, icd_levels))
#
# fill_cols <- setNames(brewer.pal(10, "Set3"), icd_levels)
#
# fig2 <- ggplot() +
#     geom_col(data = maternal_data,
#              aes(x = year, y = n, fill = group),
#              color = "black", size = 0.1) +
#     geom_line(data = violence_data,
#               aes(x = year, y = n, color = "Violent deaths"),
#               linewidth = 1.1) +
#     geom_point(data = violence_data,
#                aes(x = year, y = n, color = "Violent deaths"),
#                size = 2) +
#     scale_x_continuous(breaks = labelled_years) +
#     scale_y_continuous(label = comma) +
#     scale_fill_manual(values = fill_cols,
#                       drop   = TRUE,
#                       name   = "ICD-MM Group") +
#     scale_color_manual(values = c("Violent deaths" = "black"),
#                        name   = NULL) +
#     labs(x = "Year",
#          y = "Number of deaths") +
#     theme_minimal(base_size = 15) +
#     theme(legend.position = "bottom",
#           legend.text = element_text(size = 13),
#           legend.title = element_text(size = 13),
#           legend.key.size = unit(0.55, "cm"),
#           legend.box = "vertical",
#           legend.box.just = "left",
#           legend.margin = margin(t = 2, b = 2),
#           axis.text.y = element_text(size = 13, color = "black"),
#           axis.text.x = element_text(size = 13, angle = 45, hjust = 1, color = "black")) +
#     guides(fill  = guide_legend(nrow = 2, byrow = TRUE),
#            color = guide_legend(override.aes = list(linetype = 1, shape = 16)))

# new version
icd_levels <- c("G1: Abortive outcome",
                "G2: Hypertensive disorders",
                "G3: Obstetric haemorrhage",
                "G4: Pregnancy-related infection",
                "G5: Other obstetric",
                "G6: Complications of management",
                "G7: Non-obstetric",
                "G8: Undetermined",
                "Contributory",
                "Late maternal and sequelae",
                "Violence")

fill_cols <- setNames(c(brewer.pal(10, "Set3"), "black"), icd_levels)

deaths_summary <- deaths_icdmm %>%
    filter(!is.na(year) & between(year, 1998, 2024) & between(age, 10, 54)) %>%
    mutate(year = as.integer(year),
           group = case_when(any_violence == 1 ~ "Violence",
                             icd_mm_final %in% icd_levels ~ icd_mm_final,
                             TRUE ~ NA_character_)) %>%
    filter(!is.na(group)) %>%
    group_by(year, group) %>%
    summarise(n = n(), .groups = "drop")

maternal_data <- deaths_summary %>%
    mutate(group = fct_relevel(group, icd_levels))

year_breaks    <- sort(unique(deaths_summary$year))
labelled_years <- year_breaks[year_breaks %% 5 == 0]

fig2 <- ggplot() +
    geom_col(data = maternal_data,
             aes(x = year, y = n, fill = group),
             color = "black", linewidth = 0.1) +
    scale_x_continuous(breaks = labelled_years) +
    scale_y_continuous(label = comma) +
    scale_fill_manual(values = c(fill_cols, "black"),
                      drop   = TRUE,
                      name   = "Cause of death") +
    labs(x = "Year",
         y = "Number of deaths") +
    theme_minimal(base_size = 16) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.key.size = unit(0.55, "cm"),
          legend.box = "vertical",
          legend.box.just = "left",
          legend.margin = margin(t = 2, b = 2),
          axis.text.y = element_text(size = 14, color = "black"),
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1, color = "black")) +
    guides(fill  = guide_legend(nrow = 4, ncol=3, byrow = TRUE),
           color = guide_legend(override.aes = list(linetype = 1, shape = 16)))

ggsave(args$fig2, plot = fig2,
       width = 12, height = 8, dpi = 320, bg = "white")

# table S4
totals <- deaths_summary %>%
    group_by(group_label = group) %>%
    summarize(numeric_label = format(sum(n), big.mark = ",")) %>%
    pivot_wider(names_from = "group_label",
                values_from = "numeric_label") %>%
    mutate(year = "Total")

tabS4 <- deaths_summary %>%
    mutate(group = factor(group, levels = icd_levels)) %>%
    group_by(year) %>%
    mutate(prop = n / sum(n, na.rm = TRUE),
           numeric_label = str_squish(paste0(format(n, big.mark = ","), " (", round(prop * 100, 1), "%)")),
           group_label = group) %>%
    ungroup() %>%
    select(year, numeric_label, group_label) %>%
    pivot_wider(names_from = "group_label",
                values_from = "numeric_label") %>%
    select(year, all_of(icd_levels)) %>%
    mutate(year = as.character(year)) %>%
    bind_rows(totals) %>%
    gt(rowname_col = "year")%>%
    fmt_percent(columns = all_of(icd_levels), decimals = 1) %>%
    cols_label(.list = setNames(icd_levels, icd_levels))

gtsave(data = tabS4, filename = args$tabS4)


# figure S4
# check change in contributory conditions over time to preg or postpartum
contrib <- deaths_icdmm %>%
    filter(icd_mm_final == "Contributory")  %>%
    group_by(year) %>%
    summarise(total = n())

figS4_contrib <- ggplot(contrib, aes(x = year, y = total)) +
    geom_line(group = 1, colour = "#1f78b4", linewidth = 1) +
    geom_point(colour = "#1f78b4", size = 2) +
    labs(x = "Year",
         y = "Total deaths coded with contributory conditions") +
    ylim(0, NA) +
    theme_minimal() +
    theme(axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 16, color = "black"),
          axis.text.x = element_text(size = 16, angle = 45, hjust = 1, color = "black"))

ggsave(args$figS4, plot = figS4_contrib,
       width = 10, height = 8, dpi = 320, bg = "white")

# modalities of violence
violence_long <- deaths_icdmm %>%
    filter(any_violence == 1) %>%
    pivot_longer(cols = c(self_inflicted, external_violence),
                 names_to = "violence_type",
                 values_to = "count") %>%
    filter(count > 0)

# figure 3
# top 5 modalities per type and pregnancy status
violence_top5 <- violence_long %>%
    group_by(violence_type, pregnancy_status, lead_base_code) %>%
    summarise(total = sum(count), .groups = "drop") %>%
    group_by(violence_type, pregnancy_status) %>%
    arrange(desc(total), .by_group = TRUE) %>%
    mutate(rank = row_number(),
           proportion = total / sum(total)) %>%
    slice_head(n = 5) %>%
    ungroup() %>%
    mutate(description = case_when(lead_base_code == "X64" ~ "Intentional self-poisoning by other and unspecified drugs, medicaments, and biological substances",
                                   lead_base_code == "X68" ~ "Intentional self-poisoning by organic solvents and halogenated hydrocarbons",
                                   lead_base_code == "X69" ~ "Intentional self-poisoning by other and unspecified chemicals and noxious substances",
                                   lead_base_code == "X70" ~ "Intentional self-harm by hanging, strangulation, and suffocation",
                                   lead_base_code == "X74" ~ "Intentional self-harm by other and unspecified firearm discharge",
                                   lead_base_code == "X91" ~ "Assault by hanging, strangulation, and suffocation",
                                   lead_base_code == "X95" ~ "Assault by other and unspecified firearm discharge",
                                   lead_base_code == "X99" ~ "Assault by sharp object",
                                   lead_base_code == "Y09" ~ "Assault by unspecified means",
                                   lead_base_code == "Y34" ~ "Unspecified event, undetermined intent")) %>%
    verify(!is.na(description))

violence_top5 <- violence_top5 %>%
    mutate(pregnancy_status = if_else(pregnancy_status == "43 days-1 year postpartum",
                                      "43 days\u2013<1 year postpartum",
                                      pregnancy_status),
           pregnancy_status = factor(pregnancy_status,
                                     levels = c("Pregnant or <43 days postpartum",
                                                "43 days\u2013<1 year postpartum")))

# modalities by pregnancy timing - one for external and one for self-inflicted
external_top5 <- violence_top5 %>% filter(violence_type == "external_violence")

fig3a_external <- ggplot(external_top5,
                         aes(x = reorder_within(str_wrap(paste0(lead_base_code, " — ", description), 32),
                                                total,
                                                pregnancy_status),
                             y = total,
                             fill = pregnancy_status)) +
    geom_col(color = "black", linewidth = 0.1) +
    geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
              hjust = -0.05, size = 3.8) +
    coord_flip() +
    scale_x_reordered() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    facet_wrap(~pregnancy_status, scales = "free_y") +
    labs(x = "Modality",
         fill = "Pregnancy status",
         tag = "Panel A: External violence") +
    theme_minimal(base_size = 16) +
    theme(axis.text.x = element_text(size = 14),
          strip.text = element_blank(),
          strip.background = element_blank(),
          axis.title.x = element_blank(),
          plot.tag.position = "top",
          plot.tag = element_text(face = "bold", hjust = 0.5),
          legend.position = "none")

self_top5 <- violence_top5 %>% filter(violence_type == "self_inflicted")

fig3b_self_inflict <- ggplot(self_top5,
                             aes(x = reorder_within(str_wrap(paste0(lead_base_code, " — ", description), 32),
                                                    total,
                                                    pregnancy_status),
                                 y = total,
                                 fill = pregnancy_status)) +
    geom_col(color = "black", linewidth = 0.1) +
    geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
              hjust = -0.05, size = 3.8) +
    coord_flip() +
    scale_x_reordered() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    facet_wrap(~pregnancy_status, scales = "free_y") +
    labs(x = "Modality",
         y = "Number of deaths",
         fill = "Pregnancy status",
         tag = "Panel B: Self-inflicted violence") +
    theme_minimal(base_size = 16) +
    theme(axis.text.x = element_text(size = 14),
          strip.text = element_blank(),
          strip.background = element_blank(),
          plot.tag.position = "top",
          plot.tag = element_text(face = "bold", hjust = 0.5),
          legend.position = "bottom")

fig3_combined <- fig3a_external / fig3b_self_inflict

ggsave(args$fig3, plot = fig3_combined, bg = "white",
       width = 12, height = 14, units = "in", dpi = 320)

# figures S5 and S6
# keep only from 2004 onwards when categories were disaggregated
violence_long_2004 <- violence_long %>%
    filter(between(year, 2004, 2024))

# top 5 modalities per type and pregnancy status
violence_top5_disag <- violence_long_2004 %>%
    mutate(pregnancy_label = case_when(pregnancy_label %in% c("Pregnant", "Birth") ~ "During pregnancy or intrapartum",
                                       pregnancy_label == "43 days-1 year postpartum" ~ "43 days\u2013<1 year postpartum",
                                       TRUE ~ pregnancy_label)) %>%
    group_by(violence_type, pregnancy_label, lead_base_code) %>%
    summarise(total = sum(count), .groups = "drop") %>%
    group_by(violence_type, pregnancy_label) %>%
    arrange(desc(total), .by_group = TRUE) %>%
    mutate(rank = row_number(),
           proportion = total / sum(total)) %>%
    slice_head(n = 5) %>%
    ungroup() %>%
    mutate(description = case_when(lead_base_code == "X61" ~ "Intentional self-poisoning by and exposure to antiepileptic, sedative-hypnotic, antiparkinsonism and psychotropic drugs, not elsewhere classified",
                                   lead_base_code == "X64" ~ "Intentional self-poisoning by other and unspecified drugs, medicaments, and biological substances",
                                   lead_base_code == "X68" ~ "Intentional self-poisoning by organic solvents and halogenated hydrocarbons",
                                   lead_base_code == "X69" ~ "Intentional self-poisoning by other and unspecified chemicals and noxious substances",
                                   lead_base_code == "X70" ~ "Intentional self-harm by hanging, strangulation, and suffocation",
                                   lead_base_code == "X74" ~ "Intentional self-harm by other and unspecified firearm discharge",
                                   lead_base_code == "X91" ~ "Assault by hanging, strangulation, and suffocation",
                                   lead_base_code == "X95" ~ "Assault by other and unspecified firearm discharge",
                                   lead_base_code == "X99" ~ "Assault by sharp object",
                                   lead_base_code == "Y05" ~ "Sexual assault by bodily force",
                                   lead_base_code == "Y07" ~ "Other maltreatment",
                                   lead_base_code == "Y09" ~ "Assault by unspecified means",
                                   lead_base_code == "Y20" ~ " Hanging, strangulation and suffocation, undetermined intent",
                                   lead_base_code == "Y34" ~ "Unspecified event, undetermined intent")) %>%
    verify(!is.na(description))

external_top5 <- violence_top5_disag %>%
    filter(violence_type == "external_violence") %>%
    mutate(pregnancy_label = factor(pregnancy_label,
                                    levels = c("During pregnancy or intrapartum",
                                               "<43 days postpartum",
                                               "43 days\u2013<1 year postpartum")))

figS5 <- ggplot(external_top5,
                aes(x = reorder_within(str_wrap(paste0(lead_base_code, " - ", description), 32),
                                       total,
                                       pregnancy_label),
                    y = total,
                    fill = pregnancy_label)) +
    geom_col(color = "black", linewidth = 0.1) +
    geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
              hjust = -0.05, size = 3.4) +
    coord_flip() +
    scale_x_reordered() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    facet_wrap(~pregnancy_label, scales = "free_y", nrow = 2) +
    labs(x = "Modality",
         y = "Number of deaths",
         fill = "Pregnancy status") +
    theme_minimal(base_size = 16) +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold", size = 14),
          axis.text.x = element_text(size = 14))

ggsave(args$figS5, plot = figS5, bg = "white",
       width = 12, height = 12, units = "in", dpi = 320)

self_top5 <- violence_top5_disag %>%
    filter(violence_type == "self_inflicted") %>%
    mutate(pregnancy_label = factor(pregnancy_label,
                                    levels = c("During pregnancy or intrapartum",
                                               "<43 days postpartum",
                                               "43 days\u2013<1 year postpartum")))

figS6 <- ggplot(self_top5,
                aes(x = reorder_within(str_wrap(paste0(lead_base_code, " — ", description), 32),
                                       total,
                                       pregnancy_label),
                    y = total,
                    fill = pregnancy_label)) +
    geom_col(color = "black", linewidth = 0.1) +
    geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
              hjust = -0.05, size = 3.4) +
    coord_flip() +
    scale_x_reordered() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    facet_wrap(~ pregnancy_label, scales = "free_y", nrow = 2) +
    labs(x = "Modality",
         y = "Number of deaths",
         fill = "Pregnancy status") +
    theme_minimal(base_size = 16) +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold", size=14),
          axis.text.x = element_text(size = 14))

ggsave(args$figS6, plot = figS6, bg = "white",
       width = 13, height = 12, units = "in", dpi = 320)

# figure S7
# modalities over time by five year bins from 2008 onwards
violence_5yr_top5 <- violence_long %>%
    filter(between(year, 2008, 2024)) %>%
    mutate(year_bin = cut(year,
                          breaks = c(2008, 2013, 2018, 2025),
                          include.lowest = TRUE,
                          right = FALSE,
                          labels = c("2008\u20132012", "2013\u20132017", "2018\u20132024"))) %>%
    group_by(year_bin, violence_type, lead_base_code) %>%
    summarise(count = sum(count), .groups = "drop") %>%
    group_by(year_bin, violence_type) %>%
    mutate(total = sum(count),
           proportion = count / total) %>%
    slice_max(order_by = count, n = 5) %>%
    mutate(total = sum(count),
           proportion = count / total) %>%
    ungroup() %>%
    mutate(description = case_when(lead_base_code == "X64" ~ "Intentional self-poisoning by other and unspecified drugs, medicaments, and biological substances",
                                   lead_base_code == "X68" ~ "Intentional self-poisoning by organic solvents and halogenated hydrocarbons",
                                   lead_base_code == "X69" ~ "Intentional self-poisoning by other and unspecified chemicals and noxious substances",
                                   lead_base_code == "X70" ~ "Intentional self-harm by hanging, strangulation, and suffocation",
                                   lead_base_code == "X74" ~ "Intentional self-harm by other and unspecified firearm discharge",
                                   lead_base_code == "X91" ~ "Assault by hanging, strangulation, and suffocation",
                                   lead_base_code == "X95" ~ "Assault by other and unspecified firearm discharge",
                                   lead_base_code == "X99" ~ "Assault by sharp object",
                                   lead_base_code == "Y09" ~ "Assault by unspecified means",
                                   lead_base_code == "Y34" ~ "Unspecified event, undetermined intent")) %>%
    verify(!is.na(description))

figS7 <- violence_5yr_top5 %>%
    mutate(violence_type_label = if_else(violence_type == "external_violence",
                                         "Panel A: External violence",
                                         "Panel B: Self-inflicted violence")) %>%
    ggplot(aes(x = year_bin, y = proportion, fill = description)) +
    geom_col(position = "stack", color = "black",linewidth = 0.1) +
    facet_wrap(~violence_type_label, scales = "free_y", ncol = 1) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Period",
         y = "Proportion of violent deaths",
         fill = NULL) +
    theme_minimal(base_size = 16) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold", size=14),
          legend.position = "bottom",
          legend.text = element_text(size = 11)) +
    guides(fill = guide_legend(nrow = 10, byrow = TRUE))

ggsave(args$figS7, plot = figS7,
       width = 8, height = 10, dpi = 320, bg = "white")

# figure S8
wra_modality_comparison <- deaths_icdmm_all %>%
    filter(any_violence == 1 & between(age, 10, 54) & !is.na(age)) %>%
    mutate(preg_cat = case_when(pregnancy_status %in% c("Pregnant or <43 days postpartum", "43 days-1 year postpartum") ~ "Pregnant or <1 year postpartum",
                                pregnancy_status %in% c("Not pregnant", "1+ years postpartum", "Does not apply") ~ "Not pregnant or <1 year postpartum",
                                TRUE ~ NA_character_)) %>%
    filter(!is.na(preg_cat)) %>%
    pivot_longer(cols = c(self_inflicted, external_violence),
                 names_to = "violence_type",
                 values_to = "count") %>%
    filter(count > 0) %>%
    filter(year > 2003) %>%
    mutate(modality_description = case_when(lead_base_code %in% c("X72", "X73", "X74",
                                                                  "X93", "X94", "X95",
                                                                  "Y22", "Y23", "Y24") ~ "Firearm",
                                            lead_base_code %in% c("X70", "X91", "Y20") ~ "Hanging, strangulation, and suffocation",
                                            lead_base_code %in% c("X78", "X99", "Y28") ~ "Sharp object",
                                            lead_base_code %in% c("X84", "Y09", "Y34") ~ "Unknown",
                                            lead_base_code %in% c("X60", "X61", "X62",
                                                                  "X63", "X64", "X85",
                                                                  "Y10", "Y11", "Y12",
                                                                  "Y13", "Y14") ~ "Drugs, medicaments, and biological substances",
                                            lead_base_code %in% c("X66", "X67", "X68",
                                                                  "X69", "X86", "X87",
                                                                  "X88", "X89", "X90",
                                                                  "Y16", "Y17", "Y18",
                                                                  "Y19") ~ "Chemicals and noxious substances",
                                            TRUE ~ lead_base_code)) %>%
    group_by(violence_type, preg_cat, modality_description) %>%
    summarise(n = sum(count), .groups = "drop") %>%
    group_by(violence_type, preg_cat) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()

top5_external_desc <- wra_modality_comparison %>% filter(violence_type == "external_violence" &
                                                             preg_cat == "Pregnant or <1 year postpartum") %>%
    arrange(desc(prop)) %>%
    slice(1:5) %>%
    pull(modality_description)

top5_self_desc <- wra_modality_comparison %>% filter(violence_type == "self_inflicted" &
                                                         preg_cat == "Pregnant or <1 year postpartum") %>%
    arrange(desc(prop)) %>%
    slice(1:5) %>%
    pull(modality_description)

figS8 <- wra_modality_comparison %>%
    filter((violence_type == "external_violence" & modality_description %in% top5_external_desc) |
               (violence_type == "self_inflicted") & modality_description %in% top5_self_desc) %>%
    mutate(violence_label = if_else(violence_type == "self_inflicted",
                                    "Panel B: Self-inflicted violence",
                                    "Panel A: External violence")) %>%
    ggplot() +
    geom_bar(aes(x = modality_description, y = prop, fill = preg_cat),
             stat = "identity", position = "dodge") +
    facet_wrap(~violence_label, ncol = 1, scales = "free_y") +
    coord_flip() +
    labs(x = "", y = "% of violent deaths") +
    scale_fill_manual(values = c("Pregnant or <1 year postpartum" = "#1f78b4",
                                 "Not pregnant or <1 year postpartum" = "#a6cee3"),
                      name = "") +
    scale_y_continuous(labels = scales::label_percent()) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.key.size = unit(0.55, "cm"),
          legend.box = "vertical",
          legend.box.just = "left",
          legend.margin = margin(t = 2, b = 2),
          axis.text.y = element_text(size = 14, color = "black"),
          axis.text.x = element_text(size = 14, color = "black"),
          strip.text = element_text(face = "bold", size = 16)) +
    guides(fill  = guide_legend(nrow = 2, byrow = TRUE))

ggsave(args$figS8, figS8,
       width = 12, height = 10, dpi = 320, bg = "white")

# figure 4
mmr_colors <- rev(viridis(4, option = "plasma"))
names(mmr_colors) <- c("MMR", "MMR_suicide", "MMR_homicide", "MMR_violence_all")

mmr_long <- mmr %>%
    filter(year != 9999) %>%
    pivot_longer(cols = c(MMR, MMR_suicide, MMR_homicide, MMR_violence_all),
                 names_to = "metric",
                 values_to = "ratio") %>%
    mutate(metric = factor(metric, levels = names(mmr_colors)))

fig4a_mmr_abs <- ggplot(mmr_long, aes(x = year, y = ratio, color = metric)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = mmr_colors,
                       labels = c("MMR",
                                  "MMR + self-inflicted violence",
                                  "MMR + external violence",
                                  "MMR + all violence")) +
    labs(x = "Year",
         y = "Maternal Mortality Ratio",
         color = "Deaths included in MMR") +
    ylim(0, NA) +
    theme_minimal(base_size = 18) +
    theme(legend.position = "bottom") +
    labs(tag = "Panel A: Absolute change in MMR") +
    theme(plot.tag.position = "top",
          plot.tag = element_text(face = "bold", hjust = 0.5))

mmr_prop <- mmr %>%
    filter(between(year, 1998, 2025)) %>%
    pivot_longer(cols = c(MMR_suicide, MMR_homicide, MMR_violence_all),
                 names_to = "metric",
                 values_to = "ratio") %>%
    mutate(prop_change = ratio / MMR,
           metric = factor(metric, levels = names(mmr_colors)))

fig4b_mmr_prop <- ggplot(mmr_prop, aes(x = year, y = prop_change, color = metric)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = mmr_colors,
                       labels = c("MMR + self-inflicted violence",
                                  "MMR + external violence",
                                  "MMR + all violence")) +
    labs(y = "Relative change in MMR (%)",
         x = "Year",
         color = "Deaths included in MMR") +
    theme_minimal(base_size = 18) +
    theme(legend.position = "bottom") +
    labs(tag = "Panel B: Relative change in MMR") +
    theme(plot.tag.position = "top",
          plot.tag = element_text(face = "bold", hjust = 0.5))

fig4_mmr_combined <- (fig4a_mmr_abs / fig4b_mmr_prop) +
    theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5))

ggsave(args$fig4, fig4_mmr_combined,
       width = 13, height = 12, dpi = 320, bg = "white")

# figure 5
normalize_key <- function(x) {
    x %>%
        str_trim() %>%
        stri_trans_general("Latin-ASCII") %>%
        str_to_upper()
}

mx_sf <- ne_states(country = "Mexico", returnclass = "sf") %>%
    st_as_sf() %>%
    select(geo_name = name, geometry) %>%
    mutate(geo_name = normalize_key(geo_name),
           geo_name = if_else(geo_name == "DISTRITO FEDERAL",
                              "CIUDAD DE MEXICO",
                              geo_name))

mmr_prop_state <- mmr_state %>%
    pivot_longer(cols = c(MMR_suicide, MMR_homicide, MMR_violence_all),
                 names_to = "metric",
                 values_to = "ratio") %>%
    mutate(prop_change = ratio / MMR,
           metric = factor(metric, levels = names(mmr_colors)))

fig5 <- left_join(mx_sf, mmr_prop_state, by = c("geo_name" = "name")) %>%
    filter(!is.na(geo_name)) %>%
    mutate(geo_label = case_when(geo_name == "COLIMA" ~ "Colima",
                                 geo_name == "CHIHUAHUA" ~ "Chihuahua",
                                 geo_name == "GUANAJUATO" ~ "Guanajuato",
                                 TRUE ~ NA_character_)) %>%
    mutate(metric = case_when(metric == "MMR_suicide" ~ "MMR + self-inflicted violence",
                              metric == "MMR_homicide" ~ "MMR + external violence",
                              metric == "MMR_violence_all" ~ "MMR + all violence"),
           metric = factor(metric,
                           levels = c("MMR + self-inflicted violence",
                                      "MMR + external violence",
                                      "MMR + all violence"))) %>%
    ggplot() +
    geom_sf(aes(fill = prop_change), color = "black", linewidth = 0.2) +
    facet_wrap(~metric, ncol = 1) +
    scale_fill_viridis_c(option = "plasma",
                         labels = scales::percent_format(accuracy = 1),
                         name = "MMR change\nrelative to baseline") +
    coord_sf() +
    theme_void(base_size = 16) +
    theme(legend.position = "right",
          strip.clip = "off",
          plot.margin = margin(0,0, 0, 0),
          panel.spacing = unit(0.5, "lines"))

ggsave(args$fig5, fig5,
       width = 8, height = 12, dpi = 320, bg = "white")

# figures S9 and S10
normalize_key <- function(x) {
    x %>%
        str_trim() %>%
        stri_trans_general("Latin-ASCII") %>%
        str_to_upper()
}

mx_sf <- ne_states(country = "Mexico", returnclass = "sf") %>%
    st_as_sf() %>%
    select(geo_name = name, geometry) %>%
    mutate(key = normalize_key(geo_name))

states <- mmr_state %>%
    distinct(cod_ent_resid, name)

deaths_state_byyear <- deaths_icdmm_all %>%
    filter(between(age, 10, 54) & !is.na(age)) %>%
    mutate(preg_cat = case_when(pregnancy_status %in% c("Pregnant or <43 days postpartum", "43 days-1 year postpartum") ~ "Pregnant or <1 year postpartum",
                                pregnancy_status %in% c("Not pregnant", "1+ years postpartum", "Does not apply") ~ "Not pregnant or <1 year postpartum",
                                TRUE ~ NA_character_)) %>%
    group_by(year, cod_ent_resid, preg_cat) %>%
    summarise(preg_related = n(),
              maternal = sum(maternal == 1, na.rm = TRUE),
              self_inflicted_n  = sum(self_inflicted == 1, na.rm = TRUE),
              external_n = sum(external_violence == 1, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(cod_ent_resid = as.numeric(cod_ent_resid))

deaths_state_byyear <- left_join(deaths_state_byyear, states, by = "cod_ent_resid")
deaths_state_byyear <- deaths_state_byyear %>%
    filter(!is.na(name) & between(year, 1998, 2024)) %>% # drop missing
    mutate(year_cat = cut(year, breaks = c(2008, 2017, 2025), right = FALSE,
                          labels = c("2008\u20132016", "2017\u20132024")))

# edit names to match sf object
unique(deaths_state_byyear$name)
unique(mx_sf$key)
unique(deaths_state_byyear$name)[!(unique(deaths_state_byyear$name) %in% unique(mx_sf$key))]
deaths_state_byyear <- deaths_state_byyear %>%
    mutate(name = case_when(
        # name == "BAJA CALIFORNIA"  ~ "BAJACALIFORNIA" ,
        # name == "BAJA CALIFORNIA SUR"  ~ "BAJACALIFORNIASUR" ,
        name == "CIUDAD DE MEXICO"  ~ "DISTRITO FEDERAL",
        # name == "NUEVO LEON"   ~ "NUEVOLEON"  ,
        # name == "QUINTANA ROO"  ~ "QUINTANAROO"  ,
        # name == "SAN LUIS POTOSI"   ~  "SANLUISPOTOSI"   ,
        TRUE ~ name
    ))

# figure S9, pregnant and postpartum women only
state_cor_byyearcat <- deaths_state_byyear %>%
    filter(!is.na(year_cat) & preg_cat %in% "Pregnant or <1 year postpartum") %>%
    group_by(name, year, year_cat) %>%
    summarise(self_inflicted_n = sum(self_inflicted_n),
              external_n = sum(external_n)) %>%
    group_by(name, year_cat) %>%
    summarise(corval = cor(self_inflicted_n, external_n))

figS9 <- mx_sf %>%
    left_join(state_cor_byyearcat, by = c("key" = "name")) %>%
    filter(!is.na(year_cat)) %>%
    mutate(geo_name = ifelse(key == "DISTRITO FEDERAL", "★", geo_name)) %>%
    ggplot() +
    geom_sf(aes(fill = corval), color = "black", linewidth = 0.2) +
    geom_sf_label( # regular labels
        data = ~ dplyr::filter(.x, !(key %in% c("QUERETARO", "PUEBLA", "MEXICO"))),
        aes(label = str_wrap(geo_name, width = 14)),
        size = 2, fill = "white", color = "black", alpha = 0.7, label.size = 0) +
    geom_sf_label( # nudge up
        data = ~ dplyr::filter(.x, key %in% c("QUERETARO", "MEXICO")),
        aes(label = str_wrap(geo_name, width = 14)),
        position = position_nudge(y = 0.5),
        size = 2, fill = "white", color = "black",alpha = 0.7, label.size = 0) +
    geom_sf_label( # nudge down
        data = ~ dplyr::filter(.x, key %in% c("PUEBLA")),
        aes(label = str_wrap(geo_name, width = 14)),
        position = position_nudge(y = -0.5),
        size = 2, fill = "white", color = "black",alpha = 0.7, label.size = 0) +
    facet_wrap(~year_cat, ncol = 1) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0,
                         name = "Correlation") +
    coord_sf(expand = FALSE) +
    theme_void(base_size = 13) +
    theme(legend.position = "right",
          strip.clip = "off",
          plot.margin = margin(0,0, 0, -10),
          panel.spacing = unit(0.5, "lines"))
figS9

ggsave(args$figS9, figS9, width = 8, height = 9, dpi = 320, bg = "white")

# figure S10
state_cor_bypregcat <- deaths_state_byyear %>%
    filter(!is.na(preg_cat)) %>%
    group_by(name, year, preg_cat) %>%
    summarise(preg_related = sum(preg_related),
              maternal = sum(maternal),
              self_inflicted_n  = sum(self_inflicted_n),
              external_n = sum(external_n)) %>%
    group_by(name, preg_cat) %>%
    summarise(corval = cor(self_inflicted_n, external_n))

figS10 <- mx_sf %>%
    left_join(state_cor_bypregcat, by = c("key" = "name")) %>%
    filter(!is.na(preg_cat)) %>%
    mutate(geo_name = ifelse(key == "DISTRITO FEDERAL", "★", geo_name)) %>%
    ggplot() +
    geom_sf(aes(fill = corval), color = "black", linewidth = 0.2) +
    geom_sf_label( # regular labels
        data = ~ dplyr::filter(.x, !(key %in% c("QUERETARO", "PUEBLA", "MEXICO"))),
        aes(label = str_wrap(geo_name, width = 14)),
        size = 2, fill = "white", color = "black", alpha = 0.7, label.size = 0) +
    geom_sf_label( # nudge up
        data = ~ dplyr::filter(.x, key %in% c("QUERETARO", "MEXICO")),
        aes(label = str_wrap(geo_name, width = 14)),
        position = position_nudge(y = 0.5),
        size = 2, fill = "white", color = "black",alpha = 0.7, label.size = 0) +
    geom_sf_label( # nudge down
        data = ~ dplyr::filter(.x, key %in% c("PUEBLA")),
        aes(label = str_wrap(geo_name, width = 14)),
        position = position_nudge(y = -0.5),
        size = 2, fill = "white", color = "black",alpha = 0.7, label.size = 0) +
    facet_wrap(~preg_cat, ncol = 1) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0,
                         name = "Correlation") +
    coord_sf(expand = FALSE) +
    theme_void(base_size = 13) +
    theme(legend.position = "right",
          strip.clip = "off",
          #legend.text = element_text(angle = 45, hjust = 1)
          plot.margin = margin(0,0, 0, -10),
          panel.spacing = unit(0.5, "lines"))

ggsave(args$figS10, figS10, width = 8, height = 9, dpi = 320, bg = "white")

# done.

