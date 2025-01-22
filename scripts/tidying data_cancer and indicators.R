# Load packages 
library(rio) 
library(here)
library(tidyverse)
library(janitor)
library(rstatix)

# Import data

## Epidemiological indicators
globocan_mortality <-
  rio::import(here::here("data", "raw", "globocan_mortality.csv")) |>
  janitor::clean_names() |>
  dplyr::filter(label != "Total") |>
  dplyr::select(-sex) |>
  dplyr::mutate(indicator = "mortality")

globocan_incidence <-
  rio::import(here::here("data", "raw", "globocan_incidence.csv")) |>
  janitor::clean_names() |>
  dplyr::filter(label != "Total") |>
  dplyr::select(-sex)

## Define common columns for merging
common_columns <- c("alpha_3_code", "cancer_code", "label", "population_code_iso_un", "country")

## Merge incidence and mortality data and calculate MIR
globocan <- globocan_incidence |>
  dplyr::left_join(globocan_mortality, by = common_columns) |>
  dplyr::mutate(MIR = round(asr_world.y/asr_world.x, 2), indicator = "MIR")

# globocan_incidence |> merge(globocan_mortality, by = common_columns, all.x = TRUE)

# globocan |> dplyr::select(asr_world.x, asr_world.y, MIR) |> summary() # Check NA's

# globocan |> dplyr::filter(if_all(starts_with("asr"), ~ !is.na(.))) # Filter NA's in indicators

# Socioeconomic development indicators (HDI, SDI, and EdI)

## Import HDI data for 2022
hdi_and_comp_2022 <- import(here("data", "raw", "hdi_and_composites_2022.csv"))

## Import HDI data for 2017
hdi_and_comp_2017 <- import(here("data", "raw", "hdi_and_composites_2017.csv"))

## Import SDI data for 2018 to 2021
sdi_2018_to_2021 <- import(here("data", "raw", "sdi_2018_to_2021.csv"))

## Import GDB SDI and quintiles data for 2021
gdb_sdi_2021_and_quintiles <- import(here("data", "raw", "ihme_gdb_sdi_2021_and_quintiles.csv"))

# Complementary data

## Import world regions data
regiones_mundo <- import(here("data", "raw", "regiones_mundo.txt")) |>
  janitor::clean_names()

## Import cancer code data
cancer_code <- import(here("data", "raw", "cancer_code.csv")) |>
  janitor::clean_names()

# Function to calculate the Education and Income index (EdI)
calculate_EdI = function(
    EYS,
    MYS,
    GNI_per_capita,
    max_EYS = 20,
    max_MYS = 15,
    min_GNI = 100,
    max_GNI = 75000) {
  
  # Normalize education indicators
  norm_EYS <- EYS / max_EYS
  norm_MYS <- MYS / max_MYS
  education_index <- (norm_EYS + norm_MYS) / 2
  
  # Normalize GNI per capita
  norm_GNI <-
    (log(GNI_per_capita) - log(min_GNI)) / (log(max_GNI) - log(min_GNI))
  
  # Calculate EdI
  EdI <- sqrt(education_index * norm_GNI)
  
  return(EdI)
}

# Calculation and categorization of EdI

## Apply the 'calculate_EdI' function to the data
hdi_and_comp_2022_0 <- hdi_and_comp_2022 |>
  dplyr::mutate(
    Education_and_Income =
      calculate_EdI(
        MYS = Mean_years_of_schooling_y,
        EYS = Expected_years_of_schooling_y,
        GNI_per_capita = GNI_per_capita_2017PPP_dolar
      ),
    edi_categories = case_when(
      Education_and_Income < .53 ~ "Low",
      Education_and_Income >= .53 & Education_and_Income < .70 ~ "Medium",
      Education_and_Income >= .70 & Education_and_Income < .80 ~ "High",
      Education_and_Income >= .80 ~ "Very high"
    )
  )

# Tidying data

## Epidemiological data
globocan_0 <- merge(globocan, cancer_code, by = "cancer_code", all.x = TRUE)

globocan_0 <- globocan_0 |>
  dplyr::rename(cancer_type = label.y, Country_name = label.x) |>
  dplyr::filter(Country_name != "Total")

countries_with_epid_data <- unique(globocan_0$Country_name)

# Processing and Joining SDI and HDI data with epidemiological data

## Filter the SDI data for 2018 to 2021
sdi_2018_to_2021_0 <- sdi_2018_to_2021 |>
  dplyr::filter(Country %in% countries_with_epid_data)

## Clean column names in the SDI and quintiles data for 2021
gdb_sdi_2021_and_quintiles <- gdb_sdi_2021_and_quintiles |> 
  janitor::clean_names()

## Select relevant columns and rename
gdb_sdi_2021_and_quintiles_0 <- gdb_sdi_2021_and_quintiles |>
  dplyr::select(-location_id) |>
  dplyr::rename(
    Country_name = location_name,
    sdi_gdb_2021 = x2021_sdi_index_value,
    sdi_categories = sdi_quintile) |> 
  dplyr::filter(gdb_sdi_2021_and_quintiles$location_name %in% countries_with_epid_data)

## Filter HDI data for 2017 and 2022
hdi_and_comp_2017_0 <- hdi_and_comp_2017 |>
  dplyr::filter(hdi_and_comp_2017$Country %in% countries_with_epid_data)

hdi_and_comp_2022_0 <- hdi_and_comp_2022_0 |>
  dplyr::filter(hdi_and_comp_2022_0$Country %in% countries_with_epid_data) |>
  dplyr::rename(Country_name = Country)

## Identify countries that do not have indicator data
dplyr::setdiff(countries_with_epid_data, gdb_sdi_2021_and_quintiles$Location.Name)

## Rename the 'Country' column to 'Country_name'
sdi_2018_to_2021_0 <- sdi_2018_to_2021_0 |>
  dplyr::rename(Country_name = "Country")

## Join epidemiological data with HDI data for 2022
globocan_1 = globocan_0 |>
  dplyr::left_join(hdi_and_comp_2022_0, by = "Country_name")

## Join epidemiological data with SDI data for 2018 to 2021
globocan_2 <- globocan_1 |>
  dplyr::left_join(sdi_2018_to_2021_0, by = "Country_name")

## Join epidemiological data with SDI and quintiles data for 2021
globocan_3 <- globocan_2 |>
  dplyr::left_join(gdb_sdi_2021_and_quintiles_0, by = "Country_name")

## Clean column names to remove '_y' suffixes
colnames(globocan_3) = gsub("\\_y$", "", colnames(globocan_3))
globocan_3 = janitor::clean_names(globocan_3)

## Define the cuts for the ASR and MIR quartiles
incidence_cuts <- c(-Inf, 113.5, 152.5, 227.3, Inf) 
mortality_cuts <- c(-Inf, 74.90, 89.50, 103.20, Inf) 
mir_cuts <- c(-Inf, 0.4600, 0.5700, 0.6900, Inf)

## Classification of cancers
globocan_4 <- globocan_3 |>
  dplyr::mutate(
    neoplasia_sex = case_when(
      cancer_type %in% c(
        "Lip, oral cavity", "Salivary glands", "Oropharynx", "Nasopharynx",
        "Hypopharynx", "Oesophagus", "Stomach", "Colon", "Rectum", "Anus",
        "Liver and intrahepatic bile ducts", "Gallbladder", "Pancreas",
        "Larynx", "Lung", "Melanoma of skin",
        "Non-melanoma skin cancer", "Mesothelioma", "Kaposi sarcoma",
        "Breast", "Kidney", "Bladder", "Thyroid", "Hodgkin lymphoma",
        "Non-Hodgkin lymphoma", "Multiple myeloma", "Leukaemia",
        "All cancers excl. non-melanoma skin cancer", "Colorectum",
        " Brain, central nervous system") ~ "Both",
      cancer_type %in% c(
        "Vulva", "Vagina", "Cervix uteri", "Ovary", "Corpus uteri") ~ "Female",
      cancer_type %in% c(
        "Penis", "Prostate", "Testis") ~ "Male"
    ),
    tumor_types = case_when(
      cancer_type %in% c(
        "Lip, oral cavity", "Oropharynx", "Nasopharynx", "Hypopharynx", "Larynx",
        "Vulva", "Vagina", "Penis") ~ "Squamous cell carcinoma",
      cancer_type %in% c(
        "Salivary glands", "Stomach", "Colon", "Colorectum", "Rectum", "Pancreas",
        "Breast", "Ovary", "Corpus uteri", "Prostate", "Testis", "Kidney",
        "Thyroid") ~ "Adenocarcinoma",
      cancer_type %in% c(
        "Gallbladder", "Oesophagus", "Anus", "Lung",
        "Cervix uteri") ~ "Squamous cell/Adenocarcinoma",
      cancer_type %in% c("Hodgkin lymphoma", "Non-Hodgkin lymphoma") ~ "Lymphoma",
      cancer_type %in% c("Melanoma of skin", "Non-melanoma skin cancer") ~ "Skin",
      TRUE ~ "Others"
    ), 
    malignant_neoplasms_types = case_when(
      cancer_type %in% c(
        "Lip, oral cavity", "Salivary glands", "Oropharynx", "Nasopharynx", 
        "Hypopharynx") ~ "Lip, oral cavity and pharynx",
      cancer_type %in% c(
        "Oesophagus", "Stomach", "Colon", "Colorectum", "Rectum", "Anus",
        "Liver and intrahepatic bile ducts", "Gallbladder", 
        "Pancreas") ~ "Digestive organs",
      cancer_type %in% c(
        "Larynx", "Lung") ~ "Respiratory and intrathoracic organs",
      cancer_type %in% c(
        "Melanoma of skin",
        "Non-melanoma skin cancer") ~ "Melanoma and other malignant neoplasms of skin",
      cancer_type %in% c(
        "Mesothelioma", "Kaposi sarcoma") ~ "Mesothelial and soft tissue",
      cancer_type == "Breast" ~ "Breast",
      cancer_type %in% c(
        "Vulva", "Vagina", "Cervix uteri", "Ovary", 
        "Corpus uteri") ~ "Female genital organs",
      cancer_type %in% c("Penis", "Prostate", "Testis") ~ "Male genital organs",
      cancer_type %in% c("Kidney", "Bladder") ~ "Urinary tract",
      cancer_type %in% c(
        "Hodgkin lymphoma", "Non-Hodgkin lymphoma", "Multiple myeloma",
        "Leukaemia") ~ "Lymphoid, haematopoietic and related tissue",
      cancer_type == "Thyroid" ~ "Thyroid and other endocrine glands",
      cancer_type == " Brain, central nervous system" ~ "Eye, brain and other parts of central nervous system",
      TRUE ~ "Others"
    ))

## Define the cuts for the ASR and MIR quartiles
incidence_cuts <- c(-Inf, 113.5, 152.5, 227.3, Inf) 
mortality_cuts <- c(-Inf, 74.90, 89.50, 103.20, Inf) 
mir_cuts <- c(-Inf, 0.4600, 0.5700, 0.6900, Inf)

## Apply the cuts using cut()

globocan_4 <- globocan_4 |> 
  dplyr::mutate(
    asr_incidence_quartiles = case_when(
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator_x == "incidence" ~ cut(
          asr_world_x,
          breaks = incidence_cuts,
          labels = c("incidence_Q1", "incidence_Q2", "incidence_Q3", "incidence_Q4"))),
    asr_mortality_quartiles = case_when(
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator_y == "mortality" ~ cut(
          asr_world_y,
          breaks = mortality_cuts,
          labels = c("mortality_Q1", "mortality_Q2", "mortality_Q3", "mortality_Q4"))),
    mir_quartiles = case_when(
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator == "MIR" ~ cut(
          mir,
          breaks = mir_cuts,
          labels = c("MIR_Q1", "MIR_Q2", "MIR_Q3", "MIR_Q4"))))

## Merge globocan data with world regions data
globocan_5 <- globocan_4 |> 
  dplyr::left_join(regiones_mundo, by = "country_name")

## Select relevant columns and reshape data
globocan_6 <- globocan_5 |>
  dplyr::select(
    cancer_type,
    country_name,
    crude_rate_x,
    crude_rate_y,
    mir,
    asr_world_x,
    asr_world_y,
    human_development_index,
    education_and_income,
    sdi_gdb_2021,
    hdi_category,
    edi_categories,
    sdi_categories,
    neoplasia_sex
  ) |> 
  tidyr::pivot_longer(
    cols = c("mir", "asr_world_x", "asr_world_y"),
    names_to = "indicator",
    values_to = "asr_world"
  ) |> 
  dplyr::mutate(
    # Combine crude rates from different sources
    crude_rate = dplyr::coalesce(crude_rate_x, crude_rate_y),
    # Recode indicator names
    indicator = factor(indicator) |>
      forcats::fct_recode(
        "Incidence" = "asr_world_x", 
        "Mortality" = "asr_world_y",
        "MIR" = "mir"),
    # Create country labels for specific countries
    country_label = case_when(
      country_name %in% c(
        "United States of America", "Bolivia", "Spain", "Ethiopia", "China",
        "Japan", "India", "Peru", "Egypt", "Saudi Arabia", 
        "France (metropolitan)", "Uganda") ~ as.character(country_name),
      TRUE ~ NA_character_
    ) |>
      forcats::fct_recode(
        "USA" = "United States of America", "France" = "France (metropolitan)")
  ) |>
  # Rename columns
  dplyr::rename(
    `ASR (World)` = asr_world,
    `Crude rate` = crude_rate,
    HDI = human_development_index,
    EdI = education_and_income,
    SDI = sdi_gdb_2021,
    Indicator = indicator)

## Remove duplicate rows based on cancer_type, country_name, and Indicator
globocan_7 <- globocan_6 |>
  dplyr::distinct(cancer_type, country_name, Indicator, .keep_all = TRUE)

## Recode specific cancer types and clean column names
globocan_8 <- globocan_7 |>
  dplyr::mutate(
    cancer_type = case_when(
      cancer_type == " Brain, central nervous system" ~ "Brain, nervous system",
      cancer_type == "Liver and intrahepatic bile ducts" ~ "Liver",
      cancer_type == "All cancers excl. non-melanoma skin cancer" ~ "All cancers*",
      cancer_type == "Trachea, bronchus and lung" ~ "Lung",
      .default = as.character(cancer_type))
  ) |>
  janitor::clean_names()

# R2 heatmap data (ASR or MIR ~ HDI, EdI, and SDI by cancer type)

## Function to calculate R2 for incidence, mortality, and MIR
calculate_r2 <- function(data, indicator_value) {

  filtered_data <- data |>
    dplyr::filter(indicator == indicator_value) |>
    dplyr::select(cancer_type, hdi, ed_i, sdi, asr_world) |>
    tidyr::drop_na()
  
  correlations <- filtered_data |>
    dplyr::group_by(cancer_type) |>
    dplyr::summarise(
      hdi_cor = cor(asr_world, hdi),
      edi_cor = cor(asr_world, ed_i),
      sdi_cor = cor(asr_world, sdi)
    )
  
  r2 <- correlations |>
    dplyr::mutate(
      HDI = round(hdi_cor^2, 2),
      EdI = round(edi_cor^2, 2),
      SDI = round(sdi_cor^2, 2)
    ) |>
    dplyr::select(cancer_type, HDI, EdI, SDI) |>
    tidyr::pivot_longer(cols = c(HDI, EdI, SDI), names_to = "indicator", values_to = "R2")
  
  filtered_r2 <- r2 |>
    dplyr::filter(R2 > 0.0)
  
  data_ordered <- filtered_r2 |>
    dplyr::group_by(cancer_type) |>
    dplyr::summarise(max_R2 = max(R2, na.rm = TRUE)) |>
    dplyr::arrange(max_R2)
  
  final_data <- filtered_r2 |>
    dplyr::mutate(cancer_type = factor(cancer_type, levels = data_ordered$cancer_type))
  
  return(final_data)
}

## Calculate R2 for incidence and mortality
r2_incidence <- calculate_r2(globocan_8, "Incidence")

## Calculate R2 for mortality
r2_mortality <- calculate_r2(globocan_8, "Mortality")

## Calculate R2 for MIR
r2_mir <- calculate_r2(globocan_8, "MIR")

# Supplementary figure data of group A cancers by EdI

## Group A
group_a <- c(
  "Breast",
  "Prostate",
  "Colorectum",
  "Melanoma of skin",
  "Colon",
  "Corpus uteri",
  "Bladder",
  "Non-Hodgkin lymphoma",
  "Rectum",
  "Kidney",
  "Thyroid",
  "Leukaemia",
  "Testis",
  "Multiple myeloma",
  "Lip, oral cavity",
  "Hodgkin lymphoma",
  "Oropharynx",
  "Vulva",
  "Anus"
)

## ASR by EdI
s_figure_a_data <- globocan_8 |> 
  dplyr::filter(indicator != "MIR", cancer_type %in% group_a) |>
  dplyr::mutate(cancer_type = factor(cancer_type, levels = group_a))

## MIR by EdI
s_figure_mir_a_data <- globocan_8 |> 
  dplyr::filter(indicator == "MIR", cancer_type %in% group_a) |>
  dplyr::mutate(cancer_type = factor(cancer_type, levels = group_a))

# Supplementary figure data of group B cancers by EdI

## Group B
group_b <-c(
  "Lung",
  "Pancreas",
  "Brain, nervous system",
  "Mesothelioma")

## ASR by EdI
s_figure_b_data <- globocan_8 |>
  dplyr::filter(indicator != "MIR", cancer_type %in% group_b) |>
  dplyr::mutate(cancer_type = factor(cancer_type, levels = group_b))

## MIR by EdI
s_figure_mir_b_data <- globocan_8 |>
  dplyr::filter(indicator == "MIR", cancer_type %in% group_b) |>
  dplyr::mutate(cancer_type = factor(cancer_type, levels = group_b))

# Supplementary figure data of group C cancers by EdI

## Group C
group_c <- c(
  "Stomach",
  "Cervix uteri",
  "Oesophagus",
  "Larynx",
  "Vagina",
  "Kaposi sarcoma")

## ASR by EdI
s_figure_c_data <- globocan_8 |>
  dplyr::filter(indicator != "MIR", cancer_type %in% group_c) |>
  dplyr::mutate(cancer_type = factor(cancer_type, levels = group_c))

## MIR by EdI
s_figure_mir_c_data <- globocan_8 |>
  dplyr::filter(indicator == "MIR", cancer_type %in% group_c) |>
  dplyr::mutate(cancer_type = factor(cancer_type, levels = group_c))

# Supplementary figure data of group D cancers by EdI

## Group D
group_d <- c(
  "Ovary",
  "Liver",
  "Hypopharynx",
  "Gallbladder",
  "Penis",
  "Salivary glands",
  "Nasopharynx"
)

## ASR by EdI
s_figure_d_data <- globocan_8 |>
  dplyr::filter(indicator != "MIR", cancer_type %in% group_d) |>
  dplyr::mutate(cancer_type = factor(cancer_type, levels = group_d))

## MIR by EdI
s_figure_mir_d_data <- globocan_8 |>
  dplyr::filter(indicator == "MIR", cancer_type %in% group_d) |>
  dplyr::mutate(cancer_type = factor(cancer_type, levels = group_d))

# One-Way ANOVA test

## Filter cancer incidence by EdI categories
data_incidence_edi_globocan_8 <- globocan_8 |>
  dplyr::filter(
    indicator == "Incidence" &
      cancer_type %in% c(
        "Breast",
        "Hodgkin lymphoma",
        "Non-Hodgkin lymphoma",
        "Leukaemia",
        "Brain, nervous system"
      ) & !is.na(edi_categories)
  ) |>
  dplyr::mutate(edi_categories = factor(edi_categories) |>
                  fct_relevel("Low", "Medium", "High", "Very high"))

## Games-Howell test for cancer incidence and adjustment of 'y' positions
pw_gh_test_incidence_edi <- data_incidence_edi_globocan_8 |>
  dplyr::group_by(cancer_type) |>
  rstatix::games_howell_test(asr_world ~ edi_categories)  |>
  dplyr::mutate(
    y.position = case_when(
      cancer_type == "Brain, nervous system" ~ 8.5,
      cancer_type == "Breast" ~ 105,
      cancer_type == "Hodgkin lymphoma" ~ 3.5,
      cancer_type == "Leukaemia" ~ 12,
      TRUE ~ 14.2  # Default y.position for other cancer types
    )
  ) |>
  dplyr::filter(
    (group1 == "Low" & group2 == "Medium") |
      (group1 == "Medium" & group2 == "High") |
      (group1 == "High" & group2 == "Very high") |
      (group1 == "High" & group2 == "Medium") |
      (group1 == "Very high" & group2 == "High")
  )

## Filter cancer mortality by EdI categories
data_mortality_edi_globocan_8 <- globocan_8 |>
  dplyr::filter(
    indicator == "Mortality" &
      cancer_type %in% c("Brain, nervous system") &
      !is.na(edi_categories)
  ) |>
  dplyr::mutate(edi_categories = factor(edi_categories) |>
                  fct_relevel("Low", "Medium", "High", "Very high"))

## Games-Howell test for cancer mortality and adjustment of 'y' positions
pw_gh_test_mortality_edi <- data_mortality_edi_globocan_8 |>
  dplyr::group_by(cancer_type) |>
  rstatix::games_howell_test(asr_world ~ edi_categories) |>
  dplyr::mutate(y.position = case_when(cancer_type == "Brain, nervous system" ~ 7)) |>
  dplyr::filter((
    group1 == "Low" & group2 == "Medium") |
      (group1 == "High" & group2 == "Medium") |
      (group1 == "High" & group2 == "Very high")
  )

## Filter cancer MIR by EdI categories
data_mir_edi_globocan_8 <- globocan_8 |>
  dplyr::filter(
    indicator == "MIR" &
      cancer_type %in% c(
        "Prostate",
        "All cancers*",
        "Non-Hodgkin lymphoma",
        "Cervix uteri",
        "Rectum"
      ) & !is.na(edi_categories)
  ) |>
  dplyr::mutate(edi_categories = factor(edi_categories) |>
                  fct_relevel("Low", "Medium", "High", "Very high"))

## Games-Howell test for cancer MIR and adjustment of 'y' positions
pw_gh_test_mir_edi <- data_mir_edi_globocan_8 |>
  dplyr::group_by(cancer_type) |>
  rstatix::games_howell_test(asr_world ~ edi_categories) |>
  dplyr::mutate(
    y.position = case_when(
      cancer_type == "Prostate" ~ 0.75,
      cancer_type == "All cancers excl. non-melanoma skin cancer" ~ 0.8,
      cancer_type == "Rectum" ~ 0.98,
      cancer_type == "Cervix uteri" ~ 0.88,
      TRUE ~ 0.85
    )
  ) |>
  dplyr::filter(
    (group1 == "Low" & group2 == "Medium") |
      (group1 == "Medium" & group2 == "High") |
      (group1 == "High" & group2 == "Very high") |
      (group1 == "High" & group2 == "Medium") |
      (group1 == "Very high" & group2 == "High")
  )

## Filter cancer incidence by HDI categories
data_incidence_hdi_globocan_8 <- globocan_8 |>
  dplyr::filter(
    indicator == "Incidence" &
      cancer_type %in% c(
        "Breast",
        "Hodgkin lymphoma",
        "Non-Hodgkin lymphoma",
        "Brain, nervous system"
      ) & !is.na(hdi_category)
  ) |>
  dplyr::mutate(hdi_category = factor(hdi_category) |>
                  fct_relevel("Low", "Medium", "High", "Very high"))


## Games-Howell test for cancer incidence and adjustment of 'y' positions
pw_gh_test_incidence_hdi <- data_incidence_hdi_globocan_8 |>
  dplyr::group_by(cancer_type) |> 
  rstatix::games_howell_test(asr_world ~ hdi_category) |>
  dplyr::mutate(y.position = case_when(
    cancer_type == "Brain, nervous system" ~ 8,
    cancer_type == "Breast" ~ 105,
    cancer_type == "Hodgkin lymphoma" ~ 3.5,
    TRUE ~ 15
  )) |> 
  dplyr::filter((group1 == "Low" & group2 == "Medium") |
                  (group1 == "Medium" & group2 == "High") |
                  (group1 == "High" & group2 == "Very high") |
                  (group1 == "High" & group2 == "Medium") |
                  (group1 == "Very high" & group2 == "High"))

## Filter cancer mortality by HDI categories
data_mortality_hdi_globocan_8 <- globocan_8 |>
  dplyr::filter(
    indicator == "Mortality" &
      cancer_type %in% c(
        "Kidney",
        "Brain, nervous system",
        "Colon"
        ) & !is.na(hdi_category)
  ) |>
  dplyr::mutate(hdi_category = factor(hdi_category) |>
                  fct_relevel("Low", "Medium", "High", "Very high"))

## Games-Howell test for cancer mortality and adjustment of 'y' positions
pw_gh_test_mortality_hdi <- data_mortality_hdi_globocan_8 |>
  dplyr::group_by(cancer_type) |>
  rstatix::games_howell_test(asr_world ~ hdi_category) |>
  dplyr::mutate(
    y.position = case_when(
      cancer_type == "Kidney" ~ 4.3,
      cancer_type == "Brain, nervous system" ~ 6.7,
      cancer_type == "Colon" ~ 13
    )
  ) |>
  dplyr::filter(
    (group1 == "Low" & group2 == "Medium") |
      (group1 == "Medium" & group2 == "High") |
      (group1 == "High" & group2 == "Very high") |
      (group1 == "High" & group2 == "Medium") |
      (group1 == "Very high" & group2 == "High")
  )

## Filter cancer MIR by HDI categories
data_mir_hdi_globocan_8 <- globocan_8 |>
  dplyr::filter(
    indicator == "MIR" &
      cancer_type %in% c(
        "Prostate",
        "All cancers*",
        "Rectum",
        "Non-Hodgkin lymphoma",
        "Leukaemia"
      ) & !is.na(hdi_category)
  ) |>
  dplyr::mutate(hdi_category = factor(hdi_category) |>
                  fct_relevel("Low", "Medium", "High", "Very high"))

## Games-Howell test for cancer MIR and adjustment of 'y' positions
pw_gh_test_mir_hdi <- data_mir_hdi_globocan_8 |>
  dplyr::group_by(cancer_type) |>
  rstatix::games_howell_test(asr_world ~ hdi_category) |>
  dplyr::mutate(
    y.position = case_when(
      cancer_type == "Prostate" ~ 0.70,
      cancer_type == "All cancers*" ~ 0.8,
      cancer_type == "Rectum" ~ 0.92,
      cancer_type == "Non-Hodgkin lymphoma" ~ 0.90,
      cancer_type == "Leukaemia" ~ 1
    )
  ) |>
  dplyr::filter(
    (group1 == "Low" & group2 == "Medium") |
      (group1 == "Medium" & group2 == "High") |
      (group1 == "High" & group2 == "Very high") |
      (group1 == "High" & group2 == "Medium") |
      (group1 == "Very high" & group2 == "High")
  )

## Filter cancer incidence by SDI categories
data_incidence_sdi_globocan_8 <- globocan_8 |>
  dplyr::filter(
    indicator == "Incidence" &
      cancer_type %in% c(
        "Hodgkin lymphoma",
        "Leukaemia",
        "Brain, nervous system",
        "Breast",
        "Prostate"
      ) &
      !is.na(sdi_categories) & sdi_categories != "" & !is.na(asr_world)
  ) |>
  dplyr::mutate(sdi_categories = factor(sdi_categories) |>
                  fct_relevel("Low", "Medium", "High", "Very high"))

## Games-Howell test for cancer incidence and adjustment of 'y' positions
pw_gh_test_incidence_sdi <- data_incidence_sdi_globocan_8 |>
  dplyr::group_by(cancer_type) |>
  rstatix::games_howell_test(asr_world ~ sdi_categories) |>
  dplyr::mutate(
    y.position = case_when(
      cancer_type == "Brain, nervous system" ~ 9,
      cancer_type == "Leukaemia" ~ 13,
      cancer_type == "Breast" ~ 125,
      cancer_type == "Hodgkin lymphoma" ~ 3.5,
      TRUE ~ 110
    )
  ) |>
  dplyr::filter(
    (group1 == "Low" & group2 == "Low-middle") |
      (group1 == "Low-middle" & group2 == "Middle") |
      (group1 == "Middle" & group2 == "High-middle") |
      (group1 == "High-middle" & group2 == "High") |
      (group2 == "Low" & group1 == "Low-middle") |
      (group2 == "Low-middle" & group1 == "Middle") |
      (group2 == "Middle" & group1 == "High-middle") |
      (group2 == "High-middle" & group1 == "High")
  )

## Filter cancer mortality by SDI categories
data_mortality_sdi_globocan_8 <- globocan_8 |>
  dplyr::filter(
    indicator == "Mortality" &
      cancer_type %in% c(
        "Kidney",
        "Colorectum",
        "Brain, nervous system",
        "Pancreas") &
      !is.na(sdi_categories) & sdi_categories != "" & !is.na(asr_world)
  ) |>
  dplyr::mutate(sdi_categories = factor(sdi_categories) |>
                  fct_relevel("Low", "Medium", "High", "Very high"))

## Games-Howell test for cancer mortality and adjustment of 'y' positions
pw_gh_test_mortality_sdi <- data_mortality_sdi_globocan_8 |>
  dplyr::group_by(cancer_type) |>
  rstatix::games_howell_test(asr_world ~ sdi_categories) |>
  dplyr::mutate(
    y.position = case_when(
      cancer_type == "Kidney" ~ 5,
      cancer_type == "Colorectum" ~ 22,
      cancer_type == "Brain, nervous system" ~ 7,
      cancer_type == "Pancreas" ~ 10
    )
  ) |>
  dplyr::filter(
    (group1 == "Low" & group2 == "Low-middle") |
      (group1 == "Low-middle" & group2 == "Middle") |
      (group1 == "Middle" & group2 == "High-middle") |
      (group1 == "High-middle" & group2 == "High") |
      (group2 == "Low" & group1 == "Low-middle") |
      (group2 == "Low-middle" & group1 == "Middle") |
      (group2 == "Middle" & group1 == "High-middle") |
      (group2 == "High-middle" & group1 == "High")
  )

## Filter cancer MIR by SDI categories
data_mir_sdi_globocan_8 <- globocan_8 |>
  dplyr::filter(
    indicator == "MIR" &
      cancer_type %in% c(
        "Multiple myeloma",
        "Leukaemia",
        "Non-Hodgkin lymphoma",
        #"Kaposi sarcoma",
        "Cervix uteri",
        "Rectum",
        "Prostate",
        "All cancers*",
        "Breast",
        "Lip, oral cavity",
        "Colorectum"
      ) &
      !is.na(sdi_categories) & sdi_categories != "" & !is.na(asr_world)
  ) |>
  dplyr::mutate(sdi_categories = factor(sdi_categories) |>
                  fct_relevel("Low", "Medium", "High", "Very high"))

## Games-Howell test for cancer MIR and adjustment of 'y' positions
pw_gh_test_mir_sdi <- data_mir_sdi_globocan_8 |>
  dplyr::group_by(cancer_type) |>
  rstatix::games_howell_test(asr_world ~ sdi_categories) |>
  dplyr::mutate(
    y.position = case_when(
      cancer_type == "Prostate" ~ 0.70,
      cancer_type == "All cancers*" ~ 0.8,
      cancer_type == "Rectum" ~ 0.92,
      cancer_type == "Non-Hodgkin lymphoma" ~ 0.90,
      cancer_type == "Multiple myeloma" ~ 0.95,
      cancer_type == "Cervix uteri" ~ 0.9,
      cancer_type == "Breast" ~ 0.9,
      cancer_type == "Lip, oral cavity" ~ 0.95,
      cancer_type == "Leukaemia" ~ 0.9,
      cancer_type == "Colorectum" ~ 0.9
    )
  ) |>
  dplyr::filter(
    (group1 == "Low" & group2 == "Low-middle") |
      (group1 == "Low-middle" & group2 == "Middle") |
      (group1 == "Middle" & group2 == "High-middle") |
      (group1 == "High-middle" & group2 == "High") |
      (group2 == "Low" & group1 == "Low-middle") |
      (group2 == "Low-middle" & group1 == "Middle") |
      (group2 == "Middle" & group1 == "High-middle") |
      (group2 == "High-middle" & group1 == "High")
  )
