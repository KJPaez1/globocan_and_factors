# Import data

## Epidemiological indicators
globocan_mortality <-
  import(here("data", "raw", "globocan_mortality.csv")) |>
  janitor::clean_names() |>
  dplyr::filter(label != "Total") |>
  dplyr::select(-sex) |>
  dplyr::mutate(indicator = "mortality")
  

globocan_incidence <-
  import(here("data", "raw", "globocan_incidence.csv")) |>
  janitor::clean_names() |>
  dplyr::filter(label != "Total") |>
  dplyr::select(-sex)

common_columns <- c("alpha_3_code", "cancer_code", "label", "population_code_iso_un", "country")

# globocan <- globocan_incidence |> merge(globocan_mortality, by = common_columns, all.x = TRUE)

globocan <- globocan_incidence |>
  dplyr::left_join(globocan_mortality, by = common_columns) |>
  dplyr::mutate(MIR = round(asr_world.y/asr_world.x, 2), indicator = "MIR")

# globocan |> dplyr::select(asr_world.x, asr_world.y, MIR) |> summary() # Check NA's

# globocan |> dplyr::filter(if_all(starts_with("asr"), ~ !is.na(.))) # Filter NA's in indicators

## Socio-demographic indicators
hdi_and_comp_2022 <-
  import(here("data", "raw", "hdi_and_composites_2022.csv"))

hdi_and_comp_2017 <-
  import(here("data", "raw", "hdi_and_composites_2017.csv"))

sdi_2018_to_2021 <-
  import(here("data", "raw", "sdi_2018_to_2021.csv"))

gdb_sdi_2021_and_quintiles <-
  import(here("data", "raw", "ihme_gdb_sdi_2021_and_quintiles.csv"))

## Complementary data
regiones_mundo <- import(here("data", "raw", "regiones_mundo.txt")) |>
  janitor::clean_names()

cancer_code <- import(here("data", "raw", "cancer_code.csv")) |>
  janitor::clean_names()

# Function to calculate Education and Income (EdI) Index

## Define the function to calculate EdI
calculate_EdI = function(EYS,
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

# Calculation and categorization of Education and Income (EdI) Index

## Apply the calculate_EdI function to the data frame
hdi_and_comp_2022_0 <-
  hdi_and_comp_2022 |>
  dplyr::mutate(
    Education_and_Income =
      calculate_EdI(
        MYS = Mean_years_of_schooling_y,
        EYS = Expected_years_of_schooling_y,
        GNI_per_capita = GNI_per_capita_2017PPP_dolar
      ),
    edi_categories = case_when(
      Education_and_Income < .55 ~ "Low",
      Education_and_Income >= .55 & Education_and_Income < 69 ~ "Medium",
      Education_and_Income >= .69 & Education_and_Income < .81 ~ "High",
      Education_and_Income >= .81 ~ "Very high"
    )
  )

# Tidying data

## Epidemiological data
globocan_0 <- merge(globocan, cancer_code, by = "cancer_code", all.x = TRUE)

globocan_0 <- globocan_0 |>
  dplyr::rename(cancer_type = label.y, Country_name = label.x) |>
  dplyr::filter(Country_name != "Total")

countries_with_epid_data <- unique(globocan_0$Country_name)

# Exploring the number of countries with socioeconomic indicators

## SDI
sdi_2018_to_2021_0 <- sdi_2018_to_2021 |>
  dplyr::filter(sdi_2018_to_2021$Country %in% countries_with_epid_data)

gdb_sdi_2021_and_quintiles <- gdb_sdi_2021_and_quintiles |> 
  janitor::clean_names()

gdb_sdi_2021_and_quintiles_0 <- gdb_sdi_2021_and_quintiles |>
  dplyr::select(-location_id) |>
  dplyr::rename(
    Country_name = location_name,
    sdi_gdb_2021 = x2021_sdi_index_value,
    sdi_categories = sdi_quintile
  ) |> 
  dplyr::filter(gdb_sdi_2021_and_quintiles$location_name %in% countries_with_epid_data)

## HDI
hdi_and_comp_2017_0 <- hdi_and_comp_2017 |>
  dplyr::filter(hdi_and_comp_2017$Country %in% countries_with_epid_data)

hdi_and_comp_2022_0 <- hdi_and_comp_2022_0 |>
  dplyr::filter(hdi_and_comp_2022_0$Country %in% countries_with_epid_data) |>
  dplyr::rename(Country_name = Country)

# Countries without indicator data
dplyr::setdiff(countries_with_epid_data, gdb_sdi_2021_and_quintiles$Location.Name)

# Matching epidemiological data and indicators
sdi_2018_to_2021_0 <- sdi_2018_to_2021_0 |>
  dplyr::rename(Country_name = "Country")

globocan_1 = globocan_0 |>
  dplyr::left_join(hdi_and_comp_2022_0, by = "Country_name")

globocan_2 <- globocan_1 |>
  dplyr::left_join(sdi_2018_to_2021_0, by = "Country_name")

globocan_3 <- globocan_2 |>
  dplyr::left_join(gdb_sdi_2021_and_quintiles_0, by = "Country_name")

colnames(globocan_3) = gsub("\\_y$", "", colnames(globocan_3))
globocan_3 = janitor::clean_names(globocan_3)

# Classifying cancers
globocan_4 <- globocan_3 |>
  dplyr::mutate(
    neoplasia_sex = case_when(
      cancer_type %in% c(
        "Lip, oral cavity", "Salivary glands", "Oropharynx", "Nasopharynx",
        "Hypopharynx", "Oesophagus", "Stomach", "Colon", "Rectum", "Anus",
        "Liver and intrahepatic bile ducts", "Gallbladder", "Pancreas",
        "Larynx", "Trachea, bronchus and lung", "Melanoma of skin",
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
        "Gallbladder", "Oesophagus", "Anus", "Trachea, bronchus and lung",
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
        "Larynx", "Trachea, bronchus and lung") ~ "Respiratory and intrathoracic organs",
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
    ),
    asr_incidence_quartiles = case_when(
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator_x == "incidence" &
        asr_world_x <= 113.5 ~ "incidence_Q1",
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator_x == "incidence" &
        asr_world_x > 113.5 & asr_world_x <= 152.5 ~ "incidence_Q2",
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator_x == "incidence" &
        asr_world_x > 152.5 & asr_world_x <= 227.3 ~ "incidence_Q3",
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator_x == "incidence" &
        asr_world_x > 227.3 ~ "incidence_Q4"
    ),
    asr_mortality_quartiles = case_when(
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator_y == "mortality" &
        asr_world_y <= 74.90 ~ "mortality_Q1",
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator_y == "mortality" &
        asr_world_y > 74.90 & asr_world_y <= 89.50 ~ "mortality_Q2",
      cancer_type == "All cancers excl. non-melanoma skin cancer" & 
        indicator_y == "mortality" &
        asr_world_y > 89.50 & asr_world_y <= 103.20 ~ "mortality_Q3",
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator_y == "mortality" &
        asr_world_y > 103.20 ~ "mortality_Q4"
    ),
    mir_quartiles = case_when(
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator == "MIR" &
        mir <= 0.4600 ~ "MIR_Q1",
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator == "MIR" &
        mir > 0.4600 & mir <= 0.5700 ~ "MIR_Q2",
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator == "MIR" &
        mir > 0.5700 & mir <= 0.6900 ~ "MIR_Q3",
      cancer_type == "All cancers excl. non-melanoma skin cancer" &
        indicator == "MIR" &
        mir > 0.6900 ~ "MIR_Q4"
    ))

# Adding world regions
globocan_5 <- globocan_4 |> 
  dplyr::left_join(regiones_mundo, by = "country_name")

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
    crude_rate = coalesce(crude_rate_x, crude_rate_y),
    indicator = factor(indicator) |>
      forcats::fct_recode(
        "Incidence" = "asr_world_x", 
        "Mortality" = "asr_world_y",
        "MIR" = "mir"),
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
  dplyr::rename(
    `ASR (World)` = asr_world,
    `Crude rate` = crude_rate,
    HDI = human_development_index,
    EdI = education_and_income,
    SDI = sdi_gdb_2021,
    Indicator = indicator)

# globocan_6 |>
#   dplyr::select(country_name, SDI) |>
#   dplyr::filter(SDI > 0.9 & SDI <=9.27) |>
#   dplyr::arrange(SDI) |>
#   dplyr::distinct(country_name)
# globocan_6 |> dplyr::distinct(country_sdi)

# Complex names data
globocan_7 <- globocan_6 |>
  dplyr::distinct(cancer_type, country_name, Indicator, .keep_all = TRUE)

# Clean names data
globocan_8 <- globocan_7 |>
  dplyr::mutate(
    cancer_type = case_when(
      cancer_type == " Brain, central nervous system" ~ "Brain, nervous system",
      cancer_type == "Liver and intrahepatic bile ducts" ~ "Liver",
      .default = as.character(cancer_type))
  ) |>
  janitor::clean_names()

globocan_9 <- globocan_8 |>
  dplyr::filter(indicator != "MIR", cancer_type == "All cancers excl. non-melanoma skin cancer")

# # Figure 1
# figure_1 <- ggplot2::ggplot(
#   data = globocan_9, aes(
#     x = ed_i,
#     y = asr_world,
#     color = indicator,
#     shape = indicator
#     )
#   ) +
#   ggplot2::geom_point(aes(fill = indicator), size = 2.5) +
#   ggplot2::geom_smooth(method = "gam",
#                        formula = y ~ s(x, bs = "cr", k = -1),
#                        se = FALSE) +
#   ggplot2::geom_vline(
#     xintercept = quantile(globocan_9$ed_i, probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
#     linetype = "dashed",
#     color = "gray70",
#     linewidth = 1
#   ) +
#   ggrepel::geom_label_repel(aes(label = country_label), show.legend = FALSE) +
#   ggplot2::scale_shape_manual(values = c(21, 24)) +
#   ggsci::scale_color_igv() +
#   ggsci::scale_fill_igv(alpha = 0.1) +
#   ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
#   ggplot2::labs(x = "Education and Income index (EdI)", 
#                 y = "Age-Standardized Rate (per 100,000)") +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(
#     strip.text.x = element_text(hjust = 0),
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     text = element_text(size = 18, color = "black", family = "Syne"),
#     axis.line = element_line(colour = "black", linetype = "solid"),
#     axis.ticks = element_line(colour = "black", linetype = "solid"),
#     panel.grid = element_blank()
#   )
# 
# # Save figure 1 (JPEG)
# ggsave(
#   plot = figure_1,
#   filename = here("outputs", "FIG_1.jpeg"),
#   width = 12,
#   height = 8,
#   dpi = 500,
#   units = "in")
# 
# # Figure 2
# figure_2 <- globocan_8 |>
#   dplyr::filter(indicator == "MIR", cancer_type == "All cancers excl. non-melanoma skin cancer") |>
#   ggplot2::ggplot(aes(x = ed_i, y = asr_world)) +
#   ggplot2::geom_point(size = 2.5, shape = 1) +
#   ggplot2::geom_smooth(method = "gam",
#                        formula = y ~ s(x, bs = "cr", k = -1),
#                        se = FALSE, color = "salmon") +
#   ggplot2::geom_vline(
#     xintercept = quantile(globocan_9$ed_i, probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
#     linetype = "dashed",
#     color = "gray70",
#     linewidth = 1
#   ) +
#   ggrepel::geom_label_repel(aes(label = country_label), show.legend = FALSE) +
#   ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
#   ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
#   ggplot2::labs(x = "Education and Income index (EdI)", 
#                 y = "Mortality-to-Incidence Ratio") +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(
#     strip.text.x = element_text(hjust = 0),
#     text = element_text(size = 18, color = "black", family = "Syne"),
#     axis.line = element_line(colour = "black", linetype = "solid"),
#     axis.ticks = element_line(colour = "black", linetype = "solid"),
#     panel.grid = element_blank()
#   )
# 
# # Save figure 2 (JPEG)
# ggsave(
#   plot = figure_2,
#   filename = here("outputs", "FIG_2.jpeg"),
#   width = 12,
#   height = 8,
#   dpi = 500,
#   units = "in")
# 
# # My ggplot function for figure 2
# my_ggplot <- function(data, ...) {
#   ggplot2::ggplot(
#     data = data,
#     aes(
#       x = ed_i,
#       y = asr_world,
#       color = indicator,
#       shape = indicator
#       )
#     ) +
#     ggplot2::geom_point(aes(fill = indicator), size = 2.5) +
#     ggplot2::geom_smooth(method = "gam",
#                          formula = y ~ s(x, bs = "cr", k = -1),
#                          se = FALSE) +
#     ggplot2::geom_vline(
#       xintercept = quantile(data$ed_i, probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
#       linetype = "dashed",
#       color = "gray70",
#       linewidth = 1
#     ) +
#     ggplot2::scale_shape_manual(values = c(21, 24)) +
#     ggsci::scale_color_igv(guide = guide_legend(override.aes = list(
#       linetype = c(0, 0),
#       shape = c(1, 1),
#       size = c(4, 4)
#     ))) +
#     ggsci::scale_fill_igv(alpha = 0.1) +
#     ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.15)) +
#     ggplot2::guides(fill = "none") +
#     ggplot2::theme_minimal() +
#     ggplot2::facet_wrap(
#       vars(cancer_type),
#       labeller = labeller(cancer_type = label_wrap_gen(50)),
#       ncol = 3,
#       scales = "free"
#     ) +
#     ggplot2::theme(
#       strip.text.x = element_text(hjust = 0),
#       legend.position = "bottom",
#       legend.title = element_blank(),
#       text = element_text(size = 22, color = "black", family = "Syne"),
#       axis.line = element_line(colour = "black", linetype = "solid"),
#       axis.ticks = element_line(colour = "black", linetype = "solid"),
#       panel.grid = element_blank(),
#       plot.title = element_text(size = 24)
#     )
# }
# 
# # Figure 2A
# figure_2A_data <- globocan_8 |>
#   dplyr::filter(
#     cancer_type %in% c(
#       "Breast",
#       "Prostate",
#       "Colorectum",
#       "Melanoma of skin",
#       "Corpus uteri",
#       "Bladder"
#     )
#   ) |>
#   dplyr::mutate(cancer_type = factor(
#     cancer_type,
#     levels = c(
#       "Breast",
#       "Prostate",
#       "Colorectum",
#       "Melanoma of skin",
#       "Corpus uteri",
#       "Bladder"
#     )
#   ))
# 
# figure_2A <- my_ggplot(figure_2A_data) +
#   ggplot2::labs(title = "Group A") + 
#   ggplot2::theme(axis.title = element_blank())
# 
# # Figure 2B
# figure_2B_data <- globocan_8 |>
#   dplyr::filter(
#     cancer_type %in% c(
#       "Trachea, bronchus and lung", 
#       "Pancreas",
#       "Brain, nervous system"
#     )
#   ) |>
#   dplyr::mutate(cancer_type = factor(
#     cancer_type,
#     levels = c(
#       "Trachea, bronchus and lung", 
#       "Pancreas",
#       "Brain, nervous system"
#     )
#   ))
# 
# figure_2B <- my_ggplot(figure_2B_data) +
#   ggplot2::labs(y = "Age-Standardized Rate (per 100,000)", title = "Group B") +
#   ggplot2::theme(axis.title.y = element_text(hjust = 0), axis.title.x = element_blank())
# 
# # Figure 2C
# figure_2C_data <- globocan_8 |>
#   dplyr::filter(
#     cancer_type %in% c(
#       "Cervix uteri",
#       "Stomach",
#       "Liver"
#     )
#   ) |>
#   dplyr::mutate(cancer_type = factor(
#     cancer_type,
#     levels = c(
#       "Cervix uteri",
#       "Stomach",
#       "Liver"
#     )
#   ))
# 
# figure_2C <- my_ggplot(figure_2C_data) +
#   ggplot2::labs(x = "Education and Income index (EdI)", title = "Group C") + 
#   ggplot2::theme(axis.title.y = element_blank())
# 
# # Arrange on one page
# figure_2 <-
#   figure_2A + (figure_2B + figure_2C + plot_layout(ncol = 1)) +
#   plot_layout(ncol = 1,
#               guides = 'collect',
#               axis_titles = 'collect') &
#   theme(legend.position = 'bottom')
# 
# # Save figure 2 (PNG)
# ggsave(
#   plot = figure_2,
#   filename = here("outputs", "FIG_2.png"),
#   width = 20,
#   height = 15,
#   dpi = 500,
#   units = "in")
# 
# # 
# globocan_10 <- globocan_8 |>
#   dplyr::filter(
#     !cancer_type %in% c(
#       "All cancers excl. non-melanoma skin cancer",
#       "Colon",
#       "Rectum",
#       "Non-melanoma skin cancer")
#   ) |>
#   dplyr::mutate(
#     cancer_type = case_when(
#       cancer_type %in% c(
#         "Gallbladder",
#         "Salivary glands",
#         "Mesothelioma",
#         "Penis",
#         "Kaposi sarcoma") ~ "Others/Unspec",
#       cancer_type %in% c(
#         "Hypopharynx",
#         "Nasopharynx",
#         "Oropharynx") ~ "Other pharynx", 
#       .default = as.character(cancer_type)
#     )
#   ) |> 
#   dplyr::mutate(cancer_type = factor(
#     cancer_type,
#     levels = c(
#       "Breast",
#       "Prostate",
#       "Colorectum",
#       "Melanoma of skin",
#       "Corpus uteri",
#       "Bladder",
#       "Non-Hodgkin lymphoma",
#       "Kidney",
#       "Thyroid",
#       "Leukaemia",
#       "Ovary",
#       "Testis",
#       "Multiple myeloma",
#       "Hodgkin lymphoma",
#       "Vulva",
#       "Anus",
#       "Trachea, bronchus and lung", 
#       "Pancreas",
#       "Brain, nervous system",
#       "Cervix uteri",
#       "Stomach",
#       "Liver",
#       "Larynx",
#       "Vagina",
#       "Lip, oral cavity",
#       "Oesophagus",
#       "Other pharynx",
#       "Others/Unspec"
#     )
#   ),
#   group = case_when(
#     cancer_type %in% c(
#       "Breast",
#       "Prostate",
#       "Colorectum",
#       "Melanoma of skin",
#       "Corpus uteri",
#       "Bladder",
#       "Non-Hodgkin lymphoma",
#       "Kidney",
#       "Thyroid",
#       "Leukaemia",
#       "Ovary",
#       "Testis",
#       "Multiple myeloma",
#       "Hodgkin lymphoma",
#       "Vulva",
#       "Anus") ~ "Group A",
#     cancer_type %in% c(
#       "Trachea, bronchus and lung", 
#       "Pancreas",
#       "Brain, nervous system") ~ "Group B",
#     cancer_type %in% c(
#       "Cervix uteri",
#       "Stomach",
#       "Liver",
#       "Larynx",
#       "Vagina") ~ "Group C",
#     cancer_type %in% c(
#       "Lip, oral cavity",
#       "Oesophagus",
#       "Other pharynx",
#       "Others/Unspec") ~ "Group D",
#     .default = as.character(cancer_type)
#   ))
# 
# # Define the number of unique cancer types
# colourCount <- length(unique(globocan_10$cancer_type))
# 
# # Define the four palettes
# palette1 <- colorRampPalette(brewer.pal(4, "Reds"))(16)
# palette2 <- colorRampPalette(brewer.pal(3, "Purples"))(3)
# palette3 <- colorRampPalette(brewer.pal(6, "Greens"))(5)
# palette4 <- colorRampPalette(brewer.pal(3, "Blues"))(4)
# palette5 <- colorRampPalette(brewer.pal(3, "Greys"))(1)
# 
# # Combine the palettes
# combinedPalette <- c(palette1, palette2, palette3, palette4, palette5)
# 
# # Figure 3
# figure_3 <- ggplot2::ggplot(
#   data = globocan_10, aes(
#     x = ed_i,
#     y = asr_world,
#     fill = cancer_type
#     )
#   ) + 
#   ggplot2::geom_area(color = "gray80", size = 0.25) +
#   ggplot2::scale_fill_manual(values = combinedPalette) +
#   ggplot2::geom_vline(
#     xintercept = quantile(
#       globocan_10$ed_i,
#       probs = c(0.25, 0.5, 0.75),
#       na.rm = TRUE
#     ),
#     linetype = "dashed",
#     color = "gray80",
#     linewidth = 0.5
#   ) +
#   ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.15)) +
#   ggplot2::labs(
#     x = "Education and Income index (EdI)",
#     y = "Age-Standardized Rate (per 100,000)",
#     fill = "Cancer type") +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(
#     plot.margin = unit(c(1, 1, 1, 1), "cm"), 
#     legend.position = "right",
#     text = element_text(size = 14, color = "black", family = "Syne"),
#     axis.ticks = element_line(colour = "black", linetype = "solid"),
#     panel.grid = element_blank()
#   ) +
#   ggplot2::guides(fill = guide_legend(ncol = 1)) +
#   ggplot2::facet_wrap(
#     vars(indicator),
#     ncol = 1,
#     scales = "free",
#     strip.position = "left"
#   ) +
#   ggplot2::theme(strip.placement = "outside")
# 
# # Save figure 3 (JPEG)
# ggsave(
#   plot = figure_3,
#   filename = here("outputs", "FIG_3.jpeg"),
#   width = 14,
#   height = 8,
#   dpi = 500,
#   units = "in")
# 
# # ggplot2::stat_smooth(
# #   geom = "area",
# #   method = 'gam',
# #   formula = y ~ s(x, bs = "cr", k = -1),
# #   se = FALSE,
# #   color = "gray80"
# # )
# 
# # Figure S1
# figure_S1 <- ggplot2::ggplot(
#   data = globocan_9, aes(
#     x = hdi,
#     y = asr_world,
#     color = indicator
#   )
# ) +
#   ggplot2::geom_point(aes(fill = indicator), size = 2.5, shape = 21) +
#   ggplot2::geom_smooth(method = "gam",
#                        formula = y ~ s(x, bs = "cr", k = -1),
#                        se = FALSE) +
#   ggplot2::geom_vline(
#     xintercept = quantile(globocan_9$hdi, probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
#     linetype = "dashed",
#     color = "gray70",
#     linewidth = 1
#   ) +
#   ggrepel::geom_label_repel(aes(label = country_label), show.legend = FALSE) +
#   ggsci::scale_color_igv() +
#   ggsci::scale_fill_igv(alpha = 0.1) +
#   ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.15)) +
#   ggplot2::guides(fill = "none") +
#   ggplot2::labs(x = "Human Development Index (HDI)", y = "Age-Standardized Rate (per 100,000)") +
#   ggplot2::theme_minimal() +
#   ggplot2::facet_wrap(
#     vars(cancer_type),
#     labeller = labeller(cancer_type = label_wrap_gen(50)),
#     ncol = 3,
#     scales = "free"
#   ) +
#   ggplot2::theme(
#     strip.text.x = element_text(hjust = 0),
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     text = element_text(size = 18, color = "black", family = "Syne"),
#     axis.line = element_line(colour = "black", linetype = "solid"),
#     axis.ticks = element_line(colour = "black", linetype = "solid"),
#     panel.grid = element_blank()
#   )
# 
# # Save figure S1 (JPEG)
# ggsave(
#   plot = figure_S1,
#   filename = here("outputs", "FIG_S1.jpeg"),
#   width = 12,
#   height = 8,
#   dpi = 500,
#   units = "in")
# 
# # My ggplot function for figure S2
# my_ggplot <- function(data, ...) {
#   ggplot2::ggplot(
#     data = data,
#     aes(
#       x = hdi,
#       y = asr_world,
#       color = indicator,
#       shape = indicator
#     )
#   ) +
#     ggplot2::geom_point(aes(fill = indicator), size = 2.5) +
#     ggplot2::geom_smooth(method = "gam",
#                          formula = y ~ s(x, bs = "cr", k = -1),
#                          se = FALSE) +
#     ggplot2::geom_vline(
#       xintercept = quantile(data$hdi, probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
#       linetype = "dashed",
#       color = "gray70",
#       linewidth = 1
#     ) +
#     ggplot2::scale_shape_manual(values = c(21, 24)) +
#     ggsci::scale_color_igv(guide = guide_legend(override.aes = list(
#       linetype = c(0, 0),
#       shape = c(1, 1),
#       size = c(4, 4)
#     ))) +
#     ggsci::scale_fill_igv(alpha = 0.1) +
#     ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.15)) +
#     ggplot2::guides(fill = "none") +
#     ggplot2::theme_minimal() +
#     ggplot2::facet_wrap(
#       vars(cancer_type),
#       labeller = labeller(cancer_type = label_wrap_gen(50)),
#       ncol = 3,
#       scales = "free"
#     ) +
#     ggplot2::theme(
#       strip.text.x = element_text(hjust = 0),
#       legend.position = "bottom",
#       legend.title = element_blank(),
#       text = element_text(size = 22, color = "black", family = "Syne"),
#       axis.line = element_line(colour = "black", linetype = "solid"),
#       axis.ticks = element_line(colour = "black", linetype = "solid"),
#       panel.grid = element_blank(),
#       plot.title = element_text(size = 24)
#     )
# }
# 
# # Figure S2A
# figure_S2A_data <- globocan_8 |>
#   dplyr::filter(
#     cancer_type %in% c(
#       "Breast",
#       "Prostate",
#       "Colorectum",
#       "Melanoma of skin",
#       "Corpus uteri",
#       "Bladder"
#     )
#   ) |>
#   dplyr::mutate(cancer_type = factor(
#     cancer_type,
#     levels = c(
#       "Breast",
#       "Prostate",
#       "Colorectum",
#       "Melanoma of skin",
#       "Corpus uteri",
#       "Bladder"
#     )
#   ))
# 
# figure_S2A <- my_ggplot(figure_S2A_data) +
#   ggplot2::labs(title = "Group A") + 
#   ggplot2::theme(axis.title = element_blank())
# 
# # Figure S2B
# figure_S2B_data <- globocan_8 |>
#   dplyr::filter(
#     cancer_type %in% c(
#       "Trachea, bronchus and lung", 
#       "Pancreas",
#       "Brain, nervous system"
#     )
#   ) |>
#   dplyr::mutate(cancer_type = factor(
#     cancer_type,
#     levels = c(
#       "Trachea, bronchus and lung", 
#       "Pancreas",
#       "Brain, nervous system"
#     )
#   ))
# 
# figure_S2B <- my_ggplot(figure_S2B_data) +
#   ggplot2::labs(y = "Age-Standardized Rate (per 100,000)", title = "Group B") +
#   ggplot2::theme(axis.title.y = element_text(hjust = 0), axis.title.x = element_blank())
# 
# # Figure S2C
# figure_S2C_data <- globocan_8 |>
#   dplyr::filter(
#     cancer_type %in% c(
#       "Cervix uteri",
#       "Stomach",
#       "Liver"
#     )
#   ) |>
#   dplyr::mutate(cancer_type = factor(
#     cancer_type,
#     levels = c(
#       "Cervix uteri",
#       "Stomach",
#       "Liver"
#     )
#   ))
# 
# figure_S2C <- my_ggplot(figure_S2C_data) +
#   ggplot2::labs(x = "Human Development Index (HDI)", title = "Group C") + 
#   ggplot2::theme(axis.title.y = element_blank())
# 
# # Arrange on one page
# figure_S2 <-
#   figure_S2A + (figure_S2B + figure_S2C + plot_layout(ncol = 1)) +
#   plot_layout(ncol = 1,
#               guides = 'collect',
#               axis_titles = 'collect') &
#   theme(legend.position = 'bottom')
# 
# # Save figure S2 (PNG)
# ggsave(
#   plot = figure_S2,
#   filename = here("outputs", "FIG_S2.png"),
#   width = 20,
#   height = 15,
#   dpi = 500,
#   units = "in")
# 
# # Figure S3
# figure_S3 <- ggplot2::ggplot(
#   data = globocan_9, aes(
#     x = sdi,
#     y = asr_world,
#     color = indicator
#   )
# ) +
#   ggplot2::geom_point(aes(fill = indicator), size = 2.5, shape = 21) +
#   ggplot2::geom_smooth(method = "gam",
#                        formula = y ~ s(x, bs = "cr", k = -1),
#                        se = FALSE) +
#   ggplot2::geom_vline(
#     xintercept = quantile(globocan_9$sdi, probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
#     linetype = "dashed",
#     color = "gray70",
#     linewidth = 1
#   ) +
#   ggrepel::geom_label_repel(aes(label = country_label), show.legend = FALSE) +
#   ggsci::scale_color_igv() +
#   ggsci::scale_fill_igv(alpha = 0.1) +
#   ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.15)) +
#   ggplot2::guides(fill = "none") +
#   ggplot2::labs(x = "Socio-demographic Index (SDI)", y = "Age-Standardized Rate (per 100,000)") +
#   ggplot2::theme_minimal() +
#   ggplot2::facet_wrap(
#     vars(cancer_type),
#     labeller = labeller(cancer_type = label_wrap_gen(50)),
#     ncol = 3,
#     scales = "free"
#   ) +
#   ggplot2::theme(
#     strip.text.x = element_text(hjust = 0),
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     text = element_text(size = 18, color = "black", family = "Syne"),
#     axis.line = element_line(colour = "black", linetype = "solid"),
#     axis.ticks = element_line(colour = "black", linetype = "solid"),
#     panel.grid = element_blank()
#   )
# 
# # Save figure S3 (JPEG)
# ggsave(
#   plot = figure_S3,
#   filename = here("outputs", "FIG_S3.jpeg"),
#   width = 12,
#   height = 8,
#   dpi = 500,
#   units = "in")
# 
# # My ggplot function for figure S4
# my_ggplot <- function(data, ...) {
#   ggplot2::ggplot(
#     data = data,
#     aes(
#       x = sdi,
#       y = asr_world,
#       color = indicator,
#       shape = indicator
#     )
#   ) +
#     ggplot2::geom_point(aes(fill = indicator), size = 2.5) +
#     ggplot2::geom_smooth(method = "gam",
#                          formula = y ~ s(x, bs = "cr", k = -1),
#                          se = FALSE) +
#     ggplot2::geom_vline(
#       xintercept = quantile(data$sdi, probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
#       linetype = "dashed",
#       color = "gray70",
#       linewidth = 1
#     ) +
#     ggplot2::scale_shape_manual(values = c(21, 24)) +
#     ggsci::scale_color_igv(guide = guide_legend(override.aes = list(
#       linetype = c(0, 0),
#       shape = c(1, 1),
#       size = c(4, 4)
#     ))) +
#     ggsci::scale_fill_igv(alpha = 0.1) +
#     ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.15)) +
#     ggplot2::guides(fill = "none") +
#     ggplot2::theme_minimal() +
#     ggplot2::facet_wrap(
#       vars(cancer_type),
#       labeller = labeller(cancer_type = label_wrap_gen(50)),
#       ncol = 3,
#       scales = "free"
#     ) +
#     ggplot2::theme(
#       strip.text.x = element_text(hjust = 0),
#       legend.position = "bottom",
#       legend.title = element_blank(),
#       text = element_text(size = 22, color = "black", family = "Syne"),
#       axis.line = element_line(colour = "black", linetype = "solid"),
#       axis.ticks = element_line(colour = "black", linetype = "solid"),
#       panel.grid = element_blank(),
#       plot.title = element_text(size = 24)
#     )
# }
# 
# # Figure S4A
# figure_S4A_data <- globocan_8 |>
#   dplyr::filter(
#     cancer_type %in% c(
#       "Breast",
#       "Prostate",
#       "Colorectum",
#       "Melanoma of skin",
#       "Corpus uteri",
#       "Bladder"
#     )
#   ) |>
#   dplyr::mutate(cancer_type = factor(
#     cancer_type,
#     levels = c(
#       "Breast",
#       "Prostate",
#       "Colorectum",
#       "Melanoma of skin",
#       "Corpus uteri",
#       "Bladder"
#     )
#   ))
# 
# figure_S4A <- my_ggplot(figure_S4A_data) +
#   ggplot2::labs(title = "Group A") + 
#   ggplot2::theme(axis.title = element_blank())
# 
# # Figure S4B
# figure_S4B_data <- globocan_8 |>
#   dplyr::filter(
#     cancer_type %in% c(
#       "Trachea, bronchus and lung", 
#       "Pancreas",
#       "Brain, nervous system"
#     )
#   ) |>
#   dplyr::mutate(cancer_type = factor(
#     cancer_type,
#     levels = c(
#       "Trachea, bronchus and lung", 
#       "Pancreas",
#       "Brain, nervous system"
#     )
#   ))
# 
# figure_S4B <- my_ggplot(figure_S4B_data) +
#   ggplot2::labs(y = "Age-Standardized Rate (per 100,000)", title = "Group B") +
#   ggplot2::theme(axis.title.y = element_text(hjust = 0), axis.title.x = element_blank())
# 
# # Figure S4C
# figure_S4C_data <- globocan_8 |>
#   dplyr::filter(
#     cancer_type %in% c(
#       "Cervix uteri",
#       "Stomach",
#       "Liver"
#     )
#   ) |>
#   dplyr::mutate(cancer_type = factor(
#     cancer_type,
#     levels = c(
#       "Cervix uteri",
#       "Stomach",
#       "Liver"
#     )
#   ))
# 
# figure_S4C <- my_ggplot(figure_S4C_data) +
#   ggplot2::labs(x = "Socio-demographic Index (SDI)", title = "Group C") + 
#   ggplot2::theme(axis.title.y = element_blank())
# 
# # Arrange on one page
# figure_S4 <-
#   figure_S4A + (figure_S4B + figure_S4C + plot_layout(ncol = 1)) +
#   plot_layout(ncol = 1,
#               guides = 'collect',
#               axis_titles = 'collect') &
#   theme(legend.position = 'bottom')
# 
# # Save figure S4 (PNG)
# ggsave(
#   plot = figure_S4,
#   filename = here("outputs", "FIG_S4.png"),
#   width = 20,
#   height = 15,
#   dpi = 500,
#   units = "in")