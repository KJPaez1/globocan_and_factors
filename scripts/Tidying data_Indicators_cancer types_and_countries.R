# Import data

## Epidemiological indicators
globocan_mortality <-
  import(here("data", "raw", "globocan_mortality.csv"))

globocan_incidence <-
  import(here("data", "raw", "globocan_incidence.csv"))

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
regiones_mundo <- import(here("data", "raw", "regiones_mundo.txt"))

cancer_code <- import(here("data", "raw", "cancer_code.csv"))

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
      Education_and_Income < .53 ~ "Low",
      Education_and_Income >= .53 & Education_and_Income < .70 ~ "Medium",
      Education_and_Income >= .70 & Education_and_Income < .80 ~ "High",
      Education_and_Income >= .80 ~ "Very high"
    )
  )

# Tidying data

## Epidemiological data
globocan <- janitor::clean_names(rbind(globocan_incidence, globocan_mortality))
cancer_code <- janitor::clean_names(cancer_code)

globocan_0 <- merge(globocan, cancer_code, by = "cancer_code", all.x = TRUE)

globocan_0 <- globocan_0 |>
  dplyr::rename(cancer_type = "label.y", Country_name = "label.x") |>
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
    Country_name = "location_name",
    sdi_gdb_2021 = "x2021_sdi_index_value",
    sdi_categories = "sdi_quintile"
  ) |>
  dplyr::filter(gdb_sdi_2021_and_quintiles$location_name %in% countries_with_epid_data)

## HDI
hdi_and_comp_2017_0 <- hdi_and_comp_2017 |>
  dplyr::filter(hdi_and_comp_2017$Country %in% countries_with_epid_data)

hdi_and_comp_2022_0 <- hdi_and_comp_2022_0 |>
  dplyr::filter(hdi_and_comp_2022_0$Country %in% countries_with_epid_data) |>
  dplyr::rename(Country_name = "Country")

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
      cancer_type == "Lip, oral cavity" ~ "Both",
      cancer_type == "Salivary glands" ~ "Both",
      cancer_type == "Oropharynx" ~ "Both",
      cancer_type == "Nasopharynx" ~ "Both",
      cancer_type == "Hypopharynx" ~ "Both",
      cancer_type == "Oesophagus" ~ "Both",
      cancer_type == "Stomach" ~ "Both",
      cancer_type == "Colon" ~ "Both",
      cancer_type == "Rectum" ~ "Both",
      cancer_type == "Anus" ~ "Both",
      cancer_type == "Liver and intrahepatic bile ducts" ~ "Both",
      cancer_type == "Gallbladder" ~ "Both",
      cancer_type == "Pancreas" ~ "Both",
      cancer_type == "Larynx" ~ "Both",
      cancer_type == "Trachea, bronchus and lung" ~ "Both",
      cancer_type == "Melanoma of skin" ~ "Both",
      cancer_type == "Non-melanoma skin cancer" ~ "Both",
      cancer_type == "Mesothelioma" ~ "Both",
      cancer_type == "Kaposi sarcoma" ~ "Both",
      cancer_type == "Breast" ~ "Both",
      cancer_type == "Vulva" ~ "Female",
      cancer_type == "Vagina" ~ "Female",
      cancer_type == "Cervix uteri" ~ "Female",
      cancer_type == "Ovary" ~ "Female",
      cancer_type == "Corpus uteri" ~ "Female",
      cancer_type == "Penis" ~ "Male",
      cancer_type == "Prostate" ~ "Male",
      cancer_type == "Testis" ~ "Male",
      cancer_type == "Kidney" ~ "Both",
      cancer_type == "Bladder" ~ "Both",
      cancer_type == "Thyroid" ~ "Both",
      cancer_type == "Hodgkin lymphoma" ~ "Both",
      cancer_type == "Non-Hodgkin lymphoma" ~ "Both",
      cancer_type == "Multiple myeloma" ~ "Both",
      cancer_type == "Leukaemia" ~ "Both",
      cancer_type == "All cancers excl. non-melanoma skin cancer" ~ "Both",
      cancer_type == "Colorectum" ~ "Both",
      cancer_type == " Brain, central nervous system" ~ "Both"
    ),
    tumor_types = case_when(
      cancer_type == "Lip, oral cavity" ~ "Squamous cell carcinoma",
      cancer_type == "Salivary glands" ~ "Adenocarcinoma",
      cancer_type == "Oropharynx" ~ "Squamous cell carcinoma",
      cancer_type == "Nasopharynx" ~ "Squamous cell carcinoma",
      cancer_type == "Hypopharynx" ~ "Squamous cell carcinoma",
      cancer_type == "Oesophagus" ~ "Squamous cell/Adenocarcinoma",
      cancer_type == "Stomach" ~ "Adenocarcinoma",
      cancer_type == "Colon" ~ "Adenocarcinoma",
      cancer_type == "Colorectum" ~ "Adenocarcinoma",
      cancer_type == "Rectum" ~ "Adenocarcinoma",
      cancer_type == "Anus" ~ "Squamous cell/Adenocarcinoma",
      cancer_type == "Liver and intrahepatic bile ducts" ~
        "Hepatocellular/Adenocarcinoma",
      cancer_type == "Gallbladder" ~ "Squamous cell/Adenocarcinoma",
      cancer_type == "Pancreas" ~ "Adenocarcinoma",
      cancer_type == "Larynx" ~ "Squamous cell carcinoma",
      cancer_type == "Trachea, bronchus and lung" ~ "Squamous cell/Adenocarcinoma",
      cancer_type == "Melanoma of skin" ~ "Melanoma",
      cancer_type == "Non-melanoma skin cancer" ~ "Squamous/basal cell carcinoma",
      cancer_type == "Mesothelioma" ~ "Mesothelioma",
      cancer_type == "Kaposi sarcoma" ~ "Sarcoma",
      cancer_type == "Breast" ~ "Adenocarcinoma",
      cancer_type == "Vulva" ~ "Squamous cell carcinoma",
      cancer_type == "Vagina" ~ "Squamous cell carcinoma",
      cancer_type == "Cervix uteri" ~ "Squamous cell/Adenocarcinoma",
      cancer_type == "Ovary" ~ "Adenocarcinoma",
      cancer_type == "Corpus uteri" ~ "Adenocarcinoma",
      cancer_type == "Penis" ~ "Squamous cell carcinoma",
      cancer_type == "Prostate" ~ "Adenocarcinoma",
      cancer_type == "Testis" ~ "Adenocarcinoma",
      cancer_type == "Kidney" ~ "Adenocarcinoma",
      cancer_type == "Bladder" ~ "Urothelial carcinoma",
      cancer_type == "Thyroid" ~ "Adenocarcinoma",
      cancer_type == "Hodgkin lymphoma" ~ "Lymphoma",
      cancer_type == "Non-Hodgkin lymphoma" ~ "Lymphoma",
      cancer_type == "Multiple myeloma" ~ "Myeloma",
      cancer_type == "Leukaemia" ~ "Leukaemia",
      cancer_type == "All cancers excl. non-melanoma skin cancer" ~ "Others",
      cancer_type == " Brain, central nervous system" ~ "Brain and spinal cord cancers"
    ),
    malignant_neoplasms_types = case_when(
      cancer_type == "Lip, oral cavity" ~ "Lip, oral cavity and pharynx",
      cancer_type == "Salivary glands" ~ "Lip, oral cavity and pharynx",
      cancer_type == "Oropharynx" ~ "Lip, oral cavity and pharynx",
      cancer_type == "Nasopharynx" ~ "Lip, oral cavity and pharynx",
      cancer_type == "Hypopharynx" ~ "Lip, oral cavity and pharynx",
      cancer_type == "Oesophagus" ~ "Digestive organs",
      cancer_type == "Stomach" ~ "Digestive organs",
      cancer_type == "Colon" ~ "Digestive organs",
      cancer_type == "Colorectum" ~ "Digestive organs",
      cancer_type == "Rectum" ~ "Digestive organs",
      cancer_type == "Anus" ~ "Digestive organs",
      cancer_type == "Liver and intrahepatic bile ducts" ~ "Digestive organs",
      cancer_type == "Gallbladder" ~ "Digestive organs",
      cancer_type == "Pancreas" ~ "Digestive organs",
      cancer_type == "Larynx" ~ "Respiratory and intrathoracic organs",
      cancer_type == "Trachea, bronchus and lung" ~ "Respiratory and intrathoracic organs",
      cancer_type == "Melanoma of skin" ~ "Melanoma and other malignant neoplasms of skin",
      cancer_type == "Non-melanoma skin cancer" ~ "Melanoma and other malignant neoplasms of skin",
      cancer_type == "Mesothelioma" ~ "Mesothelial and soft tissue",
      cancer_type == "Kaposi sarcoma" ~ "Mesothelial and soft tissue",
      cancer_type == "Breast" ~ "Breast",
      cancer_type == "Vulva" ~ "Female genital organs",
      cancer_type == "Vagina" ~ "Female genital organs",
      cancer_type == "Cervix uteri" ~ "Female genital organs",
      cancer_type == "Ovary" ~ "Female genital organs",
      cancer_type == "Corpus uteri" ~ "Female genital organs",
      cancer_type == "Penis" ~ "Male genital organs",
      cancer_type == "Prostate" ~ "Male genital organs",
      cancer_type == "Testis" ~ "Male genital organs",
      cancer_type == "Kidney" ~ "Urinary tract",
      cancer_type == "Bladder" ~ "Urinary tract",
      cancer_type == "Thyroid" ~ "Thyroid and other endocrine glands",
      cancer_type == "Hodgkin lymphoma" ~ "Lymphoid, haematopoietic and related tissue",
      cancer_type == "Non-Hodgkin lymphoma" ~ "Lymphoid, haematopoietic and related tissue",
      cancer_type == "Multiple myeloma" ~ "Lymphoid, haematopoietic and related tissue",
      cancer_type == "Leukaemia" ~ "Lymphoid, haematopoietic and related tissue",
      cancer_type == "All cancers excl. non-melanoma skin cancer" ~ "Other of skin and independent multiple sites",
      cancer_type == " Brain, central nervous system" ~ "Eye, brain and other parts of central nervous system"
    ),
    number_quartiles = case_when(
      indicator == "incidence" & number <= 31.00 ~ "incidence_Q1",
      indicator == "incidence" &
        number > 31.00 & number <= 195.00 ~ "incidence_Q2",
      indicator == "incidence" &
        number > 195.00 & number <= 1088.25 ~ "incidence_Q3",
      indicator == "incidence" &
        number > 1088.25 & number <= 4775419.00 ~ "incidence_Q4",
      indicator == "mortality" & number <= 19.00 ~ "mortality_Q1",
      indicator == "mortality" &
        number > 19.00 & number <= 112.00 ~ "mortality_Q2",
      indicator == "mortality" &
        number > 112.00 & number <= 635.75 ~ "mortality_Q3",
      indicator == "mortality" &
        number > 635.75 & number <= 2560612.00 ~ "mortality_Q4"
    ),
    asr_world_quartiles = case_when(
      indicator == "incidence" & asr_world <= 0.65 ~ "incidence_Q1",
      indicator == "incidence" &
        asr_world > 0.65 & asr_world <= 2.50 ~ "incidence_Q2",
      indicator == "incidence" &
        asr_world > 2.50 & asr_world <= 7.20 ~ "incidence_Q3",
      indicator == "incidence" &
        asr_world > 7.20 & asr_world <= 349.80 ~ "incidence_Q4",
      indicator == "mortality" & asr_world <= 0.31 ~ "mortality_Q1",
      indicator == "mortality" &
        asr_world > 0.31 & asr_world <= 1.50 ~ "mortality_Q2",
      indicator == "mortality" &
        asr_world > 1.50 & asr_world <= 4.10 ~ "mortality_Q3",
      indicator == "mortality" &
        asr_world > 4.10 & asr_world <= 181.30 ~ "mortality_Q4"
    ),
    crude_rate_quartiles = case_when(
      indicator == "incidence" & crude_rate <= 0.64 ~ "incidence_Q1",
      indicator == "incidence" &
        crude_rate > 0.64 & crude_rate <= 2.40 ~ "incidence_Q2",
      indicator == "incidence" &
        crude_rate > 2.40 & crude_rate <= 9.60 ~ "incidence_Q3",
      indicator == "incidence" &
        crude_rate > 9.60 & crude_rate <= 790.60 ~ "incidence_Q4",
      indicator == "mortality" &
        crude_rate <= 0.33 ~ "mortality_Q1",
      indicator == "mortality" &
        crude_rate > 0.33 & crude_rate <= 1.40 ~ "mortality_Q2",
      indicator == "mortality" &
        crude_rate > 1.40 & crude_rate <= 5.90 ~ "mortality_Q3",
      indicator == "mortality" &
        crude_rate > 5.90 & crude_rate <= 338.40 ~ "mortality_Q4"
    )
  )

# Adding world regions

globocan_5 <- globocan_4 |> 
  dplyr::left_join(regiones_mundo, by = "country_name")