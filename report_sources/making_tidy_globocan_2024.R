library(tidyverse)
library(fs)

input_dir  <- "data/raw_2024"          # <-- folder with your dataset-*.csv files
output_dir <- "data/processed_2024"
dir_create(output_dir)

files <- dir_ls(input_dir, regexp = "dataset-.*\\.csv$")
stopifnot("No files matched in input_dir - check the path and that files start with 'dataset-'" = length(files) > 0)

globocan <- files %>%
  set_names(path_file(.)) %>%                     # keep filename for parsing
  imap_dfr(~ read_csv(
    .x,
    col_types = cols(.default = col_character()),  # read everything as text first...
    na = c("", "-", "NA")
  ) %>% mutate(source_file = .y)) %>%
  tidyr::extract(
    source_file,
    into   = c("indicator", "sex_group", "cancer_site"),
    regex  = "dataset-absolute-numbers-(inc|mort)-(both-sexes|males|females)-in-\\d{4}-(.+)\\.csv",
    remove = FALSE
  ) %>%
  mutate(
    metric      = recode(indicator, inc = "Incidence", mort = "Mortality"),
    cancer_site = str_replace_all(cancer_site, "-", " ") %>% str_to_sentence(),
    # ...then convert the numeric columns explicitly, now that everything is consistent
    across(c(`Cancer code`, `Country`, `Sex`, `Number`,
             `95% UI low`, `95% UI high`, `ASR (World)`,
             `Crude rate`, `Cumulative risk`), as.numeric)
  )


## Epidemiological indicators
globocan_2024 <-
  globocan |> 
  janitor::clean_names() |>
  dplyr::filter(label != "Total") |>
  dplyr::select(-sex_group, -sex, -source_file, -metric,
                x95_percent_ui_low, x95_percent_ui_high) |> 
  dplyr::mutate(
    indicator = dplyr::case_when(
      indicator == "mort" ~ "mortality",
      indicator == "inc" ~ "incidence"
    )
  )


write_csv(globocan_2024, path(output_dir, "globocan_2024_combined.csv"))

glimpse(globocan)






