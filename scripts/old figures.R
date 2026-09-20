# Load packages
library(ggrepel)
library(RColorBrewer)
library(here)
library(patchwork)

# Import data
source(here::here("scripts", "tidying data_cancer and indicators.R"))

# Figure 1: Global

## Figure 1A
figure_1_a <- globocan_8 |>
  dplyr::filter(indicator != "MIR" & cancer_type == "All cancers*") |>
  ggplot2::ggplot(aes(
    x = ed_i,
    y = asr_world,
    color = indicator,
    shape = indicator
  )) +
  ggplot2::geom_point(aes(fill = indicator), size = 2.5) +
  ggplot2::geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "cr", k = -1),
    se = FALSE,
    linewidth = 1
  ) +
  ggplot2::geom_vline(
    xintercept = quantile(
      globocan_8$ed_i[globocan_8$indicator != "MIR" & globocan_8$cancer_type == "All cancers*"], 
      probs = c(0.25, 0.5, 0.75), 
      na.rm = TRUE),
    linetype = "dashed",
    color = "gray70",
    linewidth = 1
  ) +
  ggrepel::geom_label_repel(aes(label = country_label), show.legend = FALSE) +
  ggplot2::scale_shape_manual(values = c(21, 24)) +
  ggsci::scale_color_igv() +
  ggsci::scale_fill_igv(alpha = 0.1) +
  ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  ggplot2::labs(
    x = "Education and Income index (EdI)",
    y = "Age-Standardized Rate (per 100,000)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    strip.text.x = element_text(hjust = 0),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(
      size = 18,
      color = "black",
      family = "Syne"
    ),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.grid = element_blank()
  )

## Figure 1B
figure_1_b <- globocan_8 |>
  dplyr::filter(indicator == "MIR" & cancer_type == "All cancers*") |>
  ggplot2::ggplot(aes(x = ed_i, y = asr_world)) +
  ggplot2::geom_point(size = 2, shape = 21) +
  ggplot2::geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "cr", k = -1),
    se = FALSE,
    linewidth = 1,
    color = "black"
  ) +
  ggplot2::geom_vline(
    xintercept = quantile(
      globocan_8$ed_i[globocan_8$indicator == "MIR" & globocan_8$cancer_type == "All cancers*"],
      probs = c(0.25, 0.5, 0.75),
      na.rm = TRUE), 
    linetype = "dashed",
    color = "gray70",
    linewidth = 1
  ) +
  ggrepel::geom_label_repel(aes(label = country_label), show.legend = FALSE) +
  ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  ggplot2::labs(
    x = "Education and Income index (EdI)",
    y = "Mortality-to-Incidence Ratio") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    strip.text.x = element_text(hjust = 0),
    text = element_text(size = 18, color = "black", family = "Syne"),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.title = element_text(color = "black"),
    panel.grid = element_blank()
  )

## Arrange multiple plots
figure_1 <- figure_1_a / figure_1_b

## Save figure 1 (PNG)
ggsave(
  plot = figure_1,
  filename = here("outputs", "FIG_1_old.png"),
  width = 10,
  height = 10,
  dpi = 300,
  units = "in")

# Figure 2: ASR

## My ggplot function for figure 2 (ASR)
my_ggplot <- function(data, ...) {
  ggplot2::ggplot(
    data = data,
    aes(
      x = ed_i,
      y = asr_world,
      color = indicator,
      shape = indicator
    )
  ) +
    ggplot2::geom_point(aes(fill = indicator), size = 2.5) +
    ggplot2::geom_smooth(
      method = "gam",
      formula = y ~ s(x, bs = "cr", k = -1),
      se = FALSE,
      linewidth = 1) +
    ggplot2::geom_vline(
      xintercept = quantile(data$ed_i, probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
      linetype = "dashed",
      color = "gray70",
      linewidth = 1
    ) +
    ggplot2::scale_shape_manual(values = c(21, 24)) +
    ggsci::scale_color_igv() +
    ggsci::scale_fill_igv(alpha = 0.1) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(
      vars(cancer_type),
      labeller = labeller(cancer_type = label_wrap_gen(50)),
      ncol = 3,
      scales = "free"
    ) +
    ggplot2::theme(
      strip.text.x = element_text(hjust = 0),
      legend.position = "bottom",
      legend.title = element_blank(),
      text = element_text(size = 18, color = "black", family = "Syne"),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      panel.grid = element_blank(),
      plot.title = element_text(size = 24)
    )
}

## Figure 2A
figure_2A_data <- globocan_8 |>
  dplyr::filter(
    indicator != "MIR",
    cancer_type %in% c(
      "Breast",
      "Prostate",
      "Colorectum",
      "Melanoma of skin",
      "Corpus uteri",
      "Bladder"
    )
  ) |>
  dplyr::mutate(cancer_type = factor(
    cancer_type,
    levels = c(
      "Breast",
      "Prostate",
      "Colorectum",
      "Melanoma of skin",
      "Corpus uteri",
      "Bladder"
    )
  ))

figure_2A <- my_ggplot(figure_2A_data) +
  ggplot2::labs(title = "Group A") +
  ggplot2::theme(axis.title = element_blank())

## Figure 2B
figure_2B_data <- globocan_8 |>
  dplyr::filter(
    indicator != "MIR",
    cancer_type %in% c(
      "Trachea, bronchus and lung",
      "Pancreas",
      "Brain, nervous system"
    )
  ) |>
  dplyr::mutate(cancer_type = factor(
    cancer_type,
    levels = c(
      "Trachea, bronchus and lung",
      "Pancreas",
      "Brain, nervous system"
    )
  ))

figure_2B <- my_ggplot(figure_2B_data) +
  ggplot2::labs(y = "Age-Standardized Rate (per 100,000)", title = "Group B") +
  ggplot2::theme(axis.title.y = element_text(hjust = 0), axis.title.x = element_blank())

## Figure 2C
figure_2C_data <- globocan_8 |>
  dplyr::filter(
    indicator != "MIR",
    cancer_type %in% c(
      "Cervix uteri",
      "Stomach",
      "Oesophagus"
    )
  ) |>
  dplyr::mutate(cancer_type = factor(
    cancer_type,
    levels = c(
      "Cervix uteri",
      "Stomach",
      "Oesophagus"
    )
  ))

figure_2C <- my_ggplot(figure_2C_data) +
  ggplot2::labs(x = "Education and Income index (EdI)", title = "Group C") +
  ggplot2::theme(
    axis.title.y = element_blank(), 
    axis.title.x = element_text(margin = margin(t = 20)))

## Arrange multiple plots
figure_2 <-
  figure_2A + (figure_2B + figure_2C + plot_layout(ncol = 1)) +
  plot_layout(ncol = 1, guides = 'collect', axis_titles = 'collect') &
  theme(legend.position = 'bottom')

## Save figure 2 (PNG)
ggsave(
  plot = figure_2,
  filename = here("outputs", "FIG_2_old.png"),
  width = 20,
  height = 15,
  dpi = 300,
  units = "in")

# Figure 3: MIR

## My ggplot function for figure 3 (MIR)
my_ggplot_mir <- function(data, ...) {
  ggplot2::ggplot(data = data, aes(x = ed_i, y = asr_world)) +
    ggplot2::geom_point(size = 2, shape = 21) +
    ggplot2::geom_smooth(
      method = "gam",
      formula = y ~ s(x, bs = "cr", k = -1),
      se = FALSE,
      linewidth = 1,
      color = "black"
    ) +
    ggplot2::geom_vline(
      xintercept = quantile(
        data$ed_i,
        probs = c(0.25, 0.5, 0.75),
        na.rm = TRUE
      ),
      linetype = "dashed",
      color = "gray70",
      linewidth = 1
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(
      vars(cancer_type),
      labeller = labeller(cancer_type = label_wrap_gen(50)),
      ncol = 3,
      scales = "free"
    ) +
    ggplot2::theme(
      strip.text.x = element_text(hjust = 0),
      text = element_text(
        size = 18,
        color = "black",
        family = "Syne"
      ),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      axis.title = element_text(color = "black"),
      panel.grid = element_blank()
    )
}

# Figure 4

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
# # Figure 4
# figure_4 <- ggplot2::ggplot(
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
#     axis.ticks = element_line(colour = "black"),
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
# # Save figure 4 (PNG)
# ggsave(
#   plot = figure_4,
#   filename = here("outputs", "FIG_4.png"),
#   width = 14,
#   height = 8,
#   dpi = 500,
#   units = "in")