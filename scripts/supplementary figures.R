# Load packages
library(ggpubr)
library(ggsci)
library(extrafont)
library(showtext)
library(mgcv)
library(patchwork)
library(here)
library(ggdist)

# Import data
source(here::here("scripts", "tidying data_cancer and indicators.R"))

# Function for the ASR plot
s_my_ggplot <- function(data, indicator, cancer, ...) {
  data_cancer_asr <- dplyr::filter(data, indicator != "MIR" & cancer_type == cancer)
  
  ggplot2::ggplot(
    data = data_cancer_asr,
    aes(
      x = .data[[indicator]],
      y = asr_world,
      color = indicator,
      shape = indicator
    )
  ) +
    ggplot2::geom_point(aes(fill = indicator), size = 2.1) +
    ggplot2::geom_smooth(
      method = "gam",
      formula = y ~ s(x, bs = "cr", k = -1),
      se = FALSE,
      linewidth = 1
    ) +
    ggplot2::geom_vline(
      xintercept = quantile(data_cancer_asr[[indicator]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
      linetype = "dashed",
      color = "gray70",
      linewidth = 1
    ) +
    ggplot2::labs(y = "ASR (per 100,000)", title = cancer) +
    ggplot2::scale_shape_manual(values = c(21, 24)) +
    ggsci::scale_color_igv() +
    ggsci::scale_fill_igv(alpha = 0.1) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text.x = element_text(hjust = 0),
      legend.title = element_blank(),
      text = element_text(size = 18, color = "black"),
      axis.line = element_line(colour = "black", linetype = "solid"),
      axis.ticks = element_line(colour = "black", linetype = "solid"),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 20)
    )
}

# Function for the MIR plot
s_my_ggplot_mir <- function(data, indicator, cancer, ...) {
  data_cancer_mir <- dplyr::filter(data, indicator == "MIR" & cancer_type == cancer)
  
  ggplot2::ggplot(
    data = data_cancer_mir, 
    aes(x = .data[[indicator]], y = asr_world)) +
    ggplot2::geom_point(size = 2.1, shape = 21, fill = "gray80") +
    ggplot2::geom_smooth(
      method = "gam",
      formula = y ~ s(x, bs = "cr", k = -1),
      se = FALSE,
      linewidth = 1,
      color = "black"
    ) +
    ggplot2::geom_vline(
      xintercept = quantile(data_cancer_mir[[indicator]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
      linetype = "dashed",
      color = "gray70",
      linewidth = 1
    ) +
    ggplot2::labs(y = "MIR") +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text.x = element_text(hjust = 0),
      text = element_text(size = 18, color = "black"),
      axis.line = element_line(colour = "black", linetype = "solid"),
      axis.ticks = element_line(colour = "black", linetype = "solid"),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 20)
    )
}

# Supplementary figure 1: Group A by EdI
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

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group
for (cancer in group_a) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- s_my_ggplot(s_figure_a_data, indicator = "ed_i", cancer = cancer)
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- s_my_ggplot_mir(s_figure_mir_a_data, indicator = "ed_i", cancer = cancer) 
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Combine all plots in plots_list into a 4-column layout
supplementary_figure_1 <-
  patchwork::wrap_plots(plots_list, ncol = 4) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group A",
    caption = "Education and Income index (EdI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 20, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.475, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_1,
  filename = here("outputs", "FIG_S1.jpeg"),
  width = 18,
  height = 25,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_1,
  filename = here("outputs", "FIG_S1.jpeg"),
  width = 18,
  height = 25,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = supplementary_figure_1,
  filename = here("outputs", "FIG_S1.eps"),
  width = 18,
  height = 25,
  units = "in"
)

# Supplementary figure 2: Group B by EdI
group_b <- c(
  "Lung",
  "Pancreas",
  "Brain, nervous system",
  "Mesothelioma"
)

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group
for (cancer in group_b) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- s_my_ggplot(s_figure_b_data, indicator = "ed_i", cancer = cancer)
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- s_my_ggplot_mir(s_figure_mir_b_data, indicator = "ed_i", cancer = cancer) 
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Combine all plots in plots_list into a 2-column layout
supplementary_figure_2 <-
  patchwork::wrap_plots(plots_list, ncol = 2) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group B",
    caption = "Education and Income index (EdI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 20, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.46, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_2,
  filename = here("outputs", "FIG_S2.jpeg"),
  width = 12,
  height = 14,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = supplementary_figure_2,
  filename = here("outputs", "FIG_S2.eps"),
  width = 12,
  height = 14,
  units = "in"
)

# Supplementary figure 3: Group C by EdI
group_c <- c(
  "Stomach",
  "Cervix uteri",
  "Oesophagus",
  "Larynx",
  "Vagina",
  "Kaposi sarcoma"
)

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group
for (cancer in group_c) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- s_my_ggplot(s_figure_c_data, indicator = "ed_i", cancer = cancer)
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- s_my_ggplot_mir(s_figure_mir_c_data, indicator = "ed_i", cancer = cancer) 
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Combine all plots in plots_list into a 3-column layout
supplementary_figure_3 <-
  patchwork::wrap_plots(plots_list, ncol = 3) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group C",
    caption = "Education and Income index (EdI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 20, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.47, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_3,
  filename = here("outputs", "FIG_S3.jpeg"),
  width = 16,
  height = 14,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_3,
  filename = here("outputs", "FIG_S3.jpeg"),
  width = 16,
  height = 14,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = supplementary_figure_3,
  filename = here("outputs", "FIG_S3.eps"),
  width = 16,
  height = 14,
  units = "in"
)

# Supplementary figure 4: Group D by EdI
group_c <- c(
  "Ovary",
  "Liver",
  "Hypopharynx",
  "Gallbladder",
  "Penis",
  "Salivary glands",
  "Nasopharynx"
)

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group
for (cancer in group_d) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- s_my_ggplot(s_figure_d_data, indicator = "ed_i", cancer = cancer)
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- s_my_ggplot_mir(s_figure_mir_d_data, indicator = "ed_i", cancer = cancer) 
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Combine all plots in plots_list into a 3-column layout
supplementary_figure_4 <-
  patchwork::wrap_plots(plots_list, ncol = 3) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group D",
    caption = "Education and Income index (EdI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 20, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.47, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_4,
  filename = here("outputs", "FIG_S4.jpeg"),
  width = 16,
  height = 18,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_4,
  filename = here("outputs", "FIG_S4.jpeg"),
  width = 16,
  height = 18,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = supplementary_figure_4,
  filename = here("outputs", "FIG_S4.eps"),
  width = 16,
  height = 18,
  units = "in"
)

# Supplementary figure 5: Group A by HDI
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

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group
for (cancer in group_a) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- s_my_ggplot(s_figure_a_data, indicator = "hdi", cancer = cancer)
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- s_my_ggplot_mir(s_figure_mir_a_data, indicator = "hdi", cancer = cancer) 
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Combine all plots in plots_list into a 4-column layout
supplementary_figure_5 <-
  patchwork::wrap_plots(plots_list, ncol = 4) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group A",
    caption = "Human Development Index (HDI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 20, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.475, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_5,
  filename = here("outputs", "FIG_S5.jpeg"),
  width = 18,
  height = 25,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_5,
  filename = here("outputs", "FIG_S5.jpeg"),
  width = 18,
  height = 25,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = supplementary_figure_5,
  filename = here("outputs", "FIG_S5.eps"),
  width = 18,
  height = 25,
  units = "in"
)

# Supplementary figure 6: Group B by HDI
group_b <- c(
  "Lung",
  "Pancreas",
  "Brain, nervous system",
  "Mesothelioma"
)

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group
for (cancer in group_b) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- s_my_ggplot(s_figure_b_data, indicator = "hdi", cancer = cancer)
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- s_my_ggplot_mir(s_figure_mir_b_data, indicator = "hdi", cancer = cancer) 
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Combine all plots in plots_list into a 4-column layout
supplementary_figure_6 <-
  patchwork::wrap_plots(plots_list, ncol = 2) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group B",
    caption = "Human Development Index (HDI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 20, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.46, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_6,
  filename = here("outputs", "FIG_S6.jpeg"),
  width = 12,
  height = 14,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_6,
  filename = here("outputs", "FIG_S6.jpeg"),
  width = 12,
  height = 14,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = supplementary_figure_6,
  filename = here("outputs", "FIG_S6.eps"),
  width = 12,
  height = 14,
  units = "in"
)

# Supplementary figure 7: Group C by HDI
group_c <- c(
  "Stomach",
  "Cervix uteri",
  "Oesophagus",
  "Larynx",
  "Vagina",
  "Kaposi sarcoma"
)

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group
for (cancer in group_c) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- s_my_ggplot(s_figure_c_data, indicator = "hdi", cancer = cancer)
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- s_my_ggplot_mir(s_figure_mir_c_data, indicator = "hdi", cancer = cancer) 
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Combine all plots in plots_list into a 4-column layout
supplementary_figure_7 <-
  patchwork::wrap_plots(plots_list, ncol = 3) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group C",
    caption = "Human Development Index (HDI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 20, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.47, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_7,
  filename = here("outputs", "FIG_S7.jpeg"),
  width = 16,
  height = 14,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_7,
  filename = here("outputs", "FIG_S7.jpeg"),
  width = 16,
  height = 14,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = supplementary_figure_7,
  filename = here("outputs", "FIG_S7.eps"),
  width = 16,
  height = 14,
  units = "in"
)

# Supplementary figure 8: Group D by HDI
group_d <- c(
  "Ovary",
  "Liver",
  "Hypopharynx",
  "Gallbladder",
  "Penis",
  "Salivary glands",
  "Nasopharynx"
)

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group
for (cancer in group_d) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- s_my_ggplot(s_figure_d_data, indicator = "hdi", cancer = cancer)
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- s_my_ggplot_mir(s_figure_mir_d_data, indicator = "hdi", cancer = cancer) 
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Combine all plots in plots_list into a 4-column layout
supplementary_figure_8 <-
  patchwork::wrap_plots(plots_list, ncol = 3) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group D",
    caption = "Human Development Index (HDI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 20, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.47, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_8,
  filename = here("outputs", "FIG_S8.jpeg"),
  width = 16,
  height = 18,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_8,
  filename = here("outputs", "FIG_S8.jpeg"),
  width = 16,
  height = 18,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = supplementary_figure_8,
  filename = here("outputs", "FIG_S8.eps"),
  width = 16,
  height = 18,
  units = "in"
)

# Supplementary figure 9: Group A by SDI
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

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group
for (cancer in group_a) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- s_my_ggplot(s_figure_a_data, indicator = "sdi", cancer = cancer)
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- s_my_ggplot_mir(s_figure_mir_a_data, indicator = "sdi", cancer = cancer) 
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Combine all plots in plots_list into a 4-column layout
supplementary_figure_9 <-
  patchwork::wrap_plots(plots_list, ncol = 4) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group A",
    caption = "Sociodemographic Index (SDI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 20, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.475, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_9,
  filename = here("outputs", "FIG_S9.jpeg"),
  width = 18,
  height = 25,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_9,
  filename = here("outputs", "FIG_S9.jpeg"),
  width = 18,
  height = 25,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = supplementary_figure_9,
  filename = here("outputs", "FIG_S9.eps"),
  width = 18,
  height = 25,
  units = "in"
)

# Supplementary figure 10: Group B by SDI
group_b <- c(
  "Lung",
  "Pancreas",
  "Brain, nervous system",
  "Mesothelioma"
)

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group
for (cancer in group_b) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- s_my_ggplot(s_figure_b_data, indicator = "sdi", cancer = cancer)
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- s_my_ggplot_mir(s_figure_mir_b_data, indicator = "sdi", cancer = cancer) 
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Combine all plots in plots_list into a 4-column layout
supplementary_figure_10 <-
  patchwork::wrap_plots(plots_list, ncol = 2) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group B",
    caption = "Sociodemographic Index (SDI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 20, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.46, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_10,
  filename = here("outputs", "FIG_S10.jpeg"),
  width = 12,
  height = 14,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_10,
  filename = here("outputs", "FIG_S10.jpeg"),
  width = 12,
  height = 14,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = supplementary_figure_10,
  filename = here("outputs", "FIG_S10.eps"),
  width = 12,
  height = 14,
  units = "in"
)

# Supplementary figure 11: Group C by SDI
group_c <- c(
  "Stomach",
  "Cervix uteri",
  "Oesophagus",
  "Larynx",
  "Vagina",
  "Kaposi sarcoma"
)

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group
for (cancer in group_c) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- s_my_ggplot(s_figure_c_data, indicator = "sdi", cancer = cancer)
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- s_my_ggplot_mir(s_figure_mir_c_data, indicator = "sdi", cancer = cancer) 
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Combine all plots in plots_list into a 4-column layout
supplementary_figure_11 <-
  patchwork::wrap_plots(plots_list, ncol = 3) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group C",
    caption = "Sociodemographic Index (SDI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 20, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.47, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_11,
  filename = here("outputs", "FIG_S11.jpeg"),
  width = 16,
  height = 18,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_11,
  filename = here("outputs", "FIG_S11.jpeg"),
  width = 16,
  height = 14,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = supplementary_figure_11,
  filename = here("outputs", "FIG_S11.eps"),
  width = 16,
  height = 14,
  units = "in"
)

# Supplementary figure 12: Group D by SDI
group_c <- c(
  "Ovary",
  "Liver",
  "Hypopharynx",
  "Gallbladder",
  "Penis",
  "Salivary glands",
  "Nasopharynx"
)

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group
for (cancer in group_d) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- s_my_ggplot(s_figure_d_data, indicator = "sdi", cancer = cancer)
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- s_my_ggplot_mir(s_figure_mir_d_data, indicator = "sdi", cancer = cancer) 
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Combine all plots in plots_list into a 4-column layout
supplementary_figure_12 <-
  patchwork::wrap_plots(plots_list, ncol = 3) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group D",
    caption = "Sociodemographic Index (SDI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 20, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.47, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_12,
  filename = here("outputs", "FIG_S12.jpeg"),
  width = 16,
  height = 18,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_12,
  filename = here("outputs", "FIG_S12.jpeg"),
  width = 16,
  height = 18,
  dpi = 500,
  units = "in"
)

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = supplementary_figure_12,
  filename = here("outputs", "FIG_S12.eps"),
  width = 16,
  height = 18,
  units = "in"
)

# Supplementary figure 13: One-Way ANOVA tests for MIR by socioeconomic development indicators

## Define a function to create the ANOVA tests
calculate_anova <- function(data, anova_data, indicator, label_x, label_y) {
  ggpubr::ggboxplot(
    data = data,
    x = indicator,
    y = "asr_world",
    fill = indicator,
    width = 0.5,
    palette = "nejm",
    alpha = 0.6,
    xlab = label_x,
    ylab = label_y,
    facet.by = "cancer_type",
    scales = "free",
    nrow = 2,
    ncol = 5
  ) +
    # Add density plots
    ggdist::stat_halfeye(
      adjust = 1,
      width = 0.6,
      justification = -0.3,
      .width = 0,
      alpha = 0.5
    ) +
    # Add jittered points
    ggbeeswarm::geom_quasirandom(alpha = 0.5, size = 3.5) +
    # Add p-value stars
    ggpubr::stat_pvalue_manual(
      anova_data,
      label = "p.adj.signif",
      step.group.by = "cancer_type",
      y.position = "y.position",
      step.increase = 0.06,
      size = 7.3
    ) +
    # Add facets by cancer type
    ggplot2::facet_wrap(~ cancer_type, scales = "free", nrow = 2, ncol = 5) +
    # Add labels for axes
    ggplot2::labs(x = label_x, y = label_y) +
    # Theme customization
    ggplot2::theme(
      axis.title = element_text(
        size = 26,
        color = "black"
      ),
      axis.text = element_text(
        size = 26,
        color = "black"
      ),
      strip.text = element_text(
        size = 26,
        color = "black"
      ),
      panel.spacing = unit(1.5, "lines"),
      axis.text.x = element_text(
        margin = margin(5, 0, 10, 0),
        angle = 35,
        hjust = 1
      ),
      axis.text.y = element_text(margin = margin(0, 5, 0, 10)),
      legend.position = "none"
    )
}

## One-Way ANOVA test of mortality by EdI category
supplementary_figure_13a <- calculate_anova(
  data = data_mir_edi_globocan_8,
  anova_data = pw_gh_test_mir_edi,
  indicator = "edi_categories",
  label_x = "Education and Income Index (EdI)",
  label_y = "Mortality-to-Incidence Ratio (MIR)"
)

## One-Way ANOVA test of incidence by HDI category
supplementary_figure_13b <- calculate_anova(
  data = data_mir_hdi_globocan_8, 
  anova_data = pw_gh_test_mir_hdi, 
  indicator = "hdi_category", 
  label_x = "Human Development Index (HDI)", 
  label_y = "Mortality-to-Incidence Ratio (MIR)")

## One-Way ANOVA test of incidence by SDI category
supplementary_figure_13c <- calculate_anova(
  data = data_mir_sdi_globocan_8, 
  anova_data = pw_gh_test_mir_sdi, 
  indicator = "sdi_categories", 
  label_x = "Sociodemographic Index (SDI)", 
  label_y = "Mortality-to-Incidence Ratio (MIR)")


## Combine all subplots into a single plot
supplementary_figure_13 <- 
  supplementary_figure_13a / supplementary_figure_13b / supplementary_figure_13c + 
  patchwork::plot_layout(ncol = 1, axis_titles = 'collect_y', heights = c(1, 1, 2.5))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = supplementary_figure_13,
  filename = here::here("outputs", "FIG_S13.jpeg"),
  width = 26,
  height = 26,
  dpi = 500,
  units = "in")

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = supplementary_figure_13,
  filename = here::here("outputs", "FIG_S13.eps"),
  width = 26,
  height = 26,
  units = "in")