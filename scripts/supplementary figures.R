# Load packages
library(ggpubr)
library(ggsci)
library(extrafont)
library(showtext)
library(mgcv)
library(patchwork)
library(here)

# Import data
source(here::here("scripts", "Tidying data_Indicators cancers and countries.R"))

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
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text.x = element_text(hjust = 0),
      legend.title = element_blank(),
      text = element_text(size = 18, color = "black", family = "Syne"),
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
      text = element_text(size = 18, color = "black", family = "Syne"),
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
      plot.title = element_text(size = 28, color = "black", family = "Syne"),
      plot.caption = element_text(size = 20, color = "black", family = "Syne", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black", family = "Syne"),
    legend.justification = c(0.475, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a PNG file
ggplot2::ggsave(
  plot = supplementary_figure_1,
  filename = here("outputs", "FIG_S1.png"),
  width = 18,
  height = 25,
  dpi = 300,
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
  "Trachea, bronchus and lung",
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
      plot.title = element_text(size = 28, color = "black", family = "Syne"),
      plot.caption = element_text(size = 20, color = "black", family = "Syne", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black", family = "Syne"),
    legend.justification = c(0.46, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a PNG file
ggplot2::ggsave(
  plot = supplementary_figure_2,
  filename = here("outputs", "FIG_S2.png"),
  width = 12,
  height = 14,
  dpi = 300,
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
      plot.title = element_text(size = 28, color = "black", family = "Syne"),
      plot.caption = element_text(size = 20, color = "black", family = "Syne", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black", family = "Syne"),
    legend.justification = c(0.47, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a PNG file
ggplot2::ggsave(
  plot = supplementary_figure_3,
  filename = here("outputs", "FIG_S3.png"),
  width = 16,
  height = 14,
  dpi = 300,
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
      plot.title = element_text(size = 28, color = "black", family = "Syne"),
      plot.caption = element_text(size = 20, color = "black", family = "Syne", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black", family = "Syne"),
    legend.justification = c(0.47, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a PNG file
ggplot2::ggsave(
  plot = supplementary_figure_4,
  filename = here("outputs", "FIG_S4.png"),
  width = 16,
  height = 18,
  dpi = 300,
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
      plot.title = element_text(size = 28, color = "black", family = "Syne"),
      plot.caption = element_text(size = 20, color = "black", family = "Syne", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black", family = "Syne"),
    legend.justification = c(0.475, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a PNG file
ggplot2::ggsave(
  plot = supplementary_figure_5,
  filename = here("outputs", "FIG_S5.png"),
  width = 18,
  height = 25,
  dpi = 300,
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
  "Trachea, bronchus and lung",
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
      plot.title = element_text(size = 28, color = "black", family = "Syne"),
      plot.caption = element_text(size = 20, color = "black", family = "Syne", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black", family = "Syne"),
    legend.justification = c(0.46, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a PNG file
ggplot2::ggsave(
  plot = supplementary_figure_6,
  filename = here("outputs", "FIG_S6.png"),
  width = 12,
  height = 14,
  dpi = 300,
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
      plot.title = element_text(size = 28, color = "black", family = "Syne"),
      plot.caption = element_text(size = 20, color = "black", family = "Syne", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black", family = "Syne"),
    legend.justification = c(0.47, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a PNG file
ggplot2::ggsave(
  plot = supplementary_figure_7,
  filename = here("outputs", "FIG_S7.png"),
  width = 16,
  height = 14,
  dpi = 300,
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
      plot.title = element_text(size = 28, color = "black", family = "Syne"),
      plot.caption = element_text(size = 20, color = "black", family = "Syne", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black", family = "Syne"),
    legend.justification = c(0.47, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a PNG file
ggplot2::ggsave(
  plot = supplementary_figure_8,
  filename = here("outputs", "FIG_S8.png"),
  width = 16,
  height = 18,
  dpi = 300,
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
      plot.title = element_text(size = 28, color = "black", family = "Syne"),
      plot.caption = element_text(size = 20, color = "black", family = "Syne", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black", family = "Syne"),
    legend.justification = c(0.475, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a PNG file
ggplot2::ggsave(
  plot = supplementary_figure_9,
  filename = here("outputs", "FIG_S9.png"),
  width = 18,
  height = 25,
  dpi = 300,
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
  "Trachea, bronchus and lung",
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
      plot.title = element_text(size = 28, color = "black", family = "Syne"),
      plot.caption = element_text(size = 20, color = "black", family = "Syne", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black", family = "Syne"),
    legend.justification = c(0.46, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a PNG file
ggplot2::ggsave(
  plot = supplementary_figure_10,
  filename = here("outputs", "FIG_S10.png"),
  width = 12,
  height = 14,
  dpi = 300,
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
      plot.title = element_text(size = 28, color = "black", family = "Syne"),
      plot.caption = element_text(size = 20, color = "black", family = "Syne", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black", family = "Syne"),
    legend.justification = c(0.47, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a PNG file
ggplot2::ggsave(
  plot = supplementary_figure_11,
  filename = here("outputs", "FIG_S11.png"),
  width = 16,
  height = 14,
  dpi = 300,
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
      plot.title = element_text(size = 28, color = "black", family = "Syne"),
      plot.caption = element_text(size = 20, color = "black", family = "Syne", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black", family = "Syne"),
    legend.justification = c(0.47, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Save the combined plot as a PNG file
ggplot2::ggsave(
  plot = supplementary_figure_12,
  filename = here("outputs", "FIG_S12.png"),
  width = 16,
  height = 18,
  dpi = 300,
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