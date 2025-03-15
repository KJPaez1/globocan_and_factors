# Load packages
library(ggpubr)
library(ggsci)
library(mgcv)
library(patchwork)
library(here)
library(ggdist)

# Fonts
# library(extrafont)
# library(showtext)

# Import data
source(here::here("scripts", "tidying data_cancer and indicators.R"))

# Figure 1: R2 heatmaps

## Define a function to create a heatmap
heatmap <- function(data, title) {
  ggplot2::ggplot(
    data = data,
    aes(
      x = indicator,
      y = cancer_type,
      fill = R2
    )
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(aes(label = sprintf("%.2f", R2)), color = "black", size = 4.5) +
    ggplot2::scale_fill_distiller(palette = "RdBu", direction = -1, limits = c(0, 0.8), name = bquote(R^2)) +
    ggplot2::labs(title = title, x = "SocioEducation and Income Indicators") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = element_text(size = 15, color = "black"),
      axis.text.y = element_text(size = 15, color = "black"),
      axis.ticks.y = element_blank(),
      axis.title.x = element_text(size = 16, color = "black", margin = margin(t = 10)),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      legend.text = element_text(size = 15, color = "black"), 
      legend.title = element_text(size = 16, color = "black"), 
      legend.key.size = unit(1, "cm"),
      plot.title = element_text(hjust = 0.5, size = 16, color = "black")
    )
  }

## Generate the heatmaps for each outcome
heatmap_incidence <- heatmap(r2_incidence, title = "Incidence")
heatmap_mortality <- heatmap(r2_mortality, title = "Mortality")
heatmap_mir <- heatmap(r2_mir, title = "MIR")

## Combine the heatmaps into a single plot
figure_1 <- (heatmap_incidence + heatmap_mortality + heatmap_mir) +
  patchwork::plot_layout(guides = "collect", axis_titles = "collect_x") &
  ggplot2::theme(legend.position = 'right')

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = figure_1,
  filename = here("outputs", "FIG_1.jpeg"),
  width = 16,
  height = 10,
  dpi = 500,
  units = "in")

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = figure_1,
  filename = here("outputs", "FIG_1.eps"),
  width = 16,
  height = 10,
  units = "in")

# Figure 2: ASR and MIR by EdI for all cancers

## Function for the ASR plot
my_ggplot <- function(data, indicator, cancer, ...) {
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
    ggplot2::geom_point(aes(fill = indicator), size = 2.5) +
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
    ggrepel::geom_label_repel(aes(label = country_label), show.legend = FALSE) +
    ggplot2::scale_shape_manual(values = c(21, 24)) +
    ggsci::scale_color_igv() +
    ggsci::scale_fill_igv(alpha = 0.1) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(y = "Age-Standardized Rate (per 100,000)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text.x = element_text(hjust = 0),
      legend.position = "bottom",
      legend.title = element_blank(),
      text = element_text(size = 18, color = "black"),
      axis.line = element_line(colour = "black", linetype = "solid"),
      axis.ticks = element_line(colour = "black", linetype = "solid"),
      panel.grid = element_blank(),
      plot.title = element_text(size = 20)
    )
}

## Function for the MIR plot
my_ggplot_mir <- function(data, indicator, cancer, ...) {
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
    ggrepel::geom_label_repel(aes(label = country_label), show.legend = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(y = "Mortality-to-Incidence Ratio (MIR)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text.x = element_text(hjust = 0),
      text = element_text(size = 18, color = "black"),
      axis.line = element_line(colour = "black", linetype = "solid"),
      axis.ticks = element_line(colour = "black", linetype = "solid"),
      panel.grid = element_blank(),
      plot.title = element_text(size = 20)
    )
}

## Figures
figure_a2 <- my_ggplot(
  data = globocan_8, indicator = "ed_i", cancer = "All cancers*") +
  ggplot2::labs(x = "Education and Income index (EdI)")

figure_b2 <- my_ggplot(
  data = globocan_8, indicator = "hdi", cancer = "All cancers*") +
  ggplot2::labs(x = "Human Development Index (HDI)")

figure_c2 <- my_ggplot(
  data = globocan_8, indicator = "sdi", cancer = "All cancers*") +
  ggplot2::labs(x = "Sociodemographic Index (SDI)")

figure_d2 <- my_ggplot_mir(
  data = globocan_8, indicator = "ed_i", cancer = "All cancers*") +
  ggplot2::labs(x = "Education and Income index (EdI)")

figure_e2 <- my_ggplot_mir(
  data = globocan_8, indicator = "hdi", cancer = "All cancers*") +
  ggplot2::labs(x = "Human Development Index (HDI)")

figure_f2 <- my_ggplot_mir(
  data = globocan_8, indicator = "sdi", cancer = "All cancers*") +
  ggplot2::labs(x = "Sociodemographic Index (SDI)")

## Combine all subplots into a single plot
figure_2 <- (figure_a2 + figure_d2 + figure_b2 + figure_e2 + figure_c2 + figure_f2) +
  plot_layout(ncol = 2, guides = 'collect', axis_titles = 'collect_y') &
  theme(
    legend.direction = "horizontal", 
    legend.position = 'bottom',
    legend.text = element_text(size = 20))

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = figure_2,
  filename = here("outputs", "FIG_2.jpeg"),
  width = 13,
  height = 13,
  dpi = 500,
  units = "in")

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = figure_2,
  filename = here("outputs", "FIG_2.eps"),
  width = 13,
  height = 13,
  units = "in")

# Figure 3: ASR and MIR by EdI and cancer types

## Group A
group_a <- c(
  "Breast",
  "Prostate",
  "Colorectum",
  "Melanoma of skin",
  "Corpus uteri",
  "Bladder"
)

## Group B
group_b <- c(
  "Lung",
  "Pancreas",
  "Brain, nervous system"
)

## Group C
group_c <- c(
  "Stomach",
  "Cervix uteri",
  "Oesophagus"
)

## Function for the ASR plot
my_ggplot_1 <- function(data, indicator, cancer, ...) {
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
    ggplot2::geom_point(aes(fill = indicator), size = 2.5) +
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
      axis.title = element_text(size = 20),
      axis.ticks = element_line(colour = "black", linetype = "solid"),
      panel.grid = element_blank(),
      plot.title = element_text(size = 20)
    )
}

## Function for the MIR plot
my_ggplot_mir_1 <- function(data, indicator, cancer, ...) {
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
      text = element_text(size = 20, color = "black"),
      axis.line = element_line(colour = "black", linetype = "solid"),
      axis.title = element_text(size = 20),
      axis.ticks = element_line(colour = "black", linetype = "solid"),
      panel.grid = element_blank(),
      plot.title = element_text(size = 20)
    )
}

## Part 1: Education and Income index (EdI)

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group A
for (cancer in group_a) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- my_ggplot_1(globocan_8, indicator = "ed_i", cancer = cancer) +
    theme(axis.title.x = element_blank(), legend.position = "none")
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- my_ggplot_mir_1(globocan_8, indicator = "ed_i", cancer = cancer) +
    theme(axis.title.x = element_blank())
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Arrange multiple plots
figure_3a <-
  patchwork::wrap_plots(plots_list, ncol = 3) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group A",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.margin = unit(c(1, 1, 1, 1), "cm")))

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group B
for (cancer in group_b) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- my_ggplot_1(globocan_8, indicator = "ed_i", cancer = cancer) +
    theme(axis.title.x = element_blank(), legend.position = "none")
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- my_ggplot_mir_1(globocan_8, indicator = "ed_i", cancer = cancer) +
    theme(axis.title.x = element_blank())
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Arrange multiple plots
figure_3b <-
  patchwork::wrap_plots(plots_list, ncol = 3) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group B",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.margin = unit(c(1, 1, 1, 1), "cm")))

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group C
for (cancer in group_c) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- my_ggplot_1(globocan_8, indicator = "ed_i", cancer = cancer) +
    theme(axis.title.x = element_blank(), legend.position = "none")
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- my_ggplot_mir_1(globocan_8, indicator = "ed_i", cancer = cancer) +
    theme(axis.title.x = element_blank())
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Arrange multiple plots
figure_3c <-
  patchwork::wrap_plots(plots_list, ncol = 3) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group C",
    caption = "Education and Income index (EdI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 24, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.45, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Part 2: Sociodemographic Index (SDI)

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group A
for (cancer in group_a) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- my_ggplot_1(globocan_8, indicator = "sdi", cancer = cancer) +
    theme(axis.title.x = element_blank(), legend.position = "none")
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- my_ggplot_mir_1(globocan_8, indicator = "sdi", cancer = cancer) +
    theme(axis.title.x = element_blank())
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Arrange multiple plots
figure_3d <-
  patchwork::wrap_plots(plots_list, ncol = 3) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group A",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.margin = unit(c(1, 1, 1, 1), "cm")))

## Create an empty list to store the plots
plots_list <- list()

## Loop through each cancer type in group B
for (cancer in group_b) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- my_ggplot_1(globocan_8, indicator = "sdi", cancer = cancer) +
    theme(axis.title.x = element_blank(), legend.position = "none")
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- my_ggplot_mir_1(globocan_8, indicator = "sdi", cancer = cancer)+
    theme(axis.title.x = element_blank())
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Arrange multiple plots
figure_3e <-
  patchwork::wrap_plots(plots_list, ncol = 3) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group B",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.margin = unit(c(1, 1, 1, 1), "cm")))

## Create an empty list to store the plots C
plots_list <- list()

## Loop through each cancer type in group
for (cancer in group_c) {
  # Generate the first plot using s_my_ggplot function
  plot1 <- my_ggplot_1(globocan_8, indicator = "sdi", cancer = cancer) +
    theme(axis.title.x = element_blank(), legend.position = "none")
  # Generate the second plot using s_my_ggplot_mir function
  plot2 <- my_ggplot_mir_1(globocan_8, indicator = "sdi", cancer = cancer) +
    theme(axis.title.x = element_blank())
  # Combine plot1 and plot2 vertically and add to the plots_list
  plots_list <- append(plots_list, list(plot1 / plot2))
}

## Arrange multiple plots
figure_3f <-
  patchwork::wrap_plots(plots_list, ncol = 3) +
  # Add a title to the combined plot and customize its appearance
  patchwork::plot_annotation(
    title = "Group C",
    caption = "Sociodemographic Index (SDI)",
    theme = theme(
      plot.title = element_text(size = 28, color = "black"),
      plot.caption = element_text(size = 24, color = "black", hjust = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"))) +
  # Collect all legends into a single legend and customize its appearance
  patchwork::plot_layout(
    guides = "collect") & 
  ggplot2::theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    legend.justification = c(0.45, 1),
    legend.margin = margin(t = 10, r = 10, b = 15, l = 10))

## Combine all subplots into a single plot
figure_3 <-
  ggpubr::ggarrange(
    figure_3a, figure_3d,
    figure_3b, figure_3e,
    figure_3c, figure_3f,
    ncol = 2,
    nrow = 3,
    common.legend = TRUE,
    heights = c(2, 1.1, 1.3)
  )

## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = figure_3,
  filename = here("outputs", "FIG_3.jpeg"),
  width = 24,
  height = 26,
  dpi = 500,
  units = "in")

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = figure_3,
  filename = here("outputs", "FIG_3.eps"),
  width = 24,
  height = 26,
  units = "in")

# Figure 4: One-Way ANOVA tests for incidence by socioEducation and Income indicators

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

## One-Way ANOVA test of incidence by EdI category
figure_4a <- calculate_anova(
  data = data_incidence_edi_globocan_8,
  anova_data = pw_gh_test_incidence_edi,
  indicator = "edi_categories",
  label_x = "Education and Income Index (EdI)",
  label_y = "Age-Standardized Rate (per 100,000)"
  ) + 
  ggplot2::labs(tag = "A)") +
  ggplot2::theme(
    plot.tag = ggplot2::element_text(size = 35, face = "bold"),
    plot.tag.position = c(0.02, 0.95),
    axis.title.y = ggplot2::element_text(colour = "white"))

## One-Way ANOVA test of incidence by HDI category
figure_4b <- calculate_anova(
  data = data_incidence_hdi_globocan_8, 
  anova_data = pw_gh_test_incidence_hdi, 
  indicator = "hdi_category", 
  label_x = "Human Development Index (HDI)", 
  label_y = "Age-Standardized Rate (per 100,000)"
  ) +
  ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "white"))

## One-Way ANOVA test of incidence by SDI category
figure_4c <- calculate_anova(
  data = data_incidence_sdi_globocan_8, 
  anova_data = pw_gh_test_incidence_sdi, 
  indicator = "sdi_categories", 
  label_x = "Sociodemographic Index (SDI)", 
  label_y = "Age-Standardized Rate (per 100,000)")

# Figure 5: One-Way ANOVA tests for mortality by socioEducation and Income indicators

## One-Way ANOVA test of mortality by EdI category
figure_5a <- calculate_anova(
  data = data_mortality_edi_globocan_8,
  anova_data = pw_gh_test_mortality_edi,
  indicator = "edi_categories",
  label_x = "Education and Income Index (EdI)",
  label_y = "Age-Standardized Rate (per 100,000)"
  ) + 
  ggplot2::labs(tag = "B)") +
  ggplot2::theme(
    plot.tag = ggplot2::element_text(size = 35, face = "bold"),
    plot.tag.position = c(0.09, 0.95),
    axis.title.y = ggplot2::element_text(colour = "white"))

## One-Way ANOVA test of incidence by HDI category
figure_5b <- calculate_anova(
  data = data_mortality_hdi_globocan_8, 
  anova_data = pw_gh_test_mortality_hdi, 
  indicator = "hdi_category", 
  label_x = "Human Development Index (HDI)", 
  label_y = "Age-Standardized Rate (per 100,000)"
  ) +
  ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "white"))

## One-Way ANOVA test of incidence by SDI category
figure_5c <- calculate_anova(
  data = data_mortality_sdi_globocan_8, 
  anova_data = pw_gh_test_mortality_sdi, 
  indicator = "sdi_categories", 
  label_x = "Sociodemographic Index (SDI)", 
  label_y = "Age-Standardized Rate (per 100,000)"
  ) +
  ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "white"))

## Combine all subplots into a single plot
figure_4 <- figure_4a + figure_4b + figure_4c + (figure_5a + figure_5b + plot_layout(ncol = 2, widths = c(0.85, 3))) + figure_5c + patchwork::plot_layout(ncol = 1, heights = c(1, 1, 1, 1, 1))


## Save the combined plot as a JPEG file
ggplot2::ggsave(
  plot = figure_4,
  filename = here::here("outputs", "FIG_4.jpeg"),
  width = 30,
  height = 30,
  dpi = 300,
  units = "in")

## Save the combined plot as a EPS file
ggplot2::ggsave(
  plot = figure_4,
  filename = here::here("outputs", "FIG_4.eps"),
  width = 26,
  height = 16,
  units = "in")

# Figure 5: One-Way ANOVA test of mortality by EdI category
# calculate_anova <- function(data, cancer_type) {
#   # Perform one-way ANOVA test
#   anova_test <- stats::aov(data$asr_world ~ data$edi_categories)
#   # Perform post-hoc Tukey test
#   tukey_test <- emmeans::emmeans(anova_test, "edi_categories", adjust = "tukey")
#   # Perform pairwise Tukey test
#   pw_test <- emmeans::pairs(tukey_test)
#   # Perform pairwise Games-Howell test
#   pw_gh_test <- emmeans::pairs(tukey_test, infer = c(TRUE, TRUE))
#   # Create a data frame to store the results
#   results <- data.frame(
#     cancer_type = cancer_type,
#     anova = summary(anova_test)[[1]][["Pr(>F)"]],
#     tukey = summary(tukey_test)[[1]][["p-value"]],
#     pw_test = summary(pw_test)[[1]][["p-value"]],
#     pw_gh_test = summary(pw_gh_test)[[1]][["p-value"]]
#   )
#   return(results)
# }


