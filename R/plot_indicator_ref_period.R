#'plotting function for indicators with reference points for 30 years consistent with climate data
#'
#' @param data indicator data
#' @param x_column year column
#' @param y_column indicator value column
#' @param y_label indicator name as character string ""
#'
#' @return a line plot with points, mean+/-sd
#' @export
#'
#' @examples
plot_indicator_ref_period <- function(data, x_column, y_column, y_label) {
  # Filter data for the years 1991-2020
  data_1991_2020 <- data |>
    dplyr::filter({{ x_column }} >= 1991 & {{ x_column }} <= 2020)

  # Calculate mean and standard deviation for 1991-2020
  mean_value <- mean(data_1991_2020[[deparse(substitute(y_column))]], na.rm = TRUE)
  sd_value <- sd(data_1991_2020[[deparse(substitute(y_column))]], na.rm = TRUE)

  # Create the plot
  ggplot(data, aes(x = {{ x_column }}, y = {{ y_column }})) +
    geom_path(color = "black") +  # Line plot
    geom_hline(yintercept = mean_value, color = "darkgreen", linetype = "dashed", linewidth = 1) +  # Mean line
    geom_hline(yintercept = mean_value + sd_value, color = "darkgreen") +  # +1 SD
    geom_hline(yintercept = mean_value - sd_value, color = "darkgreen") +  # -1 SD
    labs(y = y_label) +
    annotate(geom = "rect",
             xmin = 1991, xmax = 2020,
             ymin = -Inf, ymax = Inf,
             fill = "purple2", alpha = 0.2)+
    # Highlight points outside ±1 SD
    geom_point(
      aes(color = ifelse({{ y_column }} > mean_value + sd_value, "above_sd",
                         ifelse({{ y_column }} < mean_value - sd_value, "below_sd", "within_sd"))),
      size = 2) +
    scale_color_manual(values = c("above_sd" = "orange", "below_sd" = "blue", "within_sd" = "black"),
                       guide = "none" )+
    theme_classic()
}
