#' Plotting function for time series indicators for Ecosystem Summary Reports
#'
#' @param data is a data frame with YEAR and Indicator value
#' @param x_column YEAR column
#' @param y_column Indicator column
#' @param y_label character string ""
#'
#' @return a line plot with points, mean+/-sd
#' @export
#' @author Jamie C. Tam
#' @examples
#' \dontrun{ TODO
#' }
#'
plot_indicator <- function(data, x_column, y_column, y_label) {
  # Calculate mean and standard deviation
  mean_value <- mean(data[[deparse(substitute(y_column))]], na.rm = TRUE)
  sd_value <- sd(data[[deparse(substitute(y_column))]], na.rm = TRUE)

  # Determine the last 5 years in the data
  end_year <- max(data[[deparse(substitute(x_column))]], na.rm = TRUE)
  start_year <- end_year - 4  # Last 5 years
  last_five <- data |>
    dplyr::filter({{ x_column }} >= start_year)

  # Create the plot
  ggplot(data, aes(x = {{ x_column }}, y = {{ y_column }})) +
    geom_path(color = "black") +  # Line plot
    geom_hline(yintercept = mean_value, color = "darkgreen", linetype = "dashed", size = 1) +  # Mean line
    geom_hline(yintercept = mean_value + sd_value, color = "darkgreen") +  # +1 SD
    geom_hline(yintercept = mean_value - sd_value, color = "darkgreen") +  # -1 SD
    labs(y = y_label) +
    annotate(geom = "rect",
             xmin = start_year, xmax = end_year,
             ymin = -Inf, ymax = Inf,
             fill = "purple2", alpha = 0.2)+
        # Highlight points outside ±1 SD
    geom_point(
      aes(color = ifelse({{ y_column }} > mean_value + sd_value, "above_sd",
                         ifelse({{ y_column }} < mean_value - sd_value, "below_sd", "within_sd"))),
      size = 2) +
    scale_color_manual(values = c("above_sd" = "orange", "below_sd" = "blue", "within_sd" = "black"),
      guide = "none" )+

    # GLM trend line for the last 5 years
    # geom_smooth(data = last_five, aes(x = {{ x_column }}, y = {{ y_column }}), method = "glm", color = "red", se = FALSE) +


    theme_classic()
}




