#' Plotting temperature anomalies with rigime shifts
#'
#' @param data temperature data derived from .dat files
#' @param x_column year column
#' @param y_column temperature anomaly
#' @param mean_value mean set at 0
#' @param sd_value sd value set at 0.5
#' @param ff regime shift analysis using forward routine
#' @param bb regime shift analysis using backwards routime
#' @param y_label
#'
#' @return plot with mean, sd, regime shift lines
#' @export
#'
#' @examples
plot_temperature_anomaly <- function(data, x_column, y_column, mean_value, sd_value, ff, bb, y_label) {

  ggplot(data, aes(x = {{ x_column }}, y = {{ y_column }})) +
    geom_path(color = "black") +  # Line plot
    geom_hline(yintercept = mean_value, color = "darkgreen", linetype = "dashed") +  # Mean line
    geom_hline(yintercept = mean_value + sd_value, color = "darkgreen") +  # +1 SD
    geom_hline(yintercept = mean_value - sd_value, color = "darkgreen") +  # -1 SD
    labs(y = y_label) +
    geom_line(data = ff, aes(x = year, y = regimeMean), color = 'red', linewidth=1)+
    geom_line(data = bb, aes(x=year, y=regimeMean), color = 'blue', linewidth=1)+
    # Highlight points outside ±1 SD
    geom_point(shape = 1)+

    theme_classic()
}
