#' plotting function for NAO
#'
#' @param data nao data derived from .dat files
#' @param x_column year column
#' @param y_column nao values
#' @param mean_value mean derived from nao list in dd
#' @param sd_value sd derived from nao list in dd
#' @param y_label label for nao
#'
#' @return line plot with NAO mean and sd
#' @export
#'
#' @examples
plot_nao <- function(data, x_column, y_column, mean_value, sd_value, y_label) {

  ggplot(data, aes(x = {{ x_column }}, y = {{ y_column }})) +
    geom_path(color = "black") +  # Line plot
    geom_hline(yintercept = mean_value, color = "darkgreen", linetype = "dashed") +  # Mean line
    geom_hline(yintercept = mean_value + sd_value, color = "darkgreen") +  # +1 SD
    geom_hline(yintercept = mean_value - sd_value, color = "darkgreen") +  # -1 SD
    labs(y = y_label) +
    geom_point(shape = 1)+

    theme_classic()
}
