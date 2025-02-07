#' Plot multiple indicator time series that fit into themes
#'
#' @param data is a data frame with YEAR, value, name
#' @param x_column is the YEAR column
#' @param y_column is the value column
#' @param color_var is the name column
#' @param y_label is a character string in ""
#' @param legend_labels is a group of character strings in ""
#'
#' @return a plot
#' @export
#'
#' @examples
plot_multiple_indicators <- function(data, x_column, y_column, color_var,
                          y_label, legend_labels) {

  end_year <- max(data[[deparse(substitute(x_column))]], na.rm = TRUE)
  start_year <- end_year - 4  # Last 5 years
  last_five <- data |>
    dplyr::filter({{ x_column }} >= start_year)

ggplot2::ggplot(data, aes(x = {{ x_column }}, y = {{ y_column }}, color = {{color_var}}))+
    theme_classic() +
    annotate(geom = "rect",
             xmin = start_year, xmax = end_year,
             ymin = -Inf, ymax = Inf,
             fill = "purple2", alpha = 0.2)+
    labs(y = y_label) +
  theme(
    # legend.position = legend_pos,#no legend_pos in function
    legend.text = element_text(size = 8),
    legend.title = element_blank()  # Remove legend title
  ) +
    scale_color_discrete(labels = legend_labels) +
    geom_point(aes(color = {{color_var}})) +
    geom_path(aes(color = {{color_var}}))

}

