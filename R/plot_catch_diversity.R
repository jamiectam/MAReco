#' Plot Shannon diversity of catches
#'
#' @param data
#' @param x_column variable that is typically year
#' @param y_column variable that is the calculated shannon index
#' @param color_var character or factor variable
#' @param y_label character string in ""
#' @param legend_labels list of legend labels in ""
#'
#' @return a plot with smoothed lines and an averaged loess line
#' @export
#'
#' @examples
plot_catch_diversity <- function(data, x_column, y_column, color_var,
                                     y_label, legend_labels) {


  ggplot2::ggplot(data, aes(x = {{ x_column }}, y = {{ y_column }}, color = {{color_var}}))+
    theme_classic() +
    labs(y = y_label) +
    theme(
      # legend.position = legend_pos,#no legend_pos in function
      legend.text = element_text(size = 8),
      legend.title = element_blank()  # Remove legend title
    ) +
    scale_color_discrete(labels = legend_labels) +
    # geom_point(aes(color = {{color_var}})) +
    geom_smooth(method=loess, se=FALSE)+ #add loess here?
    geom_smooth(aes(x = {{ x_column }}, y = {{ y_column }}), method = "loess", color = "blue", se = TRUE)

}
