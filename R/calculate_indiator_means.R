

#' To calculate the mean and sd of the total timeseries for the indicator
#'
#' @param data is the ecosystem indicator data
#' @param area is the management area
#' @param ind is the column of the indicator
#'
#' @return
#' @export
#'
#' @examples
calculate_ind_longterm_mean <- function(data, area, ind) {
  data |>
    dplyr::filter(ID == area) |>
    dplyr::summarise(
      "mean_{{ind}}" := mean({{ind}}, na.rm=TRUE),
      "sd_{{ind}}" := sd({{ind}}, na.rm = TRUE)
    )
}

#' Calculates the mean and sd of a specific time period
#'
#' @param data
#' @param area
#' @param start_year
#' @param end_year
#' @param ind
#'
#' @return
#' @export
#'
#' @examples
calculate_ind_period_mean <-calculate_ind_longterm_mean <- function(data, area, start_year, end_year, ind) {
  data |>
    dplyr::filter(ID == area) |>
    dplyr::filter(YEAR >= start_year & YEAR <= end_year) |>
    dplyr::summarise(
      "mean_{{ind}}" := mean({{ind}}, na.rm=TRUE),
      "sd_{{ind}}" := sd ({{ind}}, na.rm=TRUE)
    )
}


