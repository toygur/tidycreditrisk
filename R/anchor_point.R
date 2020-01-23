#' Anchor Point Test
#'
#' @description Anchor point test
#'
#' @param df A data.frame (tibble)
#' @param total_observations The column name of total observations variable. (character)
#' @param dr_estimate The column name of bad estimate variable. (character)
#' @param central_tendency A central tendency needs to be specified for testing.
#' @param upper_green (Default is 1.2)
#' @param upper_red (Default is 1.3)
#' @param lower_green (Default is 0.8)
#' @param lower_red (Default is 0.7)
#'
#' @import dplyr
#' @import tibble
#' @export
anchor_point <- function(df, total_observations, dr_estimate, central_tendency, upper_green = 1.2, upper_red = 1.3, lower_green = 0.8, lower_red = 0.7){

  expr_total_obs <- sym(total_observations)
  expr_dr_est <- sym(dr_estimate)

  res <- df %>%
    summarise(AVG_DR = sum(!!expr_dr_est * !!expr_total_obs) / sum(!!expr_total_obs)) %>%
    mutate(CENTRAL_TENDENCY = central_tendency,
           UPPER_GREEN = AVG_DR * upper_green,
           UPPER_RED = AVG_DR * upper_red,
           LOWER_GREEN = AVG_DR * lower_green,
           LOWER_RED = AVG_DR * lower_red,
           RESULT = case_when(CENTRAL_TENDENCY > LOWER_GREEN & CENTRAL_TENDENCY < UPPER_GREEN ~ "Green",
                              CENTRAL_TENDENCY < LOWER_RED | CENTRAL_TENDENCY > UPPER_RED     ~ "Red",
                                                                                         TRUE ~ "Yellow"))

  return(res)
}
