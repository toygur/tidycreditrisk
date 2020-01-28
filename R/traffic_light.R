#' Traffic Light Tests
#'
#' @description p-value based traffic light tests (binom, normal, and correlated tests)
#'
#' @param df A data.frame (tibble)
#' @param total_observations The column name of total observations variable. (character)
#' @param dr_observation The column name of bad observation rate variable. (character)
#' @param dr_estimate The column name of bad estimate rate variable. (character)
#' @param rho test correlation (numeric)
#' @param simplfy (logical, Default is TRUE)
#'
#' @import dplyr
#' @import tibble
#' @importFrom magrittr %<>%
#' @export
traffic_light_tests <- function(df, total_observations, dr_observation, dr_estimate, rho, simplfy=T) {

  #create column variable
  expr_total_obs <- sym(total_observations)
  expr_dr_obs <- sym(dr_observation)
  expr_dr_est <- sym(dr_estimate)

  #create calculation
  res <- df %>%
    mutate(COUNT_BAD_OBS = !!expr_dr_obs * !!expr_total_obs,
           BINOM_TEST = 1 - pbinom(size = !!expr_total_obs,
                                   prob = !!expr_dr_est,
                                   q = COUNT_BAD_OBS - 1),
           NORMAL_TEST = 1 - pnorm((COUNT_BAD_OBS - 0.5 - !!expr_dr_est * !!expr_total_obs) / sqrt(!!expr_dr_est * (1 - !!expr_dr_est) * !!expr_total_obs)),
           CORRELATED_TEST =  pnorm((qnorm(!!expr_dr_est) - qnorm(!!expr_dr_obs) * (1 - rho)^0.5) / rho^0.5))


  if(!simplfy){
    res %<>%
      mutate_at(vars(contains("TEST")), list(RESULT = ~case_when(. < 0.01  ~ "Red",
                                                                 . <= 0.05 ~ "Yellow",
                                                                 . > 0.05  ~ "Green")))
  }

  return(res)
}
