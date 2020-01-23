#' Chi-Squared Test
#'
#' @description Chi-Squared test
#'
#' @param df A data.frame (tibble)
#' @param total_observations The column name of total observations variable. (character)
#' @param dr_observation The column name of bad observation variable. (character)
#' @param dr_estimate The column name of bad estimate variable. (character)
#' @param confidence_level Confidence level (Default is 0.95)
#' @param simplfy (logical, Default is TRUE)
#'
#' @details null hypothesis is the observed and predicted values are close the each other.
#'
#' @import dplyr
#' @import tibble
#' @export
chisquare_test <- function(df, total_observations, dr_observation, dr_estimate, confidence_level = 0.95, simplfy=T){

  #create column variable
  expr_total_obs <- sym(total_observations)
  expr_dr_obs <- sym(dr_observation)
  expr_dr_est <- sym(dr_estimate)

  #definitions
  alpha <- 1-confidence_level

  #crate calculation
  res_df <- df %>%
    mutate(BAD_OBSERVATION = !!expr_total_obs * !!expr_dr_obs,
           BAD_ESTIMATION = !!expr_total_obs * !!expr_dr_est,
           CHI_SQUARE = ((BAD_ESTIMATION-BAD_OBSERVATION)^2) / BAD_ESTIMATION)

  res_chisquare <- res_df %>%
    summarise(CHI_SQUARE = sum(CHI_SQUARE)) %>%
    mutate(PVALUE = pchisq(CHI_SQUARE, df=nrow(df)-1, lower.tail = F),
           RESULT = case_when(PVALUE > alpha ~ "The scale passed the test",
                              TRUE           ~ "The scale did not pass the test"))

 if(simplfy){
   res <- res_chisquare
 } else {
   res <- list(CHISQUARE_DETAIL = res_df, CHISQUARE = res_chisquare)
 }

  return(res)
}
