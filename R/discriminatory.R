#' Discriminatory tests
#'
#' @description Discriminatory tests (gini and ks)
#'
#' @param df A data.frame (tibble)
#' @param total_observations The column name of total observations variable. (character)
#' @param dr_observation The column name of bad observation rate variable. (character)
#' @param dr_estimate The column name of bad estimate rate variable. (character)
#' @param simplfy (logical, Default is TRUE)
#'
#' @import dplyr
#' @import tibble
#' @importFrom magrittr %<>%
#' @export
discriminatory_tests <- function(df, total_observations, dr_observation, dr_estimate, simplfy=T) {

  #create column variable
  expr_total_obs <- sym(total_observations)
  expr_dr_obs <- sym(dr_observation)
  expr_dr_est <- sym(dr_estimate)

  #crate calculation
  res_df <- df %>%
    mutate(BAD_OBSERVATION = !!expr_total_obs * !!expr_dr_obs,
           GOOD_OBSERVATION = !!expr_total_obs - BAD_OBSERVATION,
           BAD_RATE_OBS = BAD_OBSERVATION / sum(BAD_OBSERVATION, na.rm = T),
           GOOD_RATE_OBS = GOOD_OBSERVATION / sum(GOOD_OBSERVATION, na.rm = T),
           CUM_BAD_RATE_OBS = cumsum(BAD_RATE_OBS),
           CUM_GOOD_RATE_OBS = cumsum(GOOD_RATE_OBS),
           KS = CUM_GOOD_RATE_OBS - CUM_BAD_RATE_OBS,
           GINI = case_when(row_number() == 1 ~ CUM_BAD_RATE_OBS * CUM_GOOD_RATE_OBS,
                            TRUE              ~ (CUM_BAD_RATE_OBS+lag(CUM_BAD_RATE_OBS)) * (CUM_GOOD_RATE_OBS-lag(CUM_GOOD_RATE_OBS)) ) )

  res_discriminatory <- res_df %>%
    summarise(KS = max(KS),
              GINI = 1-sum(GINI)) %>%
    mutate(KS_RESULT = case_when(KS < 0.35  ~ "Reject",
                                 TRUE       ~ "Accept"),
           GINI_RESULT = case_when(GINI < 0.40  ~ "Weak",
                                   GINI < 0.60  ~ "Medium",
                                   GINI < 0.80  ~ "Good",
                                   TRUE         ~ "Very Good"))


  if(simplfy){
    res <- res_discriminatory
  } else {
    res <- list(DISCRIMINATORY_DETAIL = res_df, DISCRIMINATORY = res_discriminatory)
  }

  return(res)
}
