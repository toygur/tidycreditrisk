#' Binomial Test
#'
#' @description Binomial test
#'
#' @param df A data.frame (tibble)
#' @param total_observations The column name of total observations variable. (character)
#' @param dr_observation The column name of bad observation rate variable. (character)
#' @param dr_estimate The column name of bad estimate rate variable. (character)
#' @param confidence_level Confidence level (Default is 0.95)
#' @param tail Test tail specification, could be one or two. (Default is one)
#'
#' @import dplyr
#' @import tibble
#' @importFrom magrittr %<>%
#' @export
binomial_test <- function(df, total_observations, dr_observation, dr_estimate, confidence_level = 0.95, tail = "one"){

  res <- NULL

  #create column variable
  expr_total_obs <- sym(total_observations)
  expr_dr_obs <- sym(dr_observation)
  expr_dr_est <- sym(dr_estimate)

  df %<>%
    mutate(BAD_OBSERVATION = !!expr_total_obs * !!expr_dr_obs,
           BAD_ESTIMATION = !!expr_total_obs * !!expr_dr_est)

  if(tail == "one"){

    res <- df %>%
      mutate(TEST_ESTIMATION = BAD_ESTIMATION + qnorm(confidence_level) * sqrt(BAD_ESTIMATION * (1 - !!expr_dr_est)),
             RESULT = case_when(BAD_OBSERVATION > TEST_ESTIMATION ~ "Target Value Underestimated",
                                                             TRUE ~ "Target Value Correct"))

  } else if(tail == "two"){

    res <- df %>%
      mutate(TEST_ESTIMATION_UPPER = BAD_ESTIMATION + qnorm(confidence_level) * sqrt(BAD_ESTIMATION * (1 - !!expr_dr_est)),
             TEST_ESTIMATION_LOWER = BAD_ESTIMATION - qnorm(confidence_level) * sqrt(BAD_ESTIMATION * (1 - !!expr_dr_est)),
             RESULT = case_when(BAD_OBSERVATION > TEST_ESTIMATION_UPPER ~ "Target Value Underestimated",
                                BAD_OBSERVATION < TEST_ESTIMATION_LOWER ~ "Target Value Overestimated",
                                                                   TRUE ~ "Target Value Correct"))
  }

  return(res)
}

