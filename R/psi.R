#' Population Stability Index
#'
#' @description Population stability index (PSI)
#'
#' @param df A data.frame (tibble)
#' @param count_observed The column name of count observation variable. (character)
#' @param count_estimated The column name of count estimate variable. (character)
#' @param trace show total PSI index (logical, Default is FALSE)
#' @param simplfy if is TRUE, return only tibble,
#' otherwise return list (logical, Default is TRUE)
#'
#' @import dplyr
#' @import tibble
#' @importFrom magrittr %<>%
#' @export
psi <- function(df, count_observed, count_estimated, trace=F, simplfy=T){

  #create column variable
  expr_count_obs <- sym(count_observed)
  expr_count_est <- sym(count_estimated)

  #crate calculation
  res_df <- df %>%
    mutate(RATE_OBSERVED = !!expr_count_obs / sum(!!expr_count_obs, na.rm = T),
           RATE_ESTIMATED = !!expr_count_est / sum(!!expr_count_est, na.rm = T),
           INDEX = (RATE_OBSERVED - RATE_ESTIMATED) * log(RATE_OBSERVED/RATE_ESTIMATED))

  #calculate overall hhi
  res_psi <- res_df %>% summarise(PSI = sum(INDEX))

  #export trace object
  if(trace) cat(paste0("PSI: ", res_psi,"\n"))

  #export result
  if(simplfy){
    res <- res_df
  } else {
    res <- list(PSI_DETAIL = res_df, PSI = res_psi)

  }

  return(res)
}


#' Population Stability Index Table
#'
#' @description Population Stability Index table
#'
#' @param df Frequency tables for variables (nested tibble)
#' @param df_column Nested tibble column name (Default is DATA)
#' @param count_observed The column name of count observation variable. (character)
#' @param count_estimated The column name of count estimate variable. (character)
#' @param simplfy if is TRUE, return only PSI value, otherwise detail.
#'
#' @import dplyr
#' @import tibble
#' @importFrom purrr map map_dbl
#' @importFrom tidyr unnest_wider
#' @importFrom magrittr %<>%
#' @export
psi_table <- function(df, df_column="DATA", count_observed, count_estimated, simplfy=T){

  #create column variable
  expr_df <- sym(df_column)
  expr_count_obs <- sym(count_observed)
  expr_count_est <- sym(count_estimated)

  #calculate PSI index for all variables
  res <- df %>%
    mutate(PSI = map(!!expr_df, ~psi(.x, count_observed=count_observed, count_estimated=count_estimated, trace = F, simplfy = F))) %>%
    unnest_wider(PSI) %>%
    mutate(PSI = map_dbl(PSI, ~pull(.x)),
           RESULT = case_when(PSI < 0.1     ~ "Insignificant change in population",
                              PSI <= 0.25   ~ "Minor shift in population",
                              PSI > 0.25    ~ "Major shift in population"))

  if(simplfy) res %<>% select(-PSI_DETAIL)

  return(res)
}
