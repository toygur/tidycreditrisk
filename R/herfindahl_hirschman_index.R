#' Herfindahl Hirschman Index
#'
#' @description Herfindahl Hirschman Index
#'
#' @param df A data.frame (tibble)
#' @param total_observations The column name of total observations variable. (character)
#' @param trace show total HHI score (logical, Default is FALSE)
#' @param simplfy if is TRUE, return only tibble,
#' otherwise return list (logical, Default is TRUE)
#'
#' @import dplyr
#' @import tibble
#' @importFrom magrittr %<>%
#' @export
herfindahl_hirschman_index <- function(df, total_observations, trace=F, simplfy=T){

  #create column variable
  expr <- sym(total_observations)

  #crate calculation
  res_df <- df %>%
    mutate(CONCENTRATION = !!expr / sum(!!expr),
           HHI = CONCENTRATION^2)

  #calculate overall hhi
  res_hhi <- res_df %>% summarise(HHI = sum(HHI))

  #export trace object
  if(trace) cat(paste0("HHI: ", res_hhi,"\n"))

  #export result
  if(simplfy){
    res <- res_df
  } else {
    res <- list(HHI_DETAIL = res_df, HHI = res_hhi)

  }

  return(res)
}


#' Herfindahl Hirschman Index Table
#'
#' @description Herfindahl Hirschman Index table
#'
#' @param df Frequency tables for variables (nested tibble)
#' @param df_column Nested tibble column name (Default is DATA)
#' @param total_observations The column name of total observations variable. (character)
#' @param simplfy if is TRUE, return only HHI value, otherwise detail.
#'
#' @import dplyr
#' @import tibble
#' @importFrom purrr map map_dbl
#' @importFrom tidyr unnest_wider
#' @importFrom magrittr %<>%
#' @export
herfindahl_hirschman_index_table <- function(df, df_column="DATA", total_observations, simplfy=T){

  #create column variable
  expr_df <- sym(df_column)
  expr <- sym(total_observations)

  #calculate HHI index for all variables
  res <- df %>%
    mutate(HHI = map(!!expr_df, ~herfindahl_hirschman_index(.x, total_observations=total_observations, trace = F, simplfy = F))) %>%
    unnest_wider(HHI) %>%
    mutate(HHI = map_dbl(HHI, ~pull(.x)),
           RESULT = case_when(HHI < 0.20  ~ "No Concentration",
                              HHI <= 0.30 ~ "Low Concentration",
                              TRUE        ~ "High Concentration"))

  if(simplfy) res %<>% select(-HHI_DETAIL)

  return(res)
}




