#' Calculate Frequency Table
#'
#' @description Calculate frequency table
#'
#' @param df A data.frame (tibble) with independent variables and target variable.
#' @param x_target Names of independent variables. (Default is NULL)
#' @param y_target The name of target variable. (Default is NULL)
#'
#' @details
#' \itemize{
#'  \item {if y_target is not NULL, it can be factor or character.}
#'  \item {if y_target is not NULL, split_bins() are apllied the following variables: integer, double or numeric.}
#'  \item {if y_target is NULL, only factor or character variables are considered.}
#'  }
#'
#'
#' @import dplyr
#' @import  tibble
#' @importFrom  purrr map map2 map_lgl map_chr
#' @importFrom magrittr %<>%
#' @export
get_frequency_table <- function(df, x_target=NULL, y_target=NULL){

  res <- NULL

  if(is.null(y_target)){
    if(!is.null(x_target)){
      df %<>% select(x_target)
    }
  } else {
    if(!is.null(x_target)){
      df %<>% select(y_target, x_target)
    }
  }

  if(nrow(df)<1) return(res)

  temp_df <- df %>%
    mutate_if(is.factor, as.character) %>%
    as.list %>%
    enframe(name = "KEY", value = "DATA") %>%
    mutate(DATA = map2(DATA,KEY, ~setNames(list(.x), .y) %>% as_tibble ),
           CLASS = map_chr(DATA, ~pull(.x) %>% class),
           DATA = map(DATA, ~mutate(.x, ID = row_number())))

  if(is.null(y_target)) {

    res <- temp_df %>%
      filter(CLASS == "character") %>%
      mutate(DATA = map(DATA, ~select(.x, -ID) %>% group_by_all %>% summarise(OBSERVATION = n()) %>% mutate(TOTAL_OBSERVATION = sum(OBSERVATION)))) %>% select(-CLASS)

  } else {

    df_y_target <- temp_df %>% filter(KEY == y_target)
    df_x_target <- temp_df %>% filter(KEY != y_target)

    if(nrow(df_y_target)<1 || nrow(df_x_target)<1) return(res)
    if(df_y_target$CLASS != "character") return(res)

    res <- df_x_target %>%
      mutate(DATA_TARGET = df_y_target$DATA,
             DATA = map2(DATA, DATA_TARGET, ~left_join(.x,.y, by="ID") %>% select(-ID))) %>%
      select(-DATA_TARGET) %>%
      mutate(BINS = case_when(CLASS %in% c("integer","double","numeric")  ~ map(DATA, ~split_bins(.x, y_target = y_target, simplfy = F))),
             RULES = case_when(CLASS %in% c("integer","double","numeric") ~ map(BINS, ~.x[["model"]])),
             DATA = case_when(CLASS %in% c("integer","double","numeric")  ~ map(BINS, ~.x[["df"]]),
                              TRUE ~ DATA)) %>%
      select(-BINS) %>%
      filter(map_lgl(DATA, ~!is.null(.x))) %>%
      mutate(DATA = map(DATA, ~group_by_all(.x) %>% summarise(OBSERVATION = n()) %>% mutate(SUBTOTAL_OBSERVATION = sum(OBSERVATION)) %>% ungroup %>% mutate(TOTAL_OBSERVATION = sum(OBSERVATION)) ))
  }

  return(res)
}
