#' Apply bins
#'
#' @description apply_bins is for applying bins rules.
#'
#' @param model ctree model object
#' @param df A data.frame (tibble) with independent variable.
#'
#' @import dplyr
#' @import tibble
#' @importFrom magrittr %<>%
#' @import partykit
#' @export
apply_bins <- function(model, df, y_target){

  #create new column name for indipendent variable
  temp_colname <- paste0(colnames(df)[!grepl(y_target,colnames(df))])

  if(is.null(model) || width(model) <2) {

    #if there are not any significant bins
    res <- df %>%
      select(!!y_target) %>%
      mutate(!!temp_colname := "No Significant Bins") %>%
      select(!!temp_colname,!!y_target)

  } else {

    #if there are significant bins

    #create rules
    temp_rules <- partykit:::.list.rules.party(model) %>% enframe(name = "NODE", value = "BINS") %>% mutate(NODE = as.integer(NODE))

    res <- df %>% filter_at(vars(-!!y_target), any_vars(!is.na(.))) %>%
      mutate(NODE = predict.party(model, newdata = ., type = "node"),
             NODE = as.integer(NODE)) %>%
      left_join(temp_rules, by="NODE") %>%
      select(!!y_target,BINS) %>%
      bind_rows(df %>% filter_at(vars(-!!y_target), any_vars(is.na(.))) %>% select(!!y_target)) %>%
      rename(!!temp_colname := BINS) %>%
      select(!!temp_colname,!!y_target)

  }

  return(res)
}
