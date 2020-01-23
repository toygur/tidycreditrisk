#' Split bins
#'
#' @description split_bins is for binning using breaks.
#'
#' @param df A data.frame (tibble) with independent and target variable.
#' @param y_target The name of target variable. (factor or character)
#' @param p probability for minimum bucket (Default is 0.05)
#' @param simplfy (Default is TRUE)
#'
#' @import dplyr
#' @import tibble
#' @importFrom magrittr %<>%
#' @import partykit
#' @export
split_bins <- function(df, y_target, p=0.05, simplfy=T){

  #for ctree calculation
  df %<>% mutate_if(is.character, as.factor)

  #apply decision tree
  temp_ctree <- tryCatch({
    ctree(formula(paste(y_target, "~ .")),
          data = df,
          na.action = na.exclude,
          control = ctree_control(minbucket = ceiling(round(p * nrow(df)))))
  }, error = function(e) return(NULL))

  #create new column name for indipendent variable
  temp_colname <- paste0(colnames(df)[!grepl(y_target,colnames(df))])

  if(is.null(temp_ctree) || width(temp_ctree) <2) {

    #if there are not any significant bins
    res <- df %>%
      select(!!y_target) %>%
      mutate(!!temp_colname := "No Significant Bins") %>%
      mutate_if(is.factor, as.character) %>%
      select(!!temp_colname,!!y_target,everything())

  } else {

    #if there are significant bins

    #create rules
    temp_rules <- partykit:::.list.rules.party(temp_ctree) %>% enframe(name = "NODE", value = "BINS") %>% mutate(NODE = as.integer(NODE))

    #apply rules and create result object
    res <- data_party(temp_ctree) %>% as_tibble %>% rename(NODE = `(fitted)`) %>% select(1:3) %>%
      left_join(temp_rules, by="NODE") %>%
      select(!!y_target,BINS) %>%
      bind_rows(df %>% filter_at(vars(-!!y_target), any_vars(is.na(.))) %>% select(!!y_target)) %>%
      rename(!!temp_colname := BINS) %>%
      mutate_if(is.factor, as.character) %>%
      select(!!temp_colname,!!y_target,everything())
  }

  if(simplfy) {
    res <- res
  } else {
    res <- list(model = temp_ctree, df=res)
  }

  return(res)
}






