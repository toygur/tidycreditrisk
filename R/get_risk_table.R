#' Calculate Risk Table
#'
#' @description Calculate risk table
#'
#' @param df_master A master data.frame (tibble) with independent variables and target variable.
#' @param df_new A new data.frame (tibble) with independent variables and target variable.
#' @param y_target The name of target variable.
#' @param x_target Names of independent variables. (Default is NULL)
#' @param default Default flag in the target variable (Default is NULL)
#'
#'
#' @import dplyr
#' @import  tibble
#' @importFrom  purrr map map2 map_lgl map_chr pmap
#' @importFrom magrittr %<>%
#' @export
get_risk_table <- function(df_master, df_new, y_target, x_target=NULL, default=NULL){


  if(!is.null(x_target)){
    df_master %<>% select(y_target, x_target)
    df_new %<>% select(y_target, x_target)
  }

  #master data frequency table
  temp_freq_master <- get_frequency_table(df_master, y_target = y_target, x_target = x_target)

  #new data
  temp_df_new <- df_new %>%
    mutate_if(is.factor, as.character) %>%
    as.list %>%
    enframe(name = "KEY", value = "DATA_NEW") %>%
    mutate(DATA_NEW = map2(DATA_NEW,KEY, ~setNames(list(.x), .y) %>% as_tibble ),
           CLASS = map_chr(DATA_NEW, ~pull(.x) %>% class),
           DATA_NEW = map(DATA_NEW, ~mutate(.x, ID = row_number())))


  temp_df_y_new <- temp_df_new %>% filter(KEY == y_target)
  temp_df_x_new <- temp_df_new %>% filter(KEY != y_target)

  temp_df_x_new %<>%
    mutate(DATA_TARGET = temp_df_y_new$DATA_NEW,
           DATA_NEW = map2(DATA_NEW, DATA_TARGET, ~left_join(.x,.y, by="ID") %>% select(-ID))) %>%
    select(-DATA_TARGET)

  #merge datasets
  res <- temp_freq_master %>%
    mutate(MASTER_SCALE = map(DATA, ~select(.x,1) %>% distinct %>% pull)) %>%
    left_join(temp_df_x_new, by=c("KEY","CLASS")) %>%
    mutate(DATA_NEW = case_when(CLASS %in% c("integer","double","numeric") ~ map2(RULES,DATA_NEW, ~apply_bins(.x,.y, y_target = y_target)),
                                TRUE ~ DATA_NEW),
           NEW_SCALE = map(DATA_NEW, ~select(.x,1) %>% distinct %>% pull)) %>%
    filter(map2_lgl(MASTER_SCALE,NEW_SCALE, ~length(setdiff(.x,.y))==0)) %>%
    select(-contains("SCALE")) %>%
    mutate(DATA_NEW = map(DATA_NEW, ~group_by_all(.x) %>% summarise(OBSERVATION = n()) %>% mutate(SUBTOTAL_OBSERVATION = sum(OBSERVATION)) %>% ungroup %>% mutate(TOTAL_OBSERVATION = sum(OBSERVATION)) )) %>%
    select(KEY,CLASS,DATA,DATA_NEW) %>%
    mutate(RISK_DATA = pmap(list(KEY,DATA,DATA_NEW),
                            function(x,y,z) {
                              if(is.null(default)){
                                left_join(x = y,
                                          y = z %>% rename_at(vars(contains("OBSERVATION")), ~paste0(.,"_NEW")),
                                          by = c(x,y_target))
                              } else {
                                left_join(x = y %>% filter(!!sym(y_target) == default),
                                          y = z %>% filter(!!sym(y_target) == default) %>% rename_at(vars(contains("OBSERVATION")), ~paste0(.,"_NEW")),
                                          by = c(x,y_target)) %>%
                                  select(-!!y_target)
                              }
                            } )) %>%
    select(KEY,CLASS,RISK_DATA)

  return(res)
}
