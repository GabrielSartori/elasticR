#' Agreggating descritive statics by a specific variable
#'
#' @param credential the url create by function es.credential
#' @param variable a string variable, must be a numeric variable
#'
#' @return a data frame with statics measures, mean, min and percentile
#' @export
#' @examples
#'  es.summary(
#'   my_credential,
#'   "numeric_variable"
#'  )
#'
es.summary <-  function(credential, variable){

  # variavel = "valor"
  query <-
    paste0(
  '{
  "_source": [
     "', variable, '"
  ],
  "size": 0,
  "aggs": {
    "grades_stats": {
      "stats": {
        "field":  "',variable, '"
      }
    },
    "percentile": {
      "percentiles": {
        "field":  "',variable, '"
      }
    }
  }
}'
)


  list_cont <- extract.query(es_credential = credential,
                             es_query = query)

  # extract stats
  stats <-
    list_cont %>%
    .$aggregations %>%
    .$grades_stats %>%
    data.frame()

  # extract percentile
  pct <-
    list_cont %>%
    .$aggregations %>%
    .$percentile %>%
    data.frame() %>%
    plyr::rename(c("values.1.0" = "pct_1",
                   "values.5.0" = "pct_5",
                   "values.25.0" =  "pct_25",
                   "values.50.0" =  "pct_50",
                   "values.75.0" = "pct_75",
                   "values.95.0" = "pct_95",
                   "values.99.0" =  "pct_99"))


  result_df <- bind_cols("variable" = variable, pct, stats)

  return(result_df)
}
