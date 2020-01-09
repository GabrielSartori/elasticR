#' Agreggating count by a specific variable
#'
#' @param credential the url create by function es.credential
#' @param size limit rows to extract
#' @param variable a string variable, must be a factor variable
#' @param arrange_count order results by a variable or count. Default T, which mean by count.
#'
#' @return a data frame contains a specific variables
#' @export
#' @examples
#'  es.catsum(
#'   my_credential,
#'   "factor1",
#'   10,
#'   T
#'  )
#'
#'
es.catcount <- function(credential, variable, size, arrange_count = T){

  query <-
    paste0('{
    "size": 0,
    "aggs" : {
    "variable" : {
      "terms" : { "field" : "',variable, '",
        "size":', size, '}
        }
      }
    }'
    )

  list_cont <- extract.query(es_credential = credential,
                             es_query = query)


  if(isTRUE(arrange_count)){

    result_df <-
      list_cont %>%
      .$aggregations %>%
      .$variable %>%
      .$buckets %>%
      plyr::rename(c("key" = variable,
                     "doc_count" = "count"))
  }else{

    result_df <-
      list_cont %>%
      .$aggregations %>%
      .$variable %>%
      .$buckets %>%
      arrange(key) %>%
      plyr::rename(c("key" = variable,
                     "doc_count" = "count"))

  }
  return(result_df)
}
