#' Select data by specific variables
#'
#' @param credential the url create by function es.credential
#' @param size limit rows to extract
#' @param variable a vector of specific variables
#'
#' @return a data frame contains a specific variables
#' @export
#' @examples
#'  es.columndata(
#'   my_credential,
#'   100,
#'   c("atributo1", "atributo2" )
#'  )

es.columndata <- function(credential, size, variable){

  v_paste_variable <-
    variable %>%
    paste0(collapse = '", "') %>%
    paste0('"', ., '"')

  query <-
    paste0('{
      "size": ', size, ',
      "_source": [', v_paste_variable,'],
      "query": {
      "match_all": {}
      }
      }')

  list_cont <- extract.query(es_credential = credential,
                             es_query = query)

  data_extract <- list_cont$hits$hits$`_source`

  return(data_extract)

}
