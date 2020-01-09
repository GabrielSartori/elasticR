#' Filtering dataset by a specific variable
#'
#' @param credential the url create by function es.credential
#' @param variable a vector of variable,
#' @param size limit rows to extract
#' @param filter variable filter. Only one variable
#' @param answer variable answer. Only one answer

#' @return a data frame with specific
#' @export
#' @examples
#'  es.filterun(
#'   my_credential,
#'   c("var1", "var2", "var3"),
#'   100,
#'   "var1",
#'   "answer1"
#'  )
#'

es.filterun <- function(credential, variable, size,
                        filter,
                        answer
                        ){

  v_paste_variable <-
    variable %>%
    paste0(collapse = '", "') %>%
    paste0('"', ., '"')

  query <-
    paste0('{
    "_source": [', v_paste_variable,'],
    "size": ', size, ',
    "query": {
     "bool": {
      "filter": {
        "term": {"', filter,'":"', answer, '"
        }
      }
    }
  }
}'
    )
  list_cont <- extract.query(es_credential = credential,
                             es_query = query)

  data_extract <- list_cont$hits$hits$`_source`

  return(data_extract)

}



