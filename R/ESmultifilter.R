#' Filtering dataset by a multiple variables and answer. The filter works like a "and operator"
#'
#' @param credential the url create by function es.credential
#' @param variable a vector of variable
#' @param size limit rows to extract
#' @param filter list of filters

#' @return a data frame with specific filter
#' @export
#' @examples
#'  es.filterun(
#'   my_credential,
#'   c("var1", "var2"),
#'   100,
#'   list ("var1" = c("answer1", "answer2"),
#'          "var2" = c("answer1"))
#'
#'  )

es.filtermult <-
  function(credential, variable, size, es_filter){

    # paste variable
    v_paste_variable <-
      variable %>%
      paste0(collapse = '", "') %>%
      paste0('"', ., '"')

 # loop filter
    loop_terms_bracket <-
      function(variable_filter, variable_resp){
        paste0(
          '{"terms": {"',variable_filter,'":["', variable_resp, '"]}}')
      }

    paste_variable <-
      es_filter %>%
      purrr::map(~paste(.x, collapse = '","'))


    loop_query_multiple <-
      purrr::map2(.x = names(es_filter),
           .y = paste_variable,
           .f = ~loop_terms_bracket(.x, .y))  %>%
      purrr::flatten_chr() %>%
      paste0(collapse = ",")



    query <- paste0(
      '{
  "size": ', size, ',
  "_source": [', v_paste_variable,'],
  "query": {
    "bool": {
      "must": [',loop_query_multiple, '
      ]
    }
  }
}'
    )

    list_cont <- extract.query(es_credential = credential,
                               es_query = query)

    data_extract <- list_cont$hits$hits$`_source`
    return(data_extract)
  }
