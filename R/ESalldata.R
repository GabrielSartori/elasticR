#'  Get data limit by size
#'
#'  @param credential the url create by function es.credential
#'  @param size limit rowsa extracted
#'
#'  @return list with status about connection response
#'  @export
#'  @examples
#'  es.alldata(
#'  credential = credential,
#'  size = 100)
#'

es.alldata <- function(credential, size){


  query <-
    paste0('{
    "size": ', size, ',
    "query": {
      "match_all": {}
    }
  }
}')

  list_cont <- extract.query(es_credential = credential,
                             es_query = query)

  data_extract <- list_cont$hits$hits$`_source`
  return(data_extract)

}
