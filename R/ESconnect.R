#'  Testing url connection to database with credential
#'
#' @param credential the url create by function es.credential
#'
#'  @return list with status about connection response
#'  @export
#'  @examples
#'  es.connection(credential)
#'  )
#'
es.connection <- function(credential){

  res_conf <-
    httr::POST(
      url = credential,
      httr::timeout(10000),
      encode = "json",
      httr::content_type_json()
    )

  return(
    list(
      "url" = res_conf$url,
      "status" = res_conf$status_code,
      "headers" = res_conf$headers
    )
  )
}
