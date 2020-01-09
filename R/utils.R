# build_query -------------------------------------------------------------

extract.query <- function(es_credential,
                          es_query){

  # es_query = query
  es_request <-
    httr::POST(
      url = es_credential,
      body = es_query,
      httr::timeout(10000),
      encode = "raw", # this lets' httr do the work for you
      httr::content_type_json(), # easier than making a header yourself
      httr::verbose()
    )

  list_cont <- httr::content(es_request, as = "text")
  es_data <- jsonlite::fromJSON(list_cont)

return(es_data)


}
