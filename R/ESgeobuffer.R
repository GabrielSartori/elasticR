#' Spatial query by a lat/long with buffer defined
#'
#' @param credential the url create by function es.credential
#' @param variable a vector of variable
#' @param size limit rows to extract
#' @param bufffer radius area (in km)
#' @param longitude longitude
#' @param latitude latitude
#' @return data frame
#' @export
#' @examples
#' es.buffer(
#'  my_credential,
#' c("var1", "var2"),
#' 100
#' "1km",
#' longitude,
#' latitude
#'  )
#'


es.buffer <-
  function(
    credential,
    variable,
    size,
    buffer,
    longitude,
    latitude
    ){

    v_paste_variable <-
      variable %>%
      paste0(collapse = '", "') %>%
      paste0('"', ., '"')

    es_query <-
      paste0('{
    "size":', size,',
    "_source": [', v_paste_variable,'],
    "query": {
      "bool": {
        "must": [
          {
            "geo_distance": {
              "distance": "',buffer,'",
              "geohash": [',longitude,', ',latitude,']
            }
          }
        ]
      }
    },
    "sort": [
      {
        "_geo_distance": {
          "geohash": [',longitude,', ',latitude,'],
          "order": "asc",
          "unit": "km",
          "distance_type": "plane"
        }
      }
    ]
  }'
      )

    es_output <-
      httr::POST(
        url = credential,
        body = es_query,
        encode = "json", # this lets' httr do the work for you
        httr::timeout(10000),
        httr::content_type_json(), # easier than making a header yourself
        httr::verbose()
      )

    l_es_outuput <- httr::content(es_output, as = "text")
    l_es_outuput <- jsonlite::fromJSON(l_es_outuput)
    df_es <- l_es_outuput$hits$hits$`_source`
    df_es$id_es <- l_es_outuput$hits$hits$`_id`
    df_es$dist <- unlist(l_es_outuput$hits$hits$sort)

    return(df_es)
  }
