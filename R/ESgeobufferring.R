#' Spatial query by a lat/long between a buffer and a ring
#'
#' @param credential the url create by function es.credential
#' @param variable a vector of variable
#' @param size limit rows to extract
#' @param bufffer radius area (in km)
#' @param ring radius area (in km). ALways plus than buffer
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
#' "3km",
#' longitude,
#' latitude
#'  )
#'

es.buffer.ring <-
  function(credential, variable, size, latitude, longitude, buffer, ring){

    v_paste_variable <-
      variable %>%
      paste0(collapse = '", "') %>%
      paste0('"', ., '"')

    query_ring  <-
      paste0('{
 "size":', size,',
 "_source": [', v_paste_variable,'],
  "query": {
    "bool": {
      "must_not": [
        {
          "geo_shape": {
            "geometry": {
              "shape": {
                "type": "circle",
               "radius": "', buffer ,'",
                "coordinates": [
                ', longitude,', ', latitude,'
                ]
              }
            }
          }
        }
      ],
      "must": [
        {
          "geo_shape": {
            "geometry": {
              "shape": {
                "type": "circle",
                "radius": "', ring,'",
                "coordinates": [
               ',longitude,', ', latitude,'
                ]
              }
            }
          }
        }
      ]
    }
  }
}'
      )

    # Extract_data
    es_request <-
      httr::POST(
        url = credential,
        body = query_ring ,
        encode = "json", # this lets' httr do the work for you
        httr::timeout(10000),
        httr::content_type_json(), # easier than making a header yourself
        httr::verbose()
      )

    text_request <- httr::content(es_request, as = "text")
    df_request <- jsonlite::fromJSON(text_request)

    df_geo_data <-
      df_request %>%
      .$hits %>%
      .$hits %>%
      .$"_source"

    return(df_geo_data)
  }
