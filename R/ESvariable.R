#' Avaliable variables in data collection
#'
#' @param user username
#' @param passwd password
#' @param find_search find or search in string
#' @param database database name
#' @param table table name
#' @return a vector of all variables names
#' @export
#' @examples
#'  es.credential(
#'    "entreprise",
#'    "admin123",
#'    "search",
#'    "finance",
#'    "realties"
#'  )
#'

es.variable <- function(user,
                        passwd,
                        find_search, # I need to find a reason
                        database,
                        table) {

  path_es <-
    paste0("https://") %>%
    paste0(user) %>%
    paste0(":") %>%
    paste0(passwd) %>%
    paste0(find_search) %>%
    paste0(".") %>%
    paste0(database) %>%
    paste0("/elasticsearch/admin/") %>%
    paste0(table) %>%
    paste0("/_mapping")

  res_conf <-
    httr::GET(
      url = path_es,
      httr::timeout(10000),
      encode = "json",
      httr::content_type_json(),
      httr::verbose()
    )

  list_cont <- httr::content(res_conf, as = "text")
  list_cont <- jsonlite::fromJSON(list_cont)

  name_variable <- list_cont$realties$mappings$imovel$properties %>% names()

  return(name_variable)

}
