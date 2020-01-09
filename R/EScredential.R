#' Function to create url to connect api elasticsearch
#'
#' @param user username
#' @param passwd password
#' @param find_search find or search in string
#' @param database database name
#' @param table table name
#' @return a character that contain a link
#' @export
#' @examples
#'  es.credential(
#'    "entreprise",
#'    "admin123",
#'    "search",
#'    "finance",
#'    "realties"
#'  )

es.credential <- function(
  user = user,
  passwd = passwd,
  find_search = find_search, # I need to find a reason
  database = database,
  table = table
) {

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
    paste0("/_") %>%
    paste0(find_search)

  return(path_es)

}
