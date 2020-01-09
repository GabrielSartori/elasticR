#' Types of Avaliable variables in data collection
#'
#' @param user username
#' @param passwd password
#' @param find_search find or search in string
#' @param database database name
#' @param table table name
#' @return a named list. list_variable are the variable who are in list. Univariate variables are single variables. Raw variables are variable that need insert a .raw when used for aggregation functions
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

es.variable.type <- function(user,
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
      encode = "json", # this lets' httr do the work for you
      httr::content_type_json(), # easier than making a header yoursel
      httr::verbose()
    )

  list_cont <- httr::content(res_conf, as = "text")
  list_cont_df <- jsonlite::fromJSON(list_cont)

  name_variable <-
    list_cont_df$realties$mappings$imovel$properties

  # Types Univariates
  univariate_variable <-
    list_cont_df$realties$mappings$imovel$properties %>%
    map("type", extract) %>%
    discard(is.null) %>%
    bind_rows()

  # Types multivariate

  multivariate_type <-
    list_cont_df$realties$mappings$imovel$properties %>%
    map("type", extract) %>%
    keep(is.null) %>%
    names()

  # list_cont_df$realties$mappings$imovel$properties %>% glimpse()
  list_type_all <- list_cont_df$realties$mappings$imovel$properties


  get.type <- function(list_variable){

    map_depth(list_type_all, 0, list_variable) %>%
      .$properties %>%
      map_dfr("type")

  }

  # check avaliable raw
  raw_variables <-
    name_variable %>%
    map("fields", extract) %>%
    map("raw", extract) %>%
    discard(is.null) %>%
    names()


  list_variable <-
    map(.x = multivariate_type, .f = ~get.type(.x)) %>%
    set_names(multivariate_type)

  return(list(
    "list_variable" = list_variable,
    "univariate_variable" = univariate_variable,
    "raw_variables" = raw_variables)
  )

}

