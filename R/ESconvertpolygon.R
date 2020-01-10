#' Convert a elastic search polygon to sf object
#'
#' @param data_geo geodataset extracted by elastic search database. Data geo must be a polygon or a multipolygon.
#' @param var_id unique vector of geodataset
#' @param merge_id name unique vector of geodataset
#' @param data_df same geodataset in data_geo, but without geographical atributtes
#'
#' @return a sf object
#' @export
#' @examples
#'  es.polygon.r(
#'   city,
#'   city$id,
#'   "id",
#'   city_no_geo
#'  )

es.polygon.r <- function(data_geo, var_id, merge_id, data_df){

  geo_array <- data_geo %>% purrr::map_depth(1, is.array)  # Identifica se o objeto lat long vem em formato de array
  lista_array <- which(geo_array == TRUE) # Índice do formato array lat/long

  data_df_id <- var_id[lista_array] # cria nova data.base somente pela variável id
  base_array <- data_df[lista_array, ]  # cria nova data.base

  simple <- # Converte para matrix numérica de lat/long
    purrr::map(data_geo[lista_array],  ~plyr::alply(.x, 3)) %>%
    purrr::map(., ~unname(.x)) %>%
    purrr::map(., ~rbind(.x))  %>%
    purrr::map(., ~as.matrix(.x)) %>%
    purrr::map(., ~sprof::list.as.matrix(.x))

  # Converte para matrix numérica em objeto espacial id
  sp_simple <-
    simple %>%
    purrr::map2(
      .x = .,
      .y = data_df_id,
      ~list(.x) %>% sf::st_polygon(.) %>% sf::st_sfc() %>%  sf::st_sf(id = .y, geometry = .))

  union_sp <- sf::st_sf(data.table::rbindlist(sp_simple)) # Une os polígonos

  nd_spatial <- dplyr::left_join(union_sp, base_array, by = merge_id) # Une os polígonos com o data.frame

  ## Função para os polígonos em formato não array
  if(length(lista_array) != length(data_geo)){

    not_array_geo <- data_geo[-lista_array] # filtra coordinates dentre aqueles que não são array
    not_array <- var_id[-lista_array] # Filtra id dentre aqueles que não são array
    base_not_array <- data_df[-lista_array, ]  # Filtra data_df dentre aqueles que não são array

    not_sp_simple <- # Objeto espacial
      not_array_geo %>%
      purrr::map2(
        .x = .,
        .y = not_array,
        ~sf::st_polygon(.x) %>% sf::st_sfc() %>%  sf::st_sf(id = .y, geometry = .))

    not_union_sp <- sf::st_sf(data.table::rbindlist(not_sp_simple)) # Une os polígonos

    nd_spatial_not <- dplyr::left_join(not_union_sp, base_not_array, by = merge_id) # Une os polígonos com o data.frame

    # nd_spatial <- rbind(nd_spatial_not, nd_spatial)
  }

  if(exists("nd_spatial_not") == TRUE){

    nd_spatial <- rbind(nd_spatial_not, nd_spatial)

  }

  return(nd_spatial)
}
