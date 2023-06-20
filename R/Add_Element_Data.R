#' Add ionic radius and chondrite and mantle values, Z and Mass
#'
#' This is a helper function to work with Element_norm() and Element_denorm().
#' Add Ionic Radius to data and chondrite values. For now, only supports 3+ in
#' eight-fold
#' coordination for REE, Zr and Y.Values are from Shannon(1976),
#' McDonough and Sun (1995) and Palme and O'Neill (2014).
#'
#' @param dat Long data REE format
#'
#' @return A data frame
#'
# @examples
#
# Ballard_et_al_Zircon %>%
# CleanColnames(prefix = 'Zr_', suffix = '_ppm') %>%
# dplyr::select(REE_plus_Y_Elements) %>%
# tidyr::pivot_longer(cols = REE_plus_Y_Elements, names_to = 'Element_name') %>%
# add_element_data(.)
#
#
#
add_element_data <- function(dat) {

  . <- NULL

  if (!is.data.frame(dat)) {
    stop("dat should be a dataframe, you provided:", class(dat)[1])
  }

   ## matches names in the elemental data

Element_Data <- Element_Data
    dat %>% dplyr::left_join(., Element_Data, by = "Element_name")
}
