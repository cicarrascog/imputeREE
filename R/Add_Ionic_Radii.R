#' Add Chondrite or Mantle values for normalization.
#'
#' This is a helper function to work with Element_norm() and Element_denorm(). Takes long pivoted data to match element name and add normalizing values from the Element_data dataset.
#'
#'
#'
#' @param dat a dataframe or tibble.
#' @param method Ionic Radii from Shannon, 1976
#'
#' @return a data frame or tibble
#'

add_IonicRadii <- function(dat, method = ShannonRadiiVIII_Coord_3plus) {


  Element_name <- ShannonRadiiVIII_Coord_3plus <- NULL


  if (!is.data.frame(dat)) {
    stop("dat should be a dataframe, you provided:", class(dat)[1])
  }




  Element_Data <-  Element_Data %>% dplyr::select({{method}}, Element_name)
  dat <- dplyr::left_join(dat, Element_Data, by = 'Element_name')

  return(dat)

}
# @examples
#
# Ballards_2002_Zr %>%
# CleanColnames('Zr', 'ppm') %>%  Add_ID() %>%
# dplyr::select(rowid, La, Ce) %>%
# tidyr::pivot_longer(cols = -rowid, names_to = 'Element_name')  %>%
# Add_IonicRadii()
#
