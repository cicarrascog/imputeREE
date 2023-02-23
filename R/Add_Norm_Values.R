#' Add Chondrite or Mantle values for normalization.
#'
#' This is a helper function to work with Element_norm() and Element_denorm(). Takes long pivoted data to match element name and add normalizing values from the Element_data dataset.
#'
#'
#' @param chondrite PalmeOneill2014CI, Oneill2014Mantle, McDonough1995CI
#' @param dat Dataframe or tibble.
#'doc
#' @return a data frame or tibble
#'

add_NormValues <- function(dat, chondrite = PalmeOneill2014CI) {

 Element_name <-  PalmeOneill2014CI <- NULL


  if (!is.data.frame(dat)) {
    stop("dat should be a dataframe, you provided:", class(dat)[1])
  }

Element_Data <-  Element_Data %>% dplyr::select({{chondrite}}, Element_name)

dat <- dplyr::left_join(dat, Element_Data, by = 'Element_name')

return(dat)

}
# @examples
#
# Data <- testing_data %>%  CleanColnames('Zr', 'ppm') %>%  Add_ID()
#
# Data %>%
# dplyr::select(rowid, La, Ce) %>%
# tidyr::pivot_longer(cols = -rowid, names_to = 'Element_name')  %>%
# Add_NormValues()
#
