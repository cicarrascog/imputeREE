#' Add Chondrite or Mantle values for normalization.
#'
#' This is a helper function to work with Element_norm() and Element_denorm(). Takes long pivoted data to match element name and add normalizing values from the Element_data dataset.
#'
#' This is a test to see what happens in the following paragrapfs
#'
#' @family Add Functions
#' @seealso \code{\link{Element_norm()}}
#'
#' @param dat Dataframe or tibble.
#' @param method Ionic Radii from shannon1976. Not.
#'
#' @return a data frame or tibble
#' @export
#'
#' @examples
Add_IonicRadii <- function(dat, method = ShannonRadiiVIII_Coord_3plus) {

  if (!is.data.frame(dat)) {
    stop("dat should be a dataframe, you provided:", class(dat)[1])
  }

  Element_Data <-  Element_Data %>% dplyr::select({{method}}, Element_name)
  dplyr::left_join(dat, Element_Data, by = 'Element_name')

  return(dat)

}
