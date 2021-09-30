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
#' @param method Values to normalize: an option from: PalmeOneill2014CI, Oneill2014Mantle, McDonough1995CI.
#'
#' @return a data frame or tibble
#' @export
#'
#' @examples
Add_NormValues <- function(dat, method = PalmeOneill2014CI) {

  if (!is.data.frame(dat)) {
    stop("dat should be a dataframe, you provided:", class(dat)[1])
  }

Element_Data <-  Element_Data %>% dplyr::select({{method}}, Element_name)

dat <- dplyr::left_join(dat, Element_Data, by = 'Element_name')

return(dat)

}
