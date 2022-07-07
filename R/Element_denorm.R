#' Denormalize chrodrite Normalize to ppm
#'
#' @param dat A dataframe
#' @param method an option from: 'PalmeOneill2014CI', 'Oneill2014Mantle', 'McDonough1995CI'
#'
#' @return A dataframe
#'

element_denorm <- function(dat, method = PalmeOneill2014CI) {

  PalmeOneill2014CI <- rowid <- Element_Data <- Element_name <- NULL

  original <- dat

  dat <- dat %>% dplyr::select(rowid, tidyr::matches('NormalizedCalc', ignore.case = FALSE))
  dat <-  dat %>%  tidyr::pivot_longer(names_to = 'Element_name' ,cols = -rowid)

  Element_Data %>% dplyr::select(Element_name)




  return(dat)
}
