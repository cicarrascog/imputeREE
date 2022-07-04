#' Clean variable names that have preffixes or suffixes
#'
#' This is a helper function
#'
#' @param dat a dataframe
#' @param preffix A character of lengt 1
#' @param suffix A character of lengt 1
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' testing_data %>% CleanColnames(preffix = 'Zr', suffix = 'ppm')
#'
CleanColnames <- function(dat, preffix = NULL, suffix = NULL) {

   if (!is.null(preffix)) {

   dat <- dat %>%  dplyr::rename_with(~stringr::str_remove(.x, pattern = paste0('^',preffix, '[:punct:]?')))

   }

  if (!is.null(suffix)) {

  dat <- dat %>%dplyr::rename_with(~stringr::str_remove(.x, pattern = paste0('[:punct:]?', suffix, '$')))

  }

  return(dat)

}
