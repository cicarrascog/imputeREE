#' Clean variable names that have prefixes or suffixes
#'
#' This is a helper function
#'
#' @param dat a data frame
#' @param prefix A character of length 1
#' @param suffix A character of length 1
#' @importFrom magrittr %>%
#' @return A data frame


CleanColnames <- function(dat, prefix = NULL, suffix = NULL) {

   if (!is.null(prefix)) {

   dat <- dat %>%  dplyr::rename_with(~stringr::str_remove(.x, pattern = paste0('^',prefix, '[:punct:]?')))

   }

  if (!is.null(suffix)) {

  dat <- dat %>% dplyr::rename_with(~stringr::str_remove(.x, pattern = paste0('[:punct:]?', suffix, '$')))

  }

  return(dat)

}
# @export
#
# @examples
#
# testing_data |> CleanColnames(prefix = 'Zr', suffix = 'ppm')
#
