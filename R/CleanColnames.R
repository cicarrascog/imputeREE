CleanColnames <- function(dat, preffix = NULL, suffix = NULL) {



   if (!is.null(preffix)) {

   dat <- dat %>%  dplyr::rename_with(~stringr::str_remove(.x, pattern = paste0('^',preffix, '[:punct:]?')))

   }

  if (!is.null(suffix)) {

  dat <- dat %>%dplyr::rename_with(~stringr::str_remove(.x, pattern = paste0('[:punct:]?', suffix, '$')))

  }

  return(dat)

}
