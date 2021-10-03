Element_denorm <- function(dat, method = PalmeOneill2014CI) {

  PalmeOneill2014CI <- rowid <- Element_Data <- Element_name <- NULL

  original <- dat

  dat <- dat %>% dplyr::select(rowid, tidyr::matches('NormalizedCalc', ignore.case = FALSE))
  dat <-  dat %>%  tidyr::pivot_longer(names_to = 'Element_name' ,cols = -rowid)

  Element_Data %>% dplyr::select(Element_name)




  return(dat)
}
