Element_denorm <- function(dat, method = Oneill2014CI) {


  original <- dat

  dat <- dat %>% dplyr::select(rowid, tidyr::matches('NormalizedCalc', ignore.case = FALSE))
  dat <-  dat %>%  tidyr::pivot_longer(names_to = 'Element_name' ,cols = -rowid)

  Element_Data %>% dplyr::select(Element_name)




  return(dat)
}
