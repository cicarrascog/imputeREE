Add_Element_data <- function(dat) {

  . <- NULL

  if (!is.data.frame(dat)) {
    stop("dat should be a dataframe, you provided:", class(dat)[1])
  }

   ## matches names in the elemental data

Element_Data <- Element_Data
    dat %>% dplyr::left_join(., Element_Data, by = "Element_name")
}
