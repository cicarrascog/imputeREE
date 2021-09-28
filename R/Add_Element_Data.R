Add_Element_data <- function(dat, prefix = "Zr", sufix = "ppm_Normalized", Element_list = REE_plus_Y_Elements, add_ID = FALSE, add_all = FALSE) {


  if (!is.data.frame(dat)) {
    stop("dat should be a dataframe, you provided:", class(dat)[1])
  }

  ## add ID
  if (add_ID == TRUE) {
    dat <- dat %>% Add_ID(var = var)
  }

  original <- dat

  ## matches names

  Element_list <- paste(prefix, Element_list, sufix, sep = "_")

  if (is.null(prefix)) {
    Element_list <- gsub("^_", "", Element_list)
  }

  if (is.null(sufix)) {
    Element_list <- gsub("_$", "", Element_list)
  }

  ## matches names in the elemental data
  Element_Data <- Element_Data %>%
    dplyr::mutate(Element_name = paste(prefix, Element_name, sufix, sep = "_"))


  if (is.null(prefix)) {
    Element_Data <- Element_Data %>% dplyr::mutate(Element_name = stringr::str_remove(Element_name, "^_"))
  }

  if (is.null(sufix)) {
    Element_Data <- Element_Data %>% dplyr::mutate(Element_name = stringr::str_remove(Element_name, "_$"))
  }

  if (add_all == TRUE) {
    Element_Data <- Element_Data

    dat %>% dplyr::left_join(., Element_Data, by = "Element_name")
  } else {
  Element_Data <- Element_Data %>% dplyr::select(ShannonRadiiVIII_Coord_3plus, Element_name) }

  dat %>% dplyr::left_join(., Element_Data, by = "Element_name")
}
