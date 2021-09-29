Add_Element_data <- function(dat, preffix = NULL, suffix = NULL, Element_list = REE_plus_Y_Elements, add_ID = FALSE, add_all = TRUE) {


  if (!is.data.frame(dat)) {
    stop("dat should be a dataframe, you provided:", class(dat)[1])
  }

  ## add ID

  if (add_ID == TRUE) {

    dat <- dat %>% Add_ID(var = var)

  }

 dat <-  CleanColnames(suffix = suffix, preffix = preffix ) %>%
   dplyr::select(rowid, paste0('^',Element_list, '$'))

  original <- dat



   ## matches names in the elemental data

Element_Data <- Element_Data

    dat %>% dplyr::left_join(., Element_Data, by = "Element_name")
}
