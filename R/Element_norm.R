#' Calculate normalized values for a list of elements
#'
#' Element norm normalize values according to published values for the Primitive mantle and chondrites. By defect, it uses the values from Palme and O'Neill (2014). By default, REE + Y list is provided
#'
#'
#'
#' @param dat a dataframe
#' @param return a characther from: "rect" for a wide data return,"raw" for a long data return,"append" to append the results to the input data
#' @param method an opction from: Oneill2014CI, Oneill2014Mantle, McDonough1995CI
#' @param prefix a character: by defaulkt it is 'Zr'
#' @param sufix  a character: by default it expect to be the unit, usually in 'ppm'
#' @param Element_list a character vector: indicating the elements that should be normalized
#' @param ID Name of column to use for rownames. Is name is alredy in use, an error message will ask for a new name
#'
#'
#' @return a data frame
#' @export
#'
#' @examples
#' testing_data %>%  Element_norm()
#'
Element_norm <- function(dat, return = 'rect',  method = Oneill2014CI , prefix = 'Zr', sufix = 'ppm', Element_list = REE_plus_Y_Elements, ID = 'rowid') {

 var <-  Oneill2014CI <- rowid <- Element_name <- matches <- value <- . <-  NULL

  if (!is.data.frame(dat)){

    stop('dat should be a dataframe, you provided:', class(dat)[1])
  }

  if(!any(return == 'rect',return == 'raw',return == 'append')){

    stop('Please choose a valid option: \n"rect" for a wide data return.\n"raw" for a long data return. \n"append" to append the results to the input data.')
  }

  dat <- dat %>% Add_ID(var = var)

  original <- dat

  Element_list <-  paste(prefix, Element_list, sufix, sep = '_')
  Element_Data <- Element_Data %>%  dplyr::mutate(Element_name = paste(prefix, Element_name, sufix, sep = '_')) %>%  dplyr::select({{method}}, Element_name)

  # Add_ID(dat)

  dat <- dat %>%
    dplyr::select(rowid, matches(Element_list)) %>% # Select all the columns with REE-Y plus the ID column
    tidyr::pivot_longer(-rowid, names_to = "Element_name") %>% # makes data long, so it is easier to calculate
    dplyr::left_join(., Element_Data, by = "Element_name") %>%
    dplyr::mutate(Element_name = paste(Element_name, 'Normalized', sep = '_'),
                  value = value/{{method}}) %>%
    dplyr::select(-{{method}})

  if (return == 'rect') {
    dat <- dat %>%
      tidyr::pivot_wider(id_cols = rowid, names_from = Element_name, values_from = value)
    return(dat)
  }

  if (return == 'raw') {

    return(dat)
  }

  if (return == 'append') {
    dat <- dat %>%
      tidyr::pivot_wider(id_cols = rowid, names_from = Element_name, values_from = value)
    dat <- dplyr::left_join(original, dat, by = 'rowid')

    return(dat)
  }

}
# REE_norm #####################################################################

# This function takes a table as input with REE values as ppm as input.
# It returns a table with condrite normalized values for REE.
# For now, it only considers the values of McDonough, W. F. & Sun, S. -s.
# The composition of the Earth. Chemical Geology 120, 223–253 (1995).
# Arguments :
# dat : a data frame with zircon REE data in ppm.
#  pivot_longer: a logical that tells the function to return data in a long format (useful for REE diagrams), or wide format (useful for binary plots)
# join: a logical that determines if other columns in the input should be kept or not.

