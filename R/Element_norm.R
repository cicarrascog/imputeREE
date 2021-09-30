#' Calculate normalized values for a list of elements
#'
#' Element norm normalize values according to published values for the Primitive mantle and chondrites. By defect, it uses the values from Palme and O'Neill (2014). By default, REE + Y list is provided
#'
#'
#'
#' @param dat a dataframe
#' @param return a characther from: "rect" for a wide data return,"raw" for a long data return,"append" to append the results to the input data
#' @param method an option from: PalmeOneill2014CI, Oneill2014Mantle, McDonough1995CI
#' @param Element_list a character vector: indicating the elements that should be normalized. REE + Y by default
#' @inheritParams CleanColnames
#'
#' @return a data frame
#' @export
#'
#' @examples
#' testing_data %>% Element_norm()
#'
#'
Element_norm <- function(
  dat,
  return = "rect",
  method = PalmeOneill2014CI,
  preffix = NULL, ## in case you use prefix like: Whole_Rock_Ce
  suffix = NULL, ## in case you use prefix like: Ce_wt%
  Element_list = REE_plus_Y_Elements) {

  ### variable Checkin

  if (!is.data.frame(dat)) {
    stop("dat should be a dataframe, you provided:", class(dat)[1])
  }

  if (!any(return == "rect", return == "raw", return == "append")) {
    stop('Please choose a valid option: \n"rect" for a wide data return.\n"raw" for a long data return. \n"append" to append the results to the input data.')
  }

 ## add ID

dat <- dat %>% Add_ID()

original <- dat

dat <- dat %>% CleanColnames(preffix = preffix ,suffix = suffix)

Element_Data <-  Element_Data %>% dplyr::select({{method}}, Element_name)


  # Normalize data

  dat <- dat %>%
    dplyr::select(rowid, tidyr::matches(paste0('^',Element_list,"$"), ignore.case = FALSE)) %>% # Select all the columns with REE-Y plus the ID column
    tidyr::pivot_longer(-rowid, names_to = "Element_name") %>% # makes data long, so it is easier to calculate
    dplyr::left_join(., Element_Data, by = "Element_name") %>%
    dplyr::mutate(Element_name = paste(Element_name, 'Normalized', sep = '_'),
                  value = value/{{method}}) %>%
    dplyr::select(-{{method}})
# #
  ### Returns

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

return(dat)
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

