#' Calculate normalized values for a list of elements
#'
#' Element norm normalize values according to published values for the Primitive mantle and chondrites. By defect, it uses the values from Palme and O'Neill (2014). By default, REE + Y list is provided
#'
#'
#'
#' @param data a data frame
#' @param return a character from: "rect" for a wide data return,"raw" for a long data return,"append" to append the results to the input data
#' @param method an option from: PalmeOneill2014CI, Oneill2014Mantle, McDonough1995CI
#' @param Element_list a character vector: indicating the elements that should be normalized. REE + Y by default
#' @param prefix A prefix in your columns e.g. ICP_La
#' @param suffix A suffix in your columns e.g. La_ppm
#'
#' @return a data frame

Element_norm <- function(
  data,
  return = "rect",
  method = PalmeOneill2014CI,
  prefix = NULL, ## in case you use prefix like: Whole_Rock_Ce
  suffix = NULL, ## in case you use prefix like: Ce_wt%
  Element_list = REE_plus_Y_Elements) {


  PalmeOneill2014CI <- rowid <- Element_Data <- Element_name <- value <- NULL


  ### variable Check

  if (!is.data.frame(data)) {
    stop("data should be a dataframe, you provided:", class(data)[1])
  }

  if (!any(return == "rect", return == "raw", return == "append")) {
    stop('Please choose a valid option: \n"rect" for a wide data return.\n"raw" for a long data return. \n"append" to append the results to the input data.')
  }

  ## add ID

  data <- data %>% add_ID()

  original <- data

  data <- data %>% CleanColnames(prefix = prefix, suffix = suffix)

  # Element_Data <-  Element_Data %>% dplyr::select({{method}}, Element_name)


  # Normalize data

  data <- data %>%
    dplyr::select(rowid, tidyr::matches(paste0("^", Element_list, "$"), ignore.case = FALSE)) %>% # Select all the columns with REE-Y plus the ID column
    tidyr::pivot_longer(-rowid, names_to = "Element_name") %>% # makes data long, so it is easier to calculate
    add_NormValues(method = {{ method }}) %>%
    dplyr::mutate(
      Element_name = paste(Element_name, "Normalized", sep = "_"),
      value = value / {{ method }}
    ) %>%
    dplyr::select(-{{ method }})
  # #
  ### Returns

  if (return == "rect") {
    data <- data %>%
      tidyr::pivot_wider(id_cols = rowid, names_from = Element_name, values_from = value)
    return(data)
  }

  if (return == "raw") {
    return(data)
  }

  if (return == "append") {
    data <- data %>%
      tidyr::pivot_wider(id_cols = rowid, names_from = Element_name, values_from = value)
    data <- dplyr::left_join(original, data, by = "rowid")

    return(data)
  }

  return(data)
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

# @export
# @examples
#
# testing_data %>%  element_norm(prefix = 'Zr', suffix = 'ppm')
# testing_data %>%  element_norm(return = 'raw', prefix = 'Zr', suffix = 'ppm')
# testing_data %>%  element_norm(return = 'append',prefix = 'Zr', suffix = 'ppm')
#
