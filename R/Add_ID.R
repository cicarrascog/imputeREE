#' Add_ID
#'
#' Add an unique ID per observation and checks that is not overwriting an existing column.
#'
#' @param dat a tibble or a dataframe
#' @param ID Name of column to use for rownames.
#' @param ... Other parameters passed onto the `tibble::rowid_to_column()` function
#'
#' @return a data frame
#' @export
#'
#' @examples
#'
#' Element_Data %>% Add_ID()
#'
Add_ID <- function(dat, ID = "rowid", ...) {


  if(!is.data.frame(dat)) {

    stop('I am so sorry, but this function only works dataframe (or tibble) input for dat!\n',
         'You have provided an object of class: ', class(dat)[1])
  }

  if(!is.character(ID)) {

    stop('ID should be a character vector of lenght 1',
         'You have provided an object of class: ', class(ID)[1])
  }

  if (ID %in% colnames(dat)) {
    stop( "It seems that you already have a `rowid` column. Please choose a new name with the `ID` parameter")
  } else {
    dat <- dat %>% tibble::rowid_to_column(var = ID)
    return(dat)
  }
}


