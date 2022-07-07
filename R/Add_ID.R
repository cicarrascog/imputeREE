#' Add_ID
#'
#' Add an unique ID per observation and checks that is not overwriting an existing column. If the column already exist, it will take no action.
#' This is a wrapper of tibble::rowid_to_column() that checks that not columns is overwritten.
#'
#' @param dat a tibble or a dataframe
#' @param ID Name of column to use for rownames. 'rowid' is used if none is specified.
#' er parameters passed onto the `tibble::rowid_to_column()` function
#'
#' @return a data frame

add_ID <- function(dat, ID = "rowid") {


  if(!is.data.frame(dat)) {

    stop('I am so sorry, but this function only works dataframe (or tibble) input for dat!\n',
         'You have provided an object of class: ', class(dat)[1])
  }

  if(!is.character(ID) | length(ID) > 1) {

    stop('ID should be a character vector of lenght 1',
         'You have provided an object of class: ', class(ID)[1])
  }

  if (ID %in% colnames(dat)) {
    warning( paste("It seems that you already have a",ID ,"column, please change the ID to another name. No changes has been made."))

} else {
    dat <- dat %>% tibble::rowid_to_column(var = ID)
    return(dat)
  }
}


#
# @examples
#
# Element_Data %>% Add_ID()
#
