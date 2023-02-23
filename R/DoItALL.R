
#' Calculate and Impute REE missing data and anomalies.
#'
#' This is a wrapper for data %>% model_REE() %>% impute_REE %>% add_parameters()
#'
#' @param data A data frame containing REE data
#' @param prefix a string. A prefix in the columns names e.g 'Whole_rock_La', where 'Whole_rock_' is the prefix
#' @param chondrite an option from: PalmeOneill2014CI, Oneill2014Mantle, McDonough1995CI
#' @param suffix a string. A suffix in the columns names e.g 'La_ppm', where '_ppm' is the suffix
#'
#' @return A data frame. Includes imputed REE, model metrics, and calculated variables.
#' @export
#'
#' @examples
#'
#' testing_data %>% calc_all(prefix = 'Zr_', suffix = '_ppm')
#'
#'
calc_all <-
  function(data,
           prefix = NULL,
           suffix = NULL,
           chondrite = PalmeOneill2014CI){

    data %>%
      model_REE(prefix =prefix, suffix = suffix, chondrite = {{chondrite}}) %>%
      impute_REE(prefix =prefix, suffix = suffix)

}


