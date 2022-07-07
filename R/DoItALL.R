
#' Calculate and Impute REE missing data and anomalies.
#'
#' This is a wrapper for data %>% model_REE() %>% impute_REE %>% add_parameters()
#'
#' @param data A data frame containing REE data
#' @param prefix a string. A prefix in the columns names e.g 'Whole_rock_La', where 'Whole_rock_' is the prefix
#' @param suffix a string. A suffix in the columns names e.g 'La_ppm', where '_ppm' is the suffix
#' @param method an option from: 'PalmeOneill2014CI', 'Oneill2014Mantle', 'McDonough1995CI'
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
           method = PalmeOneill2014CI){

    data %>%
      model_REE(prefix =prefix, suffix = suffix, method = {{method}}) %>%
      impute_REE(prefix =prefix, suffix = suffix) %>%
      add_parameters(prefix =prefix, suffix = suffix)

}


