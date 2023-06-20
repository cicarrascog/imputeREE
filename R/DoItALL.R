
#' Calculate and Impute REE missing data and anomalies.
#'
#' This is a wrapper for `data %>% model_REE() %>% impute_REE() %>% add_parameters()`
#'
#' @inheritParams model_REE
#'
#'
#' @return A data frame. Includes imputed REE, model metrics, and calculated variables.
#' @export
#'
#' @examples
#'
#' Ballard_et_al_Zircon %>% calc_all(prefix = 'Zr_', suffix = '_ppm')
#'
#'
calc_all <-
  function(dat,
           prefix = NULL,
           suffix = NULL,
           chondrite = PalmeOneill2014CI){

    dat %>%
      model_REE(prefix =prefix, suffix = suffix, chondrite = {{chondrite}}) %>%
      impute_REE(prefix =prefix, suffix = suffix)

}


