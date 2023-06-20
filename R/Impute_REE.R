#' Impute Rare earth elements
#'
#' Imputes missing REE after modelling. Expect the output of 'model_REE()'
#' function. Only missing values are replaced.
#'
#' By default, exclude models with R-squared lower than 0.95. This limit is flexible and method dependent.
#' As guidelines, the Chondrite-Lattice mthod should consider R-squared > 0.95 for at least 3 points.
#' The Chondrite-Onuma method should consider R-squared >0.98 for at least 4 points.
#'
#'
#'
#'
#' @param data A dataframe resulting from 'model_ree()'
#' @param prefix A prefix in your columns e.g. ICP_La
#' @param suffix A suffix in your columns e.g. La_ppm
#' @param rsquared A numerical value between 0 and 1. Tolerance to mis-fitting models. set as 0.9 by default.
#'
#' @return A dataframe
#' @export
#'
#'
#'
#' @examples
#'
#' Ballard_et_al_Zircon %>%
#' dplyr::slice(1:100) %>%
#' model_REE(prefix = 'Zr', suffix = 'ppm') %>%
#' impute_REE(prefix = 'Zr', suffix = 'ppm')
#'
#'
#'
impute_REE <- function(data, prefix = NULL, suffix = NULL, rsquared = 0.95) {

  Original<-
    data

  Original_cleanNames <-
    data %>%
    CleanColnames(prefix = prefix, suffix = suffix)

  REE_DATA <-
    Original_cleanNames %>%
    dplyr::select(rowid, dplyr::matches(paste0('^',REE_plus_Y_Elements,'$') , ignore.case = F)) %>%
    tidyr::pivot_longer(cols = -rowid, names_to = 'Element', values_to = 'values')%>%
    dplyr::filter(Element != 'Ce' ,Element != 'Eu')

  rsquared_data <- Original_cleanNames %>%   dplyr::select(rowid, model_r.squared )

  calc_Data <-
    Original_cleanNames %>%
    dplyr::select(rowid, dplyr::matches('^ppmCalc'))%>%
    CleanColnames(prefix = 'ppmCalc') %>%
    tidyr::pivot_longer(cols = -rowid, names_to = 'Element', values_to = 'calc_value') %>%
    dplyr::left_join(., rsquared_data, by = 'rowid') %>%
    dplyr::filter(model_r.squared > rsquared)


  REE_DATA <- REE_DATA %>%
    dplyr::left_join(., calc_Data, by = c('rowid',"Element")) %>%
    dplyr::arrange(dplyr::desc(is.na(values))) %>%
    dplyr::mutate(imputed_values = ifelse(is.na(values), calc_value, values)) %>%
    dplyr::mutate(Element = paste0('Imputed_', Element)) %>%
    dplyr::select(rowid, Element, imputed_values) %>%
    tidyr::pivot_wider( names_from = 'Element', values_from = 'imputed_values')



  Original<- Original %>%  dplyr::left_join(., REE_DATA, by = 'rowid')

  return(Original)
}
