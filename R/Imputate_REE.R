#' Impute Rare earth elememts
#'
#' Imputes missing REE after modelling. Expect the output of Model_REE() function
#'
#' By default, exclude models with Rsqured lower than 0.9.
#'
#'
#'
#'
#' @param data
#' @param preffix
#' @param suffix
#' @param rsquared A numerical value between 0 and 1. Tolerance to missfiting models.
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#'
Imputate_REE <- function(data, preffix = NULL, suffix = NULL, rsquared = 0.9) {

  Original<-
    data

  Original_cleanNames <-
    data %>%
    CleanColnames(preffix = preffix, suffix = suffix)

  REE_DATA <-
    Original_cleanNames %>%
    dplyr::select(rowid, dplyr::matches(paste0('^',REE_plus_Y_Elements,'$') , ignore.case = F)) %>%
    tidyr::pivot_longer(cols = -rowid, names_to = 'Element', values_to = 'values')%>%
    dplyr::filter(Element != 'Ce' ,Element != 'Eu')

  rsquared_data <- Original_cleanNames %>%   dplyr::select(rowid, model_r.squared )

  calc_Data <-
    Original_cleanNames %>%
    dplyr::select(rowid, dplyr::matches('^ppmCalc'))%>%
    CleanColnames(preffix = 'ppmCalc') %>%
    tidyr::pivot_longer(cols = -rowid, names_to = 'Element', values_to = 'calc_value') %>%
    dplyr::left_join(., rsquared_data, by = 'rowid') %>%
    dplyr::filter(model_r.squared > rsquared)


  REE_DATA <- REE_DATA %>%
    dplyr::left_join(., calc_Data, by = c('rowid',"Element")) %>%
    dplyr::arrange(desc(is.na(values))) %>%
    dplyr::mutate(imputated_values = ifelse(is.na(values), calc_value, values)) %>%
    dplyr::mutate(Element = paste0('Imputated_', Element)) %>%
    dplyr::select(rowid, Element, imputated_values) %>%
    tidyr::pivot_wider( names_from = 'Element', values_from = 'imputated_values')



  Original<- Original %>%  dplyr::left_join(., REE_DATA, by = 'rowid')

  return(Original)
}
