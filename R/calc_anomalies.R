#' Title
#'
#' @param data
#' @param preffix
#' @param suffix
#'
#' @return
#' @export
#'
#' @examples
Add_parameters <- function(
  data,
  preffix = NULL,
  suffix = NULL


){

  Original <- data

  Ce_Eudata <- data %>%
    CleanColnames(preffix, suffix) %>%
    dplyr::select(rowid, Eu, Ce) %>%
    tidyr::pivot_longer(-rowid, names_to = 'Element_name')

  Others_REE_plus_y <-
    data %>%
    dplyr::select(rowid, dplyr::matches('Imputated')) %>%
    tidyr::pivot_longer(-rowid, names_to = 'Element_name') %>%
    dplyr::mutate(Element_name = stringr::str_remove(Element_name, '^Imputated_'))

 molar_data <-  dplyr::bind_rows(Ce_Eudata, Others_REE_plus_y)  %>%
    Add_Element_data() %>%
    dplyr::select(1:3, Atomic_Mass) %>%
    dplyr::mutate(REE_plus_Y_molar = (value/Atomic_Mass)) %>%
    dplyr::group_by(rowid) %>%
    dplyr::summarise(REE_plus_Y_molar= sum(REE_plus_Y_molar))


 anomalies <-  data %>%
   CleanColnames(preffix, suffix) %>%
   dplyr::select(rowid, Eu, Ce, ppmCalc_Ce, ppmCalc_Eu, P ) %>%
   dplyr::mutate( `Eu/Eu*` = Eu/ppmCalc_Eu ,
                  `Ce/Ce*` = Eu/ppmCalc_Ce,
                  P_molar  = P / Element_Data[Element_Data$Element_name == 'P', 'Atomic_Mass', drop = TRUE]) %>%
   dplyr::select(-c(Eu, Ce, ppmCalc_Ce, ppmCalc_Eu, P))


Original <- purrr::reduce(list(Original, molar_data, anomalies),dplyr::left_join, by = 'rowid' )

  return(
    Original
   )
}
