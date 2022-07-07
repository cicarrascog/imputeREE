#' Calculate relevant data from REE
#'
#' @description
#' Calculate the Eu and Ce anomalies, the sum or REE + Y and P as molar ratios from imputed data. It expects the result of data %>% model_ree() %>%  impute_ree()
#'
#'
#' @param prefix A prefix in your columns e.g. ICP_La
#' @param suffix A suffix in your columns e.g. La_ppm
#' @param data A dataframe
#'
#' @importFrom dplyr desc
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' testing_data %>%
#' dplyr::slice(12) %>%
#' model_REE(prefix = 'Zr_', suffix = '_ppm') %>%
#' impute_REE(prefix = 'Zr_', suffix = '_ppm')  %>%
#' add_parameters(prefix = 'Zr_', suffix = '_ppm')
#'
#'
add_parameters <- function(
  data,
  prefix = NULL,
  suffix = NULL


){

  Original <- data

  Ce_Eudata <- data %>%
    CleanColnames('Zr', 'ppm') %>%
    dplyr::select(.data$rowid, .data$Eu, .data$Ce) %>%
    tidyr::pivot_longer(-.data$rowid, names_to = 'Element_name')

  Others_REE_plus_y <-
    data %>%
    dplyr::select(.data$rowid, dplyr::matches('Imputed')) %>%
    tidyr::pivot_longer(-.data$rowid, names_to = 'Element_name') %>%
    dplyr::mutate(Element_name = stringr::str_remove(Element_name, '^Imputed_'))

 molar_data <-  dplyr::bind_rows(Ce_Eudata, Others_REE_plus_y)  %>%
    add_element_data() %>%
    dplyr::select(1:3, Atomic_Mass) %>%
    dplyr::mutate(REE_plus_Y_molar = (value/Atomic_Mass)) %>%
    dplyr::group_by(.data$rowid) %>%
    dplyr::summarise(REE_plus_Y_molar= sum(REE_plus_Y_molar))


 anomalies <-  data %>%
   CleanColnames('Zr', 'ppm') %>%
   dplyr::select(.data$rowid, Eu, Ce, ppmCalc_Ce, ppmCalc_Eu, P ) %>%
   dplyr::mutate( `Eu/Eu*` = Eu/ppmCalc_Eu ,
                  `Ce/Ce*` = Eu/ppmCalc_Ce,
                  P_molar  = P / Element_Data[Element_Data$Element_name == 'P', 'Atomic_Mass', drop = TRUE]) %>%
   dplyr::select(-c(Eu, Ce, ppmCalc_Ce, ppmCalc_Eu, P))


Original <- purrr::reduce(list(Original, molar_data, anomalies),dplyr::left_join, by = 'rowid' )

  return(
    Original
   )
}
