#' Title
#'
#'  This is a wrapper for data %>% Model_REE() %>% Imputate_REE %>% Add_parameters()
#' @param data
#' @inheritParams Element_norm
#'
#' @return
#' @export
#'
#' @examples
DoItALL <-
  function(data,
           preffix = NULL,
           suffix = NULL,
           method = PalmeOneill2014CI){

    data %>%
      Model_REE(preffix =preffix, suffix = suffix, method = {{method}}) %>%
      Imputate_REE(preffix =preffix, suffix = suffix) %>%
      Add_parameters(preffix =preffix, suffix = suffix)

}
