#' Model REE + Y contents using an empirical method based on the lattice strain theory
#'
#' Model REE will make a linear regression between the REE (+Y) and the relationship of the ideal Ionic Radii in the lattice site (r0) and the ionic radii of the element that use that space (ri) according to the relationship : (ri/3 + r0/6)(ri-r0)^2`
#'
#' For details in the lattice strain theory, see Blundy and Wood 1994.
#'
#' @param dat A data frame
#' @param r0 A number: ionic radii of the lattice site r0
#' @param exclude a string: vector including elements that should be ommited from modelling. La, Ce and Eu are the default. Ce and Eu should be always included
#' @param Y_correction_fact a number: correction factor for underestimated Y. 1.29 by default.
#' @inheritParams Element_norm
#'
#' @return a dataframe
#' @export
#'
#' @examples
#'
#' testing_data %>%  Model_REE(preffix = 'Zr', suffix = 'ppm')
Model_REE <- function(dat,
                      r0 = 0.84,
                      exclude = c("La", "Ce", "Eu", "Y"),
                      preffix = NULL,
                      suffix = NULL,
                      method = PalmeOneill2014CI,
                      Y_correction_fact = 1.29,
Yb_correction_fact = 1/0.8785,
Lu_correction_fact = 1/0.8943) {
  Original <- dat %>% Add_ID() ## backup of original data.

## Notes removed
  PalmeOneill2014CI <-
  rowid <-
  Element_Data <-
  Element_name <-
  value <-
  ShannonRadiiVIII_Coord_3plus <-
  data <-
  models <-
  tidied <-
  term <-
  estimate <-
  std.error <-
  statistic <-
  p.value <-
  glanced <-
  `(ri/3 + r0/6)(ri-r0)^2` <-
  estimate_Slope <-
  estimate_Intercept <-
  NormalizedCalc <-
  . <-
  ppmCalc <-
  NULL



## calculate chondrite normalized values and add ionic Radii
  dat <- dat %>%
    Element_norm("raw",
      preffix = preffix,
      suffix  = suffix
    ) %>%
    dplyr::mutate(Element_name = stringr::str_remove(Element_name, "[:punct:]?Normalized")) %>%
    Add_IonicRadii()




    Element_list <- REE_plus_Y_Elements
  #
  ###
  excludedREE <- dat %>%
    dplyr::filter(stringr::str_detect(
      string = Element_name,
      pattern = paste0(exclude, collapse = "|"),
      negate = F
    )) %>%
    dplyr::mutate(value = NA_real_)

  dat <- dat %>%
    dplyr::filter(stringr::str_detect(
      string = Element_name,
      pattern = paste0(exclude, collapse = "|"),
      negate = T
    ))


  dat <- dplyr::bind_rows(dat, excludedREE) ### add excluded REE as NA, since they are not modelled.

  model_nree <- dat %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(rowid) %>%
    dplyr::summarise(model_nree = sum(!is.na(value)))

  dat <- dplyr::left_join(dat, model_nree, by = "rowid")

  #### warning about samples with 3 data points for modelling

  lessthan3REE <- model_nree %>%
    dplyr::ungroup() %>%
    dplyr::filter(model_nree < 4) %>%
    nrow()

  warning("There are ", lessthan3REE, " Samples with less than 3 or less elements to model, consider filtering that data, or including more elements")








  #### warning about samples with 3 data points for modelling
  stopper <- dat %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.na(model_nree) | model_nree == 1) %>%
    nrow()

  if (stopper >= 2) {

    stopper <- dat %>%
      dplyr::ungroup() %>%
      dplyr::arrange(model_nree) %>%
      dplyr::filter(is.na(model_nree) | model_nree <= 2) %>%
      dplyr::select(rowid) %>%
      dplyr::distinct()

    stopper <- paste0(stopper$rowid, collapse = " ,  ")

    warning("rowid: ", stopper[1], "\n do not have enough data for modelling. They have been excluded from the modelling.")

    exluded_rows <- dat %>%
      dplyr::ungroup() %>%
      dplyr::arrange(model_nree) %>%
      dplyr::filter(is.na(model_nree) | model_nree <= 2)

    dat <- dat %>%
      dplyr::filter(!is.na(model_nree) & model_nree > 2)
  }


  ### Calculate "strain"

  dat <- dat %>%
    dplyr::mutate(
      `(ri/3 + r0/6)(ri-r0)^2` = (ShannonRadiiVIII_Coord_3plus / 3 + r0 / 6) * (ShannonRadiiVIII_Coord_3plus - r0)^2
    )


### Model REE ####
  dat <- dat %>%
    dplyr::group_by(rowid) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      models = purrr::map(data, ~ lm(log(value) ~ `(ri/3 + r0/6)(ri-r0)^2`, na.action = na.omit, data = .x)),
      tidied = purrr::map(models, broom::tidy),
      glanced = purrr::map(models, broom::glance)
    ) %>%
    tidyr::unnest(tidied) %>%
    dplyr::mutate(term = ifelse(stringr::str_detect("Intercept", term), "Intercept", "Slope")) %>%
    tidyr::pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>%
    tidyr::unnest(glanced, names_sep = "model_") %>%
    tidyr::unnest(data) %>%
    Add_NormValues(method = {{ method }}) %>%
    dplyr::mutate(
      NormalizedCalc = exp(`(ri/3 + r0/6)(ri-r0)^2` * estimate_Slope + estimate_Intercept),
      ppmCalc = NormalizedCalc * {{ method }}
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename_with(
      .cols = dplyr::matches("^glanced"),
      ~ stringr::str_replace_all(pattern = "glanced", replacement = "", string = .x))


    if (stopper >= 2) {

      dat <- dat %>%  dplyr::bind_rows(., exluded_rows)

      }

      dat <- dat %>%
      dplyr::select(-c(models, ShannonRadiiVIII_Coord_3plus, `(ri/3 + r0/6)(ri-r0)^2`, value, {{ method }})) %>%
    tidyr::pivot_wider(names_from = Element_name, values_from = c(NormalizedCalc, ppmCalc)) %>%
    dplyr::relocate(rowid, model_nree, dplyr::matches("NormalizedCalc"), dplyr::matches("ppmCalc")) # %>%

## Correction Factor for Y #####
dat <- dat %>%
        dplyr::mutate(
          ppmCalc_Y =  ppmCalc_Y * Y_correction_fact,
          NormalizedCalc_Y = NormalizedCalc_Y * Y_correction_fact,
          ppmCalc_Yb =  ppmCalc_Yb * Yb_correction_fact,
          NormalizedCalc_Yb = NormalizedCalc_Yb *Yb_correction_fact,
          ppmCalc_Lu =  ppmCalc_Lu * Lu_correction_fact,
          NormalizedCalc_Lu = NormalizedCalc_Lu *Lu_correction_fact
)

## join to original Data ####

  dat <- dplyr::left_join(Original, dat, by = "rowid")
  return(dat)
}
