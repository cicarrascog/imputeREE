#' Model REE + Y contents using an empirical method based on the lattice strain theory
#'
#' Model REE will make a linear regression between the REE (+Y) and the relationship of the ideal Ionic Radii in the lattice site (r0) and the ionic radii of the element that use that space (ri) according to the relationship : (ri/3 + r0/6)(ri-r0)^2`
#' For details in the lattice strain theory, see Blundy and Wood 1994.
#'
#' @param dat A data frame
#' @param r0 A number: ionic radii of the lattice site r0
#' @param method  a number. a choice of `1` for Carrasco-Godoy and Campell or `2` for Zhong et al. method for REE regression.
#' @param exclude a string: vector including elements that should be omitted from modelling. La, Ce and Eu are the default. Ce and Eu should be always included
#' @param prefix A prefix in your columns e.g. ICP_La
#' @param suffix A suffix in your columns e.g. La_ppm
#' @param chondrite an option from: PalmeOneill2014CI, Oneill2014Mantle, McDonough1995CI
#' @param correct_middle a logical. If `TRUE` will apply a correction factor for Nd, Sm, Gd, Tb and Dy.
#' @param Pr_correction_fact  a number: correction factor for overestimated Pr 1/0.918
#' @param Nd_correction_fact  a number: correction factor for underestimated Nd 1/0.0.989
#' @param Sm_correction_fact  a number: correction factor for overestimated Sm 1/1.022
#' @param Gd_correction_fact  a number: correction factor for overestimated Gd 1/1.033
#' @param Tb_correction_fact  a number: correction factor for overestimated Tb 1/1.050
#' @param Dy_correction_fact  a number: correction factor for overestimated Dy 1/1.032
#' @param correct_heavy a logical. If `TRUE` will apply a correction factor for Yb, Lu and Y.
#' @param Y_correction_fact a number: correction factor for underestimated Y. 1/ 0.72 by default.
#' @param Ho_correction_fact a number: correction factor for Ho. 1 by default.
#' @param Er_correction_fact a number: correction factor for underestimated Er. 1/0.97 by default.
#' @param Tm_correction_fact a number: correction factor for Tm. 1 by default.
#' @param Yb_correction_fact a number: correction factor for underestimated Yb. 1/0.8785  by default.
#' @param Lu_correction_fact a number: correction factor for underestimated Lu. 1/0.8943 by default.
#'
#' @importFrom rlang .data
#'
#' @return a dataframe
#' @export
#'
#' @examples
#'
#' testing_data %>%  model_REE(prefix = 'Zr', suffix = 'ppm')
#'
#'
model_REE <- function(dat,
                      r0 = 0.84,
                      method = 1,
                      exclude = c("La", "Ce", "Eu", "Y"),
                      prefix = NULL,
                      suffix = NULL,
                      chondrite = PalmeOneill2014CI,
                      correct_heavy = TRUE,
                      Y_correction_fact = 1/0.72,
                      Ho_correction_fact = 1,
                      Er_correction_fact = 1/0.974,
                      Tm_correction_fact = 1,
                      Yb_correction_fact = 1/0.8785,
                      Lu_correction_fact = 1/0.8943,
                      correct_middle = T,
                      Nd_correction_fact = 1/0.989,
                      Sm_correction_fact = 1/1.022,
                      Gd_correction_fact = 1/1.033,
                      Tb_correction_fact = 1/1.050,
                      Dy_correction_fact = 1/1.032,
                      Pr_correction_fact =1/0.918
) {
  Original <- dat %>% add_ID() ## backup of original data.

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
      prefix = prefix,
      suffix  = suffix,
      chondrite = {{chondrite}}
    ) %>%
    dplyr::mutate(Element_name = stringr::str_remove(Element_name, "[:punct:]?Normalized")) %>%
     add_IonicRadii()




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




### Model REE ####

  if (method == 1) {

    dat <- dat  %>%

      dplyr::mutate(
        `(ri/3 + r0/6)(ri-r0)^2` = (ShannonRadiiVIII_Coord_3plus / 3 + r0 / 6) * (ShannonRadiiVIII_Coord_3plus - r0)^2
      )

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
      add_NormValues(chondrite = {{chondrite}}) %>%
      dplyr::mutate(
        NormalizedCalc = exp(`(ri/3 + r0/6)(ri-r0)^2` * estimate_Slope + estimate_Intercept),
        ppmCalc = NormalizedCalc * {{ chondrite }}
      ) %>%
      dplyr::ungroup() %>%
      dplyr::rename_with(
        .cols = dplyr::matches("^glanced"),
        ~ stringr::str_replace_all(pattern = "glanced", replacement = "", string = .x))
  }


  if (method == 2) {

    dat <- dplyr::left_join(dat, imputeREE::Element_Data %>% dplyr::select(Z_Zhong, Element_name), by = 'Element_name')

    dat <- dat %>%
      dplyr::group_by(rowid) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        models = purrr::map(data, ~ lm(log10(value) ~ Z_Zhong, na.action = na.omit, data = .x)),
        tidied = purrr::map(models, broom::tidy),
        glanced = purrr::map(models, broom::glance)
      ) %>%
      tidyr::unnest(tidied) %>%
      dplyr::mutate(term = ifelse(stringr::str_detect("Intercept", term), "Intercept", "Slope")) %>%
      tidyr::pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>%
      tidyr::unnest(glanced, names_sep = "model_") %>%
      tidyr::unnest(data) %>%
      add_NormValues(chondrite = {{ chondrite }}) %>%
      dplyr::mutate(
        NormalizedCalc = exp(Z_Zhong * estimate_Slope + estimate_Intercept),
        ppmCalc = NormalizedCalc * {{ chondrite }}
      ) %>%
      dplyr::ungroup() %>%
      dplyr::rename_with(
        .cols = dplyr::matches("^glanced"),
        ~ stringr::str_replace_all(pattern = "glanced", replacement = "", string = .x))
  }




    if (stopper >= 2) {

      dat <- dat %>%  dplyr::bind_rows(., exluded_rows)

      }


if (method == 1) {
  dat <- dat %>%
    dplyr::select(-c(models, ShannonRadiiVIII_Coord_3plus, `(ri/3 + r0/6)(ri-r0)^2`, value, {{ chondrite }})) %>%
    tidyr::pivot_wider(names_from = Element_name, values_from = c(NormalizedCalc, ppmCalc)) %>%
    dplyr::relocate(rowid, model_nree, dplyr::matches("NormalizedCalc"), dplyr::matches("ppmCalc"))
}

if (method == 2) {
  dat <- dat %>%
    dplyr::select(-c(models, ShannonRadiiVIII_Coord_3plus, value, Z_Zhong, {{ chondrite }})) %>%
    tidyr::pivot_wider(names_from = Element_name, values_from = c(NormalizedCalc, ppmCalc)) %>%
    dplyr::relocate(rowid, model_nree, dplyr::matches("NormalizedCalc"), dplyr::matches("ppmCalc"))

}



## Correction Factor for Heavy  REE + Y #####
if (method == 1) {
if (correct_heavy) {
  dat <- correct_heavy(dat = dat,
                       Y_correction_fact =Y_correction_fact ,
                       Yb_correction_fact =Yb_correction_fact ,
                       Lu_correction_fact =Lu_correction_fact,
                       Ho_correction_fact = Ho_correction_fact,
                       Er_correction_fact = Er_correction_fact,
                       Tm_correction_fact =Tm_correction_fact

                       )
}

      ## Correction Factor for Middle  REE  #####

      if (correct_middle) {
        dat <- correct_middle(dat = dat,
                              Pr_correction_fact = Pr_correction_fact,
                              Nd_correction_fact = Nd_correction_fact,
                              Sm_correction_fact = Sm_correction_fact,
                              Gd_correction_fact = Gd_correction_fact,
                              Tb_correction_fact = Tb_correction_fact,
                              Dy_correction_fact = Dy_correction_fact)
      }

}

## join to original Data ####

  dat <- dplyr::left_join(Original, dat, by = "rowid")
  return(dat)
}
