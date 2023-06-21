#' Model REE + Y contents using different methods.
#'
#' This function models REE + Y using different methods. The Chondrite-Lattice method use
#'  a linear regression between the REE (+Y) chondrite-normalized  and the missfit term from the lattice strain equation `(ri/3 + r0/6)(ri-r0)^2`. The Chondrite-Onuma method use the quadratic relationship between the ionic radii and chondrite normalized REE values. The method of Zhong et al. (2019) use a logaritmic relationship between the atomic number of the REE and the chondrite normalized REE.
#' For details in the lattice strain theory, see Blundy and Wood 1994. For more details in the imputation methods see \href{https://link.springer.com/article/10.1007/s00410-023-02025-9}{Carrasco-Godoy and Campbell (2023)}, and \href{https://link.springer.com/article/10.1007/s00710-019-00682-y}{Zhong et al. (2019)}
#'
#' @param dat A data frame with REE data in ppm
#' @param r0 A number: ionic radii of the lattice site r0. By default is 0.87 A, the median value obtained by Carrasco-Godoy and Campbell.
#' @param method  a number. a choice of `1` for Chondrite Lattice or `2` for Zhong et al. (2019) or `3` for Chondrite-Onuma method.
#' @param exclude a string: vector including elements that should be omitted from modelling. La, Ce and Eu are the default. Ce and Eu should be always included
#' @param prefix A prefix in your columns e.g. ICP_La
#' @param suffix A suffix in your columns e.g. La_ppm
#' @param chondrite an option from: PalmeOneill2014CI, Oneill2014Mantle, McDonough1995CI
#' @param Pr_correction_fact  a number: correction factor for overestimated Pr 1/0.918
#' @param Nd_correction_fact  a number: correction factor for underestimated Nd 1/0.0.989
#' @param Sm_correction_fact  a number: correction factor for overestimated Sm 1/1.022
#' @param Gd_correction_fact  a number: correction factor for overestimated Gd 1/1.033
#' @param Tb_correction_fact  a number: correction factor for overestimated Tb 1/1.050
#' @param Dy_correction_fact  a number: correction factor for overestimated Dy 1/1.032
#' @param Y_correction_fact a number: correction factor for underestimated Y. 1/ 0.72 by default.
#' @param Ho_correction_fact a number: correction factor for Ho. 1 by default.
#' @param Er_correction_fact a number: correction factor for underestimated Er. 1/0.97 by default.
#' @param Tm_correction_fact a number: correction factor for Tm. 1 by default.
#' @param Yb_correction_fact a number: correction factor for underestimated Yb. 1/0.8785  by default.
#' @param Lu_correction_fact a number: correction factor for underestimated Lu. 1/0.8943 by default.
#' @param Calibrate Logical (T or F). If True, the model is calibrated using the correction factors. By default it is the reciprocal of the median REE from the work of Carrasco-Godoy and Campbell is used.
#' @param long_format If T, rectangular long data is returned.
#' @param estimate_r0 If T, r0 is estimated using a method similar to the one from Loader et al. 2022.
#' @param r0_step If r0 is estimated, this define the step for iteration. smaller step heavily increases the computing time.
#' @param r0_min Minimun value from which the iteration starts. Calculated from r0.
#' @param r0_max Maximun value at which iteration ends. Calculated from r0.
#'
#' @importFrom rlang .data
#' @family model REE
#'
#' @return a dataframe
#' @export
#'
#' @examples
#'
#' Ballard_et_al_Zircon %>%  model_REE(prefix = 'Zr', suffix = 'ppm')
#'
#'
model_REE <- function(dat,
                      method = 1,
                      long_format = F,
                      exclude = c("La", "Pr", "Ce", "Eu", "Y"),
                      r0 = 0.84,
                      chondrite = PalmeOneill2014CI,
                      estimate_r0 = FALSE,
                      r0_step = 0.01,
                      r0_min = 0.01,
                      r0_max = 0.15,
                      prefix = NULL,
                      suffix = NULL,
                      Calibrate = T,
                      Pr_correction_fact = 1 / 0.918,
                      Y_correction_fact = 1 / 0.72,
                      Dy_correction_fact = 1 / 1.032,
                      Ho_correction_fact = 1,
                      Er_correction_fact = 1 / 0.974,
                      Tm_correction_fact = 1,
                      Yb_correction_fact = 1 / 0.8785,
                      Lu_correction_fact = 1 / 0.8943,
                      Nd_correction_fact = 1 / 0.989,
                      Sm_correction_fact = 1 / 1.022,
                      Gd_correction_fact = 1 / 1.033,
                      Tb_correction_fact = 1 / 1.050) {
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
    Z_Zhong <-
    correct <-
    estimate_X1 <-
  estimate_X2 <-
  glanced_model_r.squared <-

    NULL


  ### Known Bug: If a colum is missing, it will not be calculated. Still easy to recalculate with the model parameters.


  ## calculate chondrite normalized values and add ionic Radii

  dat <- dat %>%
    Element_norm("raw",
      prefix = prefix,
      suffix = suffix,
      chondrite = {{ chondrite }}
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
    if (estimate_r0 == F) {

      dat <- dat %>%
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
        add_NormValues(chondrite = {{ chondrite }}) %>%
        dplyr::mutate(
          NormalizedCalc = exp(`(ri/3 + r0/6)(ri-r0)^2` * estimate_Slope + estimate_Intercept),
          ppmCalc = NormalizedCalc * {{ chondrite }}
        ) %>%
        dplyr::ungroup() %>%
        dplyr::rename_with(
          .cols = dplyr::matches("^glanced"),
          ~ stringr::str_replace_all(pattern = "glanced", replacement = "", string = .x)
        )
    } # method assumes constant r0 for all

    if (estimate_r0 == T) {
      binder <- NULL
      r0_range <- seq(r0 - r0_min, r0 + r0_max, r0_step)

      for (i in r0_range) {
        data_to_bind <- dat %>%
          dplyr::group_by(rowid) %>%
          tidyr::nest() %>%
          dplyr::mutate(data = purrr::map(
            data, ~ .x %>% dplyr::mutate(`(ri/3 + r0/6)(ri-r0)^2` = ((ShannonRadiiVIII_Coord_3plus / 3) + (i / 6)) * (ShannonRadiiVIII_Coord_3plus - i)^2)
          )) %>%
          dplyr::mutate(
            models = purrr::map(data, ~ lm(log(value) ~ `(ri/3 + r0/6)(ri-r0)^2`, na.action = na.omit, data = .x)),
            tidied = purrr::map(models, broom::tidy),
            glanced = purrr::map(models, broom::glance)
          ) %>%
          dplyr::mutate(r0 = i)

        binder <- dplyr::bind_rows(binder, data_to_bind)
      }


      dat <- binder %>%
        tidyr::unnest(glanced, names_sep = "_model_") %>%
        dplyr::group_by(rowid) %>%
        dplyr::slice_max(glanced_model_r.squared) %>%
        tidyr::unnest(tidied) %>%
        dplyr::mutate(term = ifelse(stringr::str_detect("Intercept", term), "Intercept", "Slope")) %>%
        tidyr::pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>%
        tidyr::unnest(data) %>%
        add_NormValues(chondrite = {{ chondrite }}) %>%
        dplyr::mutate(
          NormalizedCalc = exp(`(ri/3 + r0/6)(ri-r0)^2` * estimate_Slope + estimate_Intercept),
          ppmCalc = NormalizedCalc * {{ chondrite }}
        ) %>%
        dplyr::ungroup() %>%
        dplyr::rename_with(
          .cols = dplyr::matches("^glanced_"),
          ~ stringr::str_replace_all(pattern = "glanced_", replacement = "", string = .x)
        )
    } # method estimate r0 interating along a range r0 values and selecting the best fit.
  } # Carrasco and Campbell method




  if (method == 2) {
    dat <- dplyr::left_join(dat, imputeREE::Element_Data %>% dplyr::select(Z_Zhong, Element_name), by = "Element_name")

    dat <- dat %>%
      dplyr::group_by(rowid) %>%
      dplyr::mutate(
        value = log10(value),
        Z_Zhong = log10(Z_Zhong)
      ) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        models = purrr::map(data, ~ lm(value ~ Z_Zhong, na.action = na.omit, data = .x)),
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
        NormalizedCalc = 10^(Z_Zhong * estimate_Slope + estimate_Intercept),
        ppmCalc = NormalizedCalc * {{ chondrite }},
        value = 10^value
      ) %>%
      dplyr::ungroup() %>%
      dplyr::rename_with(
        .cols = dplyr::matches("^glanced"),
        ~ stringr::str_replace_all(pattern = "glanced", replacement = "", string = .x)
      )
  }


## onuma Diagram
  if (method == 3) {
    # dat <- dplyr::left_join(dat, imputeREE::Element_Data %>% dplyr::select(ShannonRadiiVIII_Coord_3plus, Element_name), by = "Element_name")

    dat <- dat %>%
      dplyr::group_by(rowid) %>%
      dplyr::mutate(
      value = log10(value)
      ) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        models = purrr::map(data, ~
                              lm(value ~ poly(ShannonRadiiVIII_Coord_3plus, degree = 2, raw = T),
                                 na.action = na.omit, data = .x)),
        tidied = purrr::map(models, broom::tidy),
        glanced = purrr::map(models, broom::glance)
      ) %>%

      tidyr::unnest(tidied) %>%
      dplyr::mutate(term = dplyr::case_when(
        stringr::str_detect(string = term, pattern =  "Intercept")  ~ "Intercept",
        stringr::str_detect(string = term, pattern = ".*1$")  ~ "X1",
        stringr::str_detect(string = term, pattern = ".*2$")  ~ "X2"))  %>%
      tidyr::pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>%
      tidyr::unnest(glanced, names_sep = "model_") %>%
      tidyr::unnest(data) %>%
      add_NormValues(chondrite = {{ chondrite }}) %>%
      dplyr::mutate(
        NormalizedCalc = 10^((ShannonRadiiVIII_Coord_3plus * estimate_X1) + (ShannonRadiiVIII_Coord_3plus*ShannonRadiiVIII_Coord_3plus*estimate_X2) + estimate_Intercept),
        ppmCalc = NormalizedCalc * {{ chondrite }},
        value = 10^value
        ) %>%
      dplyr::ungroup() %>%
      dplyr::rename_with(
        .cols = dplyr::matches("^glanced"),
        ~ stringr::str_replace_all(pattern = "glanced", replacement = "", string = .x)
      )
  }


  if (stopper >= 2) {
    dat <- dat %>% dplyr::bind_rows(., exluded_rows)
  }


  ## correction if output is LONG format ########################
  if (long_format == T) {
    if (method == 1) {
      if (Calibrate == T) {


      corrections <- data.frame(Element_name = c("La",
                                                 "Ce",
                                                 "Pr",
                                                 "Nd",
                                                 "Sm",
                                                 "Eu",
                                                 "Gd",
                                                 "Tb",
                                                 "Dy",
                                                 "Ho",
                                                 "Er",
                                                 "Tm",
                                                 "Yb",
                                                 "Lu",
                                                 "Y"
      ),
      correct = c(  1,
                    1,
                    Pr_correction_fact,
                    Nd_correction_fact,
                    Sm_correction_fact,
                    1,
                    Gd_correction_fact,
                    Tb_correction_fact,
                    Dy_correction_fact,
                    Ho_correction_fact,
                    Er_correction_fact,
                    Tm_correction_fact,
                    Yb_correction_fact,
                    Lu_correction_fact,
                    Y_correction_fact)

      )

      dat <- dplyr::left_join(dat, corrections,by = 'Element_name') %>%
                dplyr::mutate(NormalizedCalc = correct * NormalizedCalc,
                       ppmCalc =correct *ppmCalc ) %>% dplyr::select(-correct)

    }
    }
    }



## correction if output is wide format ########################
  if (long_format == F) {
    if (method == 1) {
      if (estimate_r0 == F) {
        dat <- dat %>%
          dplyr::select(-c(models, ShannonRadiiVIII_Coord_3plus, `(ri/3 + r0/6)(ri-r0)^2`, value, {{ chondrite }})) %>%
          tidyr::pivot_wider(names_from = Element_name, values_from = c(NormalizedCalc, ppmCalc)) %>%
          dplyr::relocate(rowid, model_nree, dplyr::matches("NormalizedCalc"), dplyr::matches("ppmCalc"))
      }

      if (estimate_r0 == T) {
        dat <- dat %>%
          dplyr::select(-c(models, ShannonRadiiVIII_Coord_3plus, `(ri/3 + r0/6)(ri-r0)^2`, value, {{ chondrite }})) %>%
          tidyr::pivot_wider(names_from = Element_name, values_from = c(NormalizedCalc, ppmCalc)) %>%
          dplyr::relocate(rowid, model_nree, r0, dplyr::matches("NormalizedCalc"), dplyr::matches("ppmCalc"))
      }

      # ## Correction Factor for Heavy  REE + Y #####

      if (method == 1) {
        if (Calibrate) {
          dat <- correct_heavy(
            dat = dat,
            Y_correction_fact = Y_correction_fact,
            Yb_correction_fact = Yb_correction_fact,
            Lu_correction_fact = Lu_correction_fact,
            Ho_correction_fact = Ho_correction_fact,
            Er_correction_fact = Er_correction_fact,
            Tm_correction_fact = Tm_correction_fact
          )
        }

        ## Correction Factor for Middle  REE  #####

        if (Calibrate) {
          dat <- correct_middle(
            dat = dat,
            Pr_correction_fact = Pr_correction_fact,
            Nd_correction_fact = Nd_correction_fact,
            Sm_correction_fact = Sm_correction_fact,
            Gd_correction_fact = Gd_correction_fact,
            Tb_correction_fact = Tb_correction_fact,
            Dy_correction_fact = Dy_correction_fact
          )
        }
      }

      dat <- dplyr::left_join(Original, dat, by = "rowid")
    }




    if (method == 2) {
      dat <- dat %>%
        dplyr::select(-c(models, ShannonRadiiVIII_Coord_3plus, value, Z_Zhong, {{ chondrite }})) %>%
        tidyr::pivot_wider(names_from = Element_name, values_from = c(NormalizedCalc, ppmCalc)) %>%
        dplyr::relocate(rowid, model_nree, dplyr::matches("NormalizedCalc"), dplyr::matches("ppmCalc"))

      dat <- dplyr::left_join(Original, dat, by = "rowid")
    }




    if (method == 3) {
      dat <- dat %>%
        dplyr::select(-c(models, ShannonRadiiVIII_Coord_3plus, value, {{ chondrite }})) %>%
        tidyr::pivot_wider(names_from = Element_name, values_from = c(NormalizedCalc, ppmCalc)) %>%
        dplyr::relocate(rowid, model_nree, dplyr::matches("NormalizedCalc"), dplyr::matches("ppmCalc"))


      if (method == 3) {
        if (Calibrate) {
          dat <- correct_heavy(
            dat = dat,
            Y_correction_fact = Y_correction_fact,
            Yb_correction_fact = Yb_correction_fact,
            Lu_correction_fact = Lu_correction_fact,
            Ho_correction_fact = Ho_correction_fact,
            Er_correction_fact = Er_correction_fact,
            Tm_correction_fact = Tm_correction_fact
          )
        }

 ## Correction Factor for Middle  REE  #####

        if (Calibrate) {
          dat <- correct_middle(
            dat = dat,
            Pr_correction_fact = Pr_correction_fact,
            Nd_correction_fact = Nd_correction_fact,
            Sm_correction_fact = Sm_correction_fact,
            Gd_correction_fact = Gd_correction_fact,
            Tb_correction_fact = Tb_correction_fact,
            Dy_correction_fact = Dy_correction_fact
          )
        }
      }

      dat <- dplyr::left_join(Original, dat, by = "rowid")
    }



  }






  return(dat)
}
