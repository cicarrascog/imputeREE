#' Corrects for the model deviations of Yb, Lu and Y
#'
#' Calculated value of Yb, Lu and Y slightly deviates from the linear regression. This function apply a correction to compensates those deviations. This function is wrapped inside model_REE()
#'
#' @param Pr_correction_fact  a number: correction factor for overestimated Pr 1/0.918
#' @param Nd_correction_fact  a number: correction factor for underestimated Nd 1/0.0.989
#' @param Sm_correction_fact  a number: correction factor for overestimated Sm 1/1.022
#' @param Gd_correction_fact  a number: correction factor for overestimated Gd 1/1.033
#' @param Tb_correction_fact  a number: correction factor for overestimated Tb 1/1.050
#' @param Dy_correction_fact  a number: correction factor for overestimated Dy 1/1.032
#'
#' @param dat A dataframe
#'
#' @return a data frame

correct_middle <- function(dat,
                          Nd_correction_fact = 1/0.989,
                          Sm_correction_fact = 1/1.022,
                          Gd_correction_fact = 1/1.033,
                          Tb_correction_fact = 1/1.050,
                          Dy_correction_fact = 1/1.032,
                          Pr_correction_fact = 1/0.918
){

  dat <- dat %>%
    dplyr::mutate(
      ppmCalc_Pr =  .data$ppmCalc_Pr * Pr_correction_fact,
      NormalizedCalc_Pr = .data$NormalizedCalc_Pr * Pr_correction_fact,

      ppmCalc_Nd =  .data$ppmCalc_Nd * Nd_correction_fact,
      NormalizedCalc_Nd = .data$NormalizedCalc_Nd * Nd_correction_fact,

      ppmCalc_Sm =  .data$ppmCalc_Sm * Sm_correction_fact,
      NormalizedCalc_Sm = .data$NormalizedCalc_Sm *Sm_correction_fact,

      ppmCalc_Gd =  .data$ppmCalc_Gd * Gd_correction_fact,
      NormalizedCalc_Gd = .data$NormalizedCalc_Gd *Gd_correction_fact ,

      ppmCalc_Tb =  .data$ppmCalc_Tb * Tb_correction_fact,
      NormalizedCalc_Tb = .data$NormalizedCalc_Tb *Tb_correction_fact,

      ppmCalc_Dy =  .data$ppmCalc_Dy * Dy_correction_fact,
      NormalizedCalc_Dy = .data$NormalizedCalc_Dy *Dy_correction_fact
    )
  return(dat)
}
