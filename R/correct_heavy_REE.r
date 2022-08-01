#' Corrects for the model deviations of Er, Yb, Lu and Y
#'
#' Calculated value of Yb, Lu and Y slightly deviates from the linear regression. This function apply a correction to compensates those deviations. This function is wrapped inside model_REE()
#'
#' @param dat A dataframe
#' @param Y_correction_fact a number: correction factor for underestimated Y. 1/ 0.72 by default.
#' @param Ho_correction_fact a number: correction factor for Ho. 1 by default.
#' @param Er_correction_fact a number: correction factor for underestimated Er. 1/0.974 by default.
#' @param Tm_correction_fact a number: correction factor for Tm. 1 by default.
#' @param Yb_correction_fact a number: correction factor for underestimated Yb. 1/0.907  by default.
#' @param Lu_correction_fact a number: correction factor for underestimated Lu. 1/0.926 by default.
#'
#' @return a data frame

correct_heavy <- function(dat,
                           Y_correction_fact = 1/0.72,
                           Ho_correction_fact = 1,
                           Er_correction_fact = 1/0.974,
                           Tm_correction_fact = 1,
                           Yb_correction_fact = 1/0.907,
                           Lu_correction_fact = 1/0.926
){

dat <- dat %>%
  dplyr::mutate(
    ppmCalc_Y =  .data$ppmCalc_Y * Y_correction_fact,
    NormalizedCalc_Y = .data$NormalizedCalc_Y * Y_correction_fact,

    ppmCalc_Ho =  .data$ppmCalc_Ho * Ho_correction_fact,
    NormalizedCalc_Ho = .data$NormalizedCalc_Ho * Ho_correction_fact,

    ppmCalc_Er =  .data$ppmCalc_Er * Er_correction_fact,
    NormalizedCalc_Er = .data$NormalizedCalc_Er * Er_correction_fact,

    ppmCalc_Yb =  .data$ppmCalc_Yb * Yb_correction_fact,
    NormalizedCalc_Yb = .data$NormalizedCalc_Yb *Yb_correction_fact,

    ppmCalc_Tm =  .data$ppmCalc_Tm * Tm_correction_fact,
    NormalizedCalc_Tm = .data$NormalizedCalc_Tm * Tm_correction_fact,

    ppmCalc_Lu =  .data$ppmCalc_Lu * Lu_correction_fact,
    NormalizedCalc_Lu = .data$NormalizedCalc_Lu *Lu_correction_fact
  )
  return(dat)
}

