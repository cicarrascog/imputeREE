#' Corrects for the model deviations of Yb, Lu and Y
#'
#' Calculated value of Yb, Lu and Y slightly deviates from the linear regression. This function apply a correction to compensates those deviations. This function is wrapped inside model_REE()
#'
#' @param dat A dataframe
#' @param Y_correction_fact a number: correction factor for underestimated Y. 1.29 by default.
#' @param Yb_correction_fact a number: correction factor for underestimated Yb 1/0.8785
#' @param Lu_correction_fact a number: correction factor for underestimated Lu 1/0.8943
#'
#' @return a data frame

correct_heavy <- function(dat,
                           Y_correction_fact = 1.29,
                           Yb_correction_fact = 1/0.8785,
                           Lu_correction_fact = 1/0.8943
){

dat <- dat %>%
  dplyr::mutate(
    ppmCalc_Y =  .data$ppmCalc_Y * Y_correction_fact,
    NormalizedCalc_Y = .data$NormalizedCalc_Y * Y_correction_fact,
    ppmCalc_Yb =  .data$ppmCalc_Yb * Yb_correction_fact,
    NormalizedCalc_Yb = .data$NormalizedCalc_Yb *Yb_correction_fact,
    ppmCalc_Lu =  .data$ppmCalc_Lu * Lu_correction_fact,
    NormalizedCalc_Lu = .data$NormalizedCalc_Lu *Lu_correction_fact
  )
  return(dat)
}

