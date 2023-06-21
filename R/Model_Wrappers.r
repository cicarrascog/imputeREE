#' Model REE contents using the method of \href{https://link.springer.com/article/10.1007/s00710-019-00682-y}{Zhong et al. (2019)}
#'
#' This function apply the logarithmic regression using the method of \href{https://link.springer.com/article/10.1007/s00710-019-00682-y}{Zhong et al. (2019)}. This method considers the relationship between the logarithm of the REE atomic number vs their chondrite normalized values. For more information refer to the \href{https://link.springer.com/article/10.1007/s00710-019-00682-y}{Zhong et al. (2019)} and \href{https://link.springer.com/article/10.1007/s00410-023-02025-9}{Carrasco-Godoy and Campbell (2023)} for a discussion of its limitations to calculate La or Ce*.
#'
#' @inheritParams model_REE
#' @family model REE
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' Ballard_et_al_Zircon %>%  modelZhong(prefix = 'Zr', suffix = 'ppm')



modelZhong <- function(dat,
                       exclude = c("La", "Pr", "Ce", "Eu", "Y"),
                       Calibrate = F,
                       chondrite = PalmeOneill2014CI,
                       prefix= NULL,
                       suffix = NULL) {

  model_REE(dat = dat,
            method = 2,
            chondrite={{chondrite}},
            exclude = exclude,
            Calibrate = Calibrate,
            prefix = prefix,
            suffix = suffix)
}


#' Model REE contents using the Chondrite-Lattice method of \href{https://link.springer.com/article/10.1007/s00410-023-02025-9}{Carrasco-Godoy and Campbell (2023)}
#'

#' This function apply the Chondrite-Lattice method which is a linear regression between the misfit parameter from the lattice strain equation and the logarithm of their chondrite normalized values. At least 2 points are required to use this method.
#' This method is based on the work of  \href{https://www.nature.com/articles/372452a0}{Blundy and Wood (1994)} but using chondrite normalized values as noted by \href{https://link.springer.com/article/10.1007/s00410-023-02025-9}{Carrasco-Godoy and Campbell (2023)}. Refer to \href{https://link.springer.com/article/10.1007/s00410-023-02025-9}{Carrasco-Godoy and Campbell (2023)} for details.
#'
#' @inheritParams model_REE
#' @family model REE
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' Ballard_et_al_Zircon %>%  modelChondrite_lattice(prefix = 'Zr', suffix = 'ppm')



modelChondrite_lattice <- function(dat,
                                   exclude = c("La", "Pr", "Ce", "Eu", "Y"),
                                   Calibrate = T,
                                   prefix= NULL,
                                   suffix = NULL,
                                   r0= 0.87,
                                   chondrite = PalmeOneill2014CI,
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

  model_REE(dat = dat,
            method = 1,
            exclude = exclude,
            chondrite={{chondrite}},
            Calibrate = Calibrate,
            prefix = prefix,
            suffix = suffix,
            r0 = r0,
            estimate_r0 = F,
            Pr_correction_fact = Pr_correction_fact ,
            Y_correction_fact  = Y_correction_fact  ,
            Dy_correction_fact = Dy_correction_fact ,
            Ho_correction_fact = Ho_correction_fact ,
            Er_correction_fact = Er_correction_fact ,
            Tm_correction_fact = Tm_correction_fact ,
            Yb_correction_fact = Yb_correction_fact ,
            Lu_correction_fact = Lu_correction_fact ,
            Nd_correction_fact = Nd_correction_fact ,
            Sm_correction_fact = Sm_correction_fact ,
            Gd_correction_fact = Gd_correction_fact ,
            Tb_correction_fact = Tb_correction_fact )
}


#' Model REE contents using the Chondrite-Onuma method of \href{https://link.springer.com/article/10.1007/s00410-023-02025-9}{Carrasco-Godoy and Campbell (2023)}
#'
#' This function apply the Chondrite-Onuma method which is a quadratic regression between the ionic radius of the REE and the logarithm of their chondrite normalized values. At least 3 non-linear points are required to use this method.
#'This method is based on the work of  \href{https://www.sciencedirect.com/science/article/abs/pii/S0012821X6880010X}{Onuma et al. (1968)} but using chondrite normalized values as noted by \href{https://link.springer.com/article/10.1007/s00410-023-02025-9}{Carrasco-Godoy and Campbell (2023)}. Refer to \href{https://link.springer.com/article/10.1007/s00410-023-02025-9}{Carrasco-Godoy and Campbell (2023)} for details.
#'
#' @inheritParams model_REE
#' @family model REE
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' Ballard_et_al_Zircon %>%  modelChondrite_Onuma(prefix = 'Zr', suffix = 'ppm')


modelChondrite_Onuma <-
  function(dat,
           exclude = c("La", "Pr", "Ce", "Eu", "Y"),
           Calibrate = T,
           chondrite = PalmeOneill2014CI,
           prefix= NULL,
           suffix = NULL,
           Pr_correction_fact = 1/1,
           Nd_correction_fact = 1/1.026486418,
           Sm_correction_fact = 1/0.971111041,
           Gd_correction_fact = 1/0.959282410,
           Tb_correction_fact = 1/1.000985745,
           Dy_correction_fact = 1/1.030049321,
           Ho_correction_fact = 1/1.018711009,
           Er_correction_fact = 1/0.996610693,
           Tm_correction_fact = 1/1.053205463,
           Yb_correction_fact = 1/0.982656111,
           Lu_correction_fact = 1/0.952608321,
           Y_correction_fact = 1/0.665380561) {

  model_REE(dat = dat,
            method = 3,
            exclude = exclude,
            Calibrate = Calibrate,
            chondrite={{chondrite}},
            prefix = prefix,
            suffix = suffix,
            Pr_correction_fact = Pr_correction_fact ,
            Y_correction_fact  = Y_correction_fact  ,
            Dy_correction_fact = Dy_correction_fact ,
            Ho_correction_fact = Ho_correction_fact ,
            Er_correction_fact = Er_correction_fact ,
            Tm_correction_fact = Tm_correction_fact ,
            Yb_correction_fact = Yb_correction_fact ,
            Lu_correction_fact = Lu_correction_fact ,
            Nd_correction_fact = Nd_correction_fact ,
            Sm_correction_fact = Sm_correction_fact ,
            Gd_correction_fact = Gd_correction_fact ,
            Tb_correction_fact = Tb_correction_fact )
}
