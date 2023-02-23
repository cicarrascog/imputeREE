#' Element data for calculations
#'
#' A dataset containing CI and Mantle values for normalization for selected elements. The data used is from IUPAC, Palme and O'Neill (2014), and McDonough and Sun (1995). Ionic Radii are from Shannon (1976).
#'
#'
#' @format A data frame with 77 rows and 11 variables:
#' \describe{
#'   \item{Z}{Atomic Number}
#'   \item{Element_name}{Element Symbol}
#'   \item{Atomic_Mass}{Atomic Mass from IUPAC}
#'   \item{Unit}{Measure Unit of the Concentrations, ppm = parts per million, pct = percentage}
#'   \item{PalmeOneill2014CI}{Chondrite values from Palme and Oneil (2014)}
#'   \item{PalmeOneill2014CI_RSD}{Uncertainty from chondrite values from Palme and O'Neill (2014) as RSD (Relative standard Deviation)}
#'   \item{PalmeOneill2014Mantle}{Primitive Mantle values from Palme and O'Neill (2014)}
#'   \item{PalmeOneill2014Mantle_RSD}{Uncertainty from Primitive Mantle Values from Palme and O'Neill (2014) as RSD (Relative standard Deviation)}
#'   \item{McDonough1995CI}{Chondrite values from McDonough and Sun (1995)}
#'   \item{ShannonRadiiVIII_Coord_3plus}{Shannon (1976) Ionic Radii for elements in Eight-fold coordination and 3+ charge}
#'   \item{Z_Zhong}{numbers assigned by Zhong et al. (2019) for a logarithmic regression to calculate Zircon REE.}
#'   ...
#' }
#' @source IUPAC Website (\url{https://iupac.org/})
#' @source Palme, H., and O’Neill, H. St. C., 2014, 3.1 - Cosmochemical Estimates of Mantle Composition, in Holland, H. D. and Turekian, K. K. eds., Treatise on Geochemistry (Second Edition): Oxford, Elsevier, p. 1-39.(\doi{https://doi.org/10.1016/B978-0-08-095975-7.00201-1})
#' @source McDonough, W. F., and Sun, S. -s., 1995, The composition of the Earth: Chemical Geology, v. 120, p. 223-253.(\doi{https://doi.org/10.1016/0009-2541(94)00140-4})
#' @source Shannon, R. D., 1976, Revised effective ionic radii and systematic studies of interatomic distances in halides and chalcogenides: Acta Crystallographica Section A, v. 32, p. 751-767. \doi{ https://doi.org/10.1107/S0567739476001551}
#' @source Shannon, R. D., 1976, Revised effective ionic radii and systematic studies of interatomic distances in halides and chalcogenides: Acta Crystallographica Section A, v. 32, p. 751-767. \doi{ https://doi.org/10.1107/S0567739476001551}
#'
#' @source Zhong, S., Seltmann, R., Qu, H., and Song, Y., 2019, Characterization of the zircon Ce anomaly for estimation of oxidation state of magmas: a revised Ce/Ce* method: Mineralogy and Petrology, v. 113, no. 6, p. 755–763. \doi{10.1007/s00710-019-00682-y}

#'
"Element_Data"
