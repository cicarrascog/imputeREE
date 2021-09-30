Model_REE <- function(dat,
                      r0 = 0.84,
                      include_Y = TRUE,
                      exclude = c('La','Ce','Eu'),
                      preffix = NULL,
                      suffix = NULL
                      ) {


Original <- dat %>%  Add_ID() ## backup of original data.

dat <- dat %>%
       Element_norm('raw',
               preffix = preffix,
               suffix  = suffix) %>%
  dplyr::mutate(Element_name = stringr::str_remove(Element_name, '[:punct:]?Normalized')) %>%
  Add_IonicRadii()



### Select element data

if(include_Y == F){

  Element_list <- REE_Elements

} else {

  Element_list <- REE_plus_Y_Elements

  }

#
###
excludedREE <-  dat %>%
  dplyr::filter(stringr::str_detect(string = Element_name ,
                                    pattern = paste0(exclude, collapse = '|') ,
                                    negate = F
  )) %>%  dplyr::mutate(value = NA_real_)

dat <-  dat %>%
  dplyr::filter(stringr::str_detect(string = Element_name ,
                                    pattern = paste0(exclude, collapse = '|') ,
                                    negate = T
  ))
# #
dat <-  dplyr::bind_rows(dat, excludedREE)

model_nree <- dat %>% dplyr::filter(!is.na(value)) %>%
  dplyr::group_by({{group}}) %>%
  dplyr::summarise(nree = sum(!is.na(Element_name)))

lessthan3REE <- model_nree %>% dplyr::ungroup() %>%  dplyr::filter(nree < 4) %>% nrow()

warning('There are ', lessthan3REE, ' Samples with less than 3 or less REE to model, consider filtering that data')


dat <- dplyr::left_join(dat, model_nree, by = 'rowid')

dat <- dat %>%
  dplyr::mutate(
    `(ri/3 + r0/6)(ri-r0)^2` = (ShannonRadiiVIII_Coord_3plus / 3 + r0/6)*(ShannonRadiiVIII_Coord_3plus -r0)^2)

dat <- dat %>%
  dplyr::group_by({{group}}) %>%
  tidyr::nest() %>%
  dplyr::mutate(
  models = purrr::map(data,   ~lm(log(value)~`(ri/3 + r0/6)(ri-r0)^2`, na.action = na.omit, data = .x)),
  tidied =   purrr::map(models, broom::tidy),
  glanced = purrr::map(models, broom::glance)
) %>%
  tidyr::unnest(tidied) %>%
  dplyr::mutate(term = ifelse(stringr::str_detect('Intercept', term), 'Intercept', 'Slope')) %>%
  tidyr::pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>%
  tidyr::unnest(glanced, names_sep = 'model_') %>%
  tidyr::unnest(data) %>%
  dplyr::mutate(Element_name = paste0(Element_name, 'Calc'),
                value = exp(`(ri/3 + r0/6)(ri-r0)^2` * estimate_Slope+ estimate_Intercept )) %>%
  dplyr::ungroup() %>%
   dplyr::rename_with(.cols = dplyr::matches('^glanced'), ~stringr::str_replace_all(pattern = 'glanced', replacement = '', string = .x)) %>%
  dplyr::select(-c(models, ShannonRadiiVIII_Coord_3plus,`(ri/3 + r0/6)(ri-r0)^2` )) %>%
  tidyr::pivot_wider(names_from = Element_name, values_from = value) %>%
  dplyr::relocate({{group}}, nree, dplyr::matches(REE_plus_Y_Elements, ignore.case = FALSE))
#   dplyr::relocate({{group}}, nree, !dplyr::matches(Element_list))
# #   dplyr::relocate(!dplyr::matches('Intercept'))
# # dplyr::relocate(!dplyr::matches('Slope'))


dat <- dplyr::left_join(Original, dat, by = 'rowid' )

 return(dat)
}

# model_ree ####################################################################

## This function quickly takes the data from REE_norm() function and model the REE using a nested Table.
# Arguments:
# dat: a data frame in long data format from the REE_norm() function
# group: unique identification for each observation in the long data. By default is ID.




