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


dat <-  dplyr::bind_rows(dat, excludedREE) ### add excluded REE as NA, since they are not modelled.

model_nree <- dat %>% dplyr::filter(!is.na(value)) %>%
  dplyr::group_by(rowid) %>%
  dplyr::summarise(model_nree = sum(!is.na(value) ))

dat <- dplyr::left_join(dat, model_nree, by = 'rowid')

#### warning about samples with 3 data points for modelling

lessthan3REE <- model_nree %>% dplyr::ungroup() %>%  dplyr::filter(model_nree <  4) %>% nrow()

warning('There are ', lessthan3REE, ' Samples with less than 3 or less REE to model, consider filtering that data, or including more elements')








#### warning about samples with 3 data points for modelling
stopper <- dat %>% dplyr::ungroup() %>%  dplyr::filter(is.na(model_nree)|model_nree == 1) %>% nrow()

if(stopper >= 2 ) {

  stopper <- dat %>%
    dplyr::ungroup() %>%
    dplyr::arrange(model_nree) %>%
    dplyr::filter(is.na(model_nree) | model_nree <= 2) %>%
    dplyr::select(rowid) %>%
    dplyr::distinct()

  stopper <- paste0(stopper$rowid, collapse = ' ,  ')

  warning('rowid: ', stopper[1] , '\n do not have enough data for modelling. They have been excluded from the modelling.')

  exluded_rows <- dat %>%
    dplyr::ungroup() %>%
    dplyr::arrange(model_nree) %>%
    dplyr::filter(is.na(model_nree) | model_nree <= 2)

  dat <- dat %>%
     dplyr::filter(!is.na(model_nree) & model_nree > 2)

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




