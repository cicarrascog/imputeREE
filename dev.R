load_all()
data <- testing_data %>%  Model_REE(preffix = 'Zr', suffix = 'ppm')
test <- data %>%  Imputate_REE('Zr', 'ppm')

# Ce_Eudata <- test %>%
#   CleanColnames('Zr', 'ppm') %>%
#   dplyr::select(rowid, Eu, Ce) %>%
#   tidyr::pivot_longer(-rowid, names_to = 'Element_name')
# Ce_Eudata

Others_REE_plus_y <-
  test %>%
  dplyr::select(rowid, dplyr::matches('Imputated')) %>%
    tidyr::pivot_longer(-rowid, names_to = 'Element_name') %>%
    dplyr::mutate(Element_name = stringr::str_remove(Element_name, '^Imputated_'))

test2 <- dplyr::bind_rows(Ce_Eudata, Others_REE_plus_y)  %>%
  Add_Element_data() %>%
  dplyr::select(1:3, Atomic_Mass) %>%
  dplyr::mutate(REE_plus_Y_molar = (value/Atomic_Mass)) %>%  dplyr::group_by(rowid) %>%
  dplyr::summarise(REE_plus_Y_molar= sum(REE_plus_Y_molar))



  test2 %>% dplyr::left_join( test,., by='rowid')

  load_all()
  # test %>%  Add_parameters() %>%  dplyr::glimpse()
testing_data %>%  DoItALL(preffix  = 'Zr', suffix = 'ppm') %>%  dplyr::glimpse()

