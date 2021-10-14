data <- testing_data %>%  Model_REE(preffix = 'Zr', suffix = 'ppm')
# data %>% dplyr::glimpse()


data2 <- data %>% CleanColnames(preffix = 'Zr', suffix = 'ppm')

originalData <- data2 %>%  dplyr::select(rowid, dplyr::matches(paste0('^',REE_plus_Y_Elements,'$') , ignore.case = F)) %>% tidyr::pivot_longer(cols = -rowid, names_to = 'Element', values_to = 'values')


calc_Data <- data2 %>% CleanColnames(preffix = 'Zr', suffix = 'ppm') %>%  dplyr::select(rowid, dplyr::matches('^ppmCalc'))


rsquared_data <- data2 %>%   dplyr::select(rowid, model_r.squared )

calc_Data <- calc_Data %>%  CleanColnames(preffix = 'ppmCalc') %>% tidyr::pivot_longer(cols = -rowid, names_to = 'Element', values_to = 'calc_value') %>%  dplyr::left_join(., rsquared_data, by = 'rowid') %>%  dplyr::filter(model_r.squared >0.9)

originalData %>% dplyr::left_join(., calc_Data, by = c('rowid',"Element")) %>% dplyr::arrange(desc(is.na(values))) %>%  dplyr::mutate(imputated_values = ifelse(is.na(values), calc_value, values)) %>%  dplyr::mutate(Element = paste0('Imputated_', Element)) %>% dplyr::select(rowid, Element, imputated_values) %>% tidyr::pivot_wider( names_from = 'Element', values_from = 'imputated_values')


load_all()
data %>%  Imputate_REE('Zr', 'ppm') %>% View()
