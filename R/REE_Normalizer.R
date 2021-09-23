# REE_norm #####################################################################

# This function takes a table as input with REE values as ppm as input.
# It returns a table with condrite normalized values for REE.
# For now, it only considers the values of McDonough, W. F. & Sun, S. -s.
# The composition of the Earth. Chemical Geology 120, 223–253 (1995).
# Arguments :
# dat : a data frame with zircon REE data in ppm.
#  pivot_longer: a logical that tells the function to return data in a long format (useful for REE diagrams), or wide format (useful for binary plots)
# join: a logical that determines if other columns in the input should be kept or not.


REE_norm <- function(dat, pivot_longer = T, join = F) {
  dat <- check_ID(dat)
  original <- dat
  source('code/final_code/Constant_data.R')
  dat <- dat %>%
    select(ID, all_of(REE_Y_group)) %>% # Select all the columns with REE-Y plus the ID column
    pivot_longer(-ID, names_to = "Element_Labels") %>% # makes data long, so it is easier to calculate
    left_join(., REE_database, by = "Element_Labels") %>% # left join the data from
    mutate(Element_Labels = paste0(Element_Labels, "_c"),
           norm_values = ifelse(Element_Labels == 0, NA, value / Sun_and_mc_1995)) %>%
    select(-names(select(REE_database, -Element_Labels))) #

  if (pivot_longer == F) {
    dat <- dat %>%
      select(-value) %>%
      pivot_wider(id_cols = ID, names_from = Element_Labels, values_from = norm_values)
  }

  if (join == T) {
    dat <- left_join(original, dat, by = "ID")
  }

  dat
}
