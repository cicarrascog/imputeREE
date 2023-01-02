


library(devtools)
library(tidyverse)
load_all()
Zhong_data <- read.csv('C://Users/ccarr/Desktop/Zhong_data.csv')



test <- model_REE(dat = testing_data %>%  slice(1:10),prefix = 'Zr_', suffix = '_ppm',method = 1, estimate_r0 = T, long_format = T , correct_heavy = T, correct_middle = T)


test %>%  View()


test2 <-   model_REE(dat = Zhong_data, method = 1, estimate_r0 = T, long_format = T  )
test3 <-   model_REE(dat = Zhong_data, method = 2, long_format = T  )
test4 <-   model_REE(dat = Zhong_data, method = 1, estimate_r0 = F, long_format = T  )
test5 <-   model_REE(dat = Zhong_data, method = 1, estimate_r0 = F, long_format = T , correct_heavy = T, correct_middle = T)
test6 <-   model_REE(dat = Zhong_data, method = 1, estimate_r0 = T, long_format = T , correct_heavy = T, correct_middle = T)
# test6
test2 %>% ggplot(aes(x = value,NormalizedCalc , color = model_r.squared)) + geom_point() + scale_x_log10() + scale_y_log10(limits = c(10^-3,10^3)) + geom_abline()
test3 %>% ggplot(aes(x = 10^value,NormalizedCalc , color =  model_r.squared)) + geom_point()+ scale_x_log10() + scale_y_log10(limits = c(10^-3,10^3)) + geom_abline()
test4 %>% ggplot(aes(x = value,NormalizedCalc , color =  model_r.squared)) + geom_point()+ scale_x_log10() + scale_y_log10(limits = c(10^-3,10^3)) + geom_abline()
test5 %>% ggplot(aes(x = value,NormalizedCalc , color =  model_r.squared)) + geom_point()+ scale_x_log10() + scale_y_log10(limits = c(10^-3,10^3)) + geom_abline()
test6 %>% ggplot(aes(x = value,NormalizedCalc , color =  model_r.squared)) + geom_point()+ scale_x_log10() + scale_y_log10(limits = c(10^-3,10^3)) +
  geom_abline()

bind_rows(
  yardstick::rmse(test2, truth = value, estimate = NormalizedCalc),
  yardstick::rmse(test3, truth = value, estimate = NormalizedCalc),
  yardstick::rmse(test4, truth = value, estimate = NormalizedCalc),
  yardstick::rmse(test5, truth = value, estimate = NormalizedCalc),
  yardstick::rmse(test6, truth = value, estimate = NormalizedCalc)
) %>%  view()


