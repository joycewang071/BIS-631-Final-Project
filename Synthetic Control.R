# install.packages('tidysynth')
# https://github.com/edunford/tidysynth

library(tidysynth)
library(tidyverse)


weibo_out <-
  weibo_week %>%
  
  # initial the synthetic control object
  synthetic_control(outcome = proportion,
                    unit = IP_May, 
                    time = week,
                    i_unit = "Shanghai", 
                    i_time = 13, 
                    generate_placebos=T 
                    # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights

  generate_predictor(time_window = 5:12,
                     female = mean(female, na.rm = T),
                     verified = mean(verified, na.rm = T)
                     ) %>%
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 5:12, # time to use in the optimization task
                   optimization_method ='Nelder-Mead',
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  # Generate the synthetic control
  generate_control()

avg_ATT <- grab_synthetic_control(weibo_out) |> 
  filter(time_unit > 12) |>
  mutate(dif = real_y - synth_y) |> 
  summarize(CE = mean(dif))

weibo_out%>% plot_trends()

weibo_out |> grab_unit_weights()

weibo_out |> grab_predictor_weights()

weibo_out |> plot_weights()
