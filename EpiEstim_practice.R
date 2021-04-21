library(EpiEstim)
library(ggplot2)
library(incidence)
data(Flu2009)

incidence <- Flu2009$incidence
plot(as.incidence(Flu2009$incidence$I, dates = Flu2009$incidence$dates))

dates <- incidence$dates
I <- incidence$I

t_start <- seq(2, nrow(Flu2009$incidence)-13)
t_end <- t_start + 13

res <- estimate_R(incidence, method = "non_parametric_si", config = make_config(list(si_distr = Flu2009$si_distr)))
plot(res)                  

si_distr <- Flu2009$si_distr
#

si_data <- Flu2009$si_data

res_noSI <- estimate_R(incidence, method = "si_from_data", si_data = )

res_parametricsi <- estimate_R(Flu2009$incidence, method = "parametric_si", config = make_config)