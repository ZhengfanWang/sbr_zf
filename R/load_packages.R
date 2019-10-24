library(openxlsx)

library(plyr)

library(tidyverse)

library(haven)

library(ggplot2)

library(splines)

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)