
library(rjags)
library(R2jags)
library(tidyverse)

dat_list <- readRDS("def_adj_data_hic.rds")
dat <- dat_list[[11]]
names(dat)

# dat_list_new <- readRDS("def_adj_data_hic_Nov19.rds")
# dat_list_old <- readRDS("def_adj_data_hic.rds")
# 
# dat_500g_new <- dat_list_new[[10]]
# dat_500g_old <- dat_list_old[[9]]
# dplyr::setdiff(dat_500g_new[,1:7], dat_500g_old[,1:7])

dat500 <- dat %>%
  select(iso, year, region, nSB, nLB, nSB28, definition_rv,
         definition_raw, ori_def28, nSB_adj_unknown) %>%
  rename(nsb = nSB, nsb28 = nSB28, lb = nLB, def = definition_rv)

dat500$nsb[dat500$nsb==0.5] <- 0
dat500$nsb28[dat500$nsb28==0.5] <- 0

# plot(nsb28~nsb, data=dat500); abline(0,1)
# dat500[which(dat500$nsb28 > dat500$nsb),] # only a few

# some of this doesn't really do anything b/c we don't have 500gAND28wks
# (just copied over from 1000g)
dat500g <- 
  dat500 %>% 
  mutate(
    # get ranges for b
    # min_intersect = floor(0.5*pmin(nsb, nsb28)),
    min_intersect = 0,
    max_intersect = pmin(nsb, nsb28),
    ncombis = max_intersect - min_intersect + 1
  ) %>%
  mutate(def = ifelse(def == "ge500g", "def_500", "def_intersect" )) %>%
  filter(!( (nsb > nsb28) & def == "def_intersect")) %>%
  arrange(def) 

n <- dim(dat500g)[1] # all obs are def_500
# n_500 = n

jags_data <- list(n = n,
                  nsb28 = dat500g$nsb28,
                  nsb = as.integer(dat500g$nsb),
                  zeroes = rep(0, n),
                  min_intersect = as.integer(dat500g$min_intersect[1:n]),
                  ncombis = as.integer(dat500g$ncombis[1:n])
)

mod_500g <- jags.model(file = "model_overlap_500g.txt",
                  data = jags_data,
                  n.chains = 3,
                  n.adapt = 5000)
# first chunk for testing
system.time(
  samp1 <- coda.samples(mod_500g, c("mu","sigma"), n.iter=100) # 7.24 sec/iter
)
summary(samp1)
plot(samp1)

# keep burning in
samp2 <- coda.samples(mod_500g, c("mu","sigma"), n.iter=900)
summary(samp2)
plot(samp2)
gelman.diag(samp2, autoburnin=T)
gelman.diag(samp2, autoburnin=F)
geweke.diag(samp2)

# actually sample
samp3 <- coda.samples(mod_500g, c("mu","sigma"), n.iter=2000)
summary(samp3)
plot(samp3)
gelman.diag(samp3, autoburnin=F)
saveRDS(samp3, "samp_500g.rds")
