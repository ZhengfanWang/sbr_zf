
library(rjags)
library(R2jags)
library(tidyverse)

# dat_list <- readRDS("def_adj_data_hic_old.rds")
# dat <- rbind(dat_list[[4]], dat_list[[5]]) #ge1000g, ge1000gANDge28wks

dat_list <- readRDS("def_adj_data_hic.rds")
dat <- rbind(dat_list[[6]], dat_list[[7]]) #ge1000g, ge1000gANDge28wks
names(dat)

res <- dat %>%
  select(iso, year, region, nSB, nLB, nSB28, definition_rv,
         definition_raw, ori_def28, nSB_adj_unknown) %>%
  rename(nsb = nSB, nsb28 = nSB28, lb = nLB, def = definition_rv) %>%
  filter(!is.na(nsb)) # 35 observations are missing nsb

res %>%
  ggplot(aes(nsb, nsb28, color = iso, shape = def)) +
  geom_point()

res2 <- res
res2$nsb[res2$nsb==0.5] <- 0
res2$nsb28[res2$nsb28==0.5] <- 0

dat2 <- 
  res2 %>%
  mutate(
    # get ranges for b
    # min_intersect = floor(0.5*pmin(nsb, nsb28)),
    min_intersect = 0,
    max_intersect = pmin(nsb, nsb28),
    ncombis = max_intersect - min_intersect + 1
  ) %>%
  mutate(def = ifelse(def == "ge1000g", "def_1000", "def_intersect" )) %>%
  filter(!( (nsb > nsb28) & def == "def_intersect")) %>%
  arrange(def) # for jags, just have def_1000 first 

n <- dim(dat2)[1]
n_1000 <- sum(dat2$def == "def_1000")

jags_data <- list(n = n,
                  n_1000 = n_1000,
                  nsb28 = dat2$nsb28,
                  nsb = as.integer(dat2$nsb),
                  zeroes = rep(0, n_1000),
                  min_intersect = as.integer(dat2$min_intersect[1:n_1000]),
                  ncombis = as.integer(dat2$ncombis[1:n_1000])
)

# Hi Lucia, here's an example of what I would do
# but I don't know how it will change when parallelizing

mod_1000g <- jags.model(file = "model_overlap_1000g.txt",
                  data = jags_data,
                  n.chains = 4,
                  n.adapt = 5000)
update(mod_1000g, 5000) # burn-in (optional)
samp_1000g <- coda.samples(mod_1000g, c("mu","sigma"), n.iter=100000)


#### below is old code where I sampled in chunks ####

mod <- jags.model(file = "model_overlap_1000g.txt",
                  data = jags_data,
                  n.chains = 3,
                  n.adapt = 2500)
test <- coda.samples(mod, c("mu","sigma"), n.iter = 100)
plot(test)
samp_full <- coda.samples(mod, c("mu","sigma"), n.iter = 10000)
summary(samp_full)
plot(samp_full)

# starts where we left off
test <- coda.samples(mod, c("mu","sigma"), n.iter=100)
summary(test)
plot(test)

system.time(
  samp_more <- coda.samples(mod, c("mu","sigma"), n.iter=300)
)

plot(samp_more)

system.time(
  samp_final <- coda.samples(mod, c("mu","sigma"), n.iter=5000)
)

summary(samp_final)

pdf("samp_1000g.pdf", width=9)
plot(samp_final)
dev.off()

# LA's way of running
# mod <- jags(data = jags_data,
#                      parameters.to.save = c("p", "mu", "sigma"),
#                      model.file = "model_overlap5.txt")
# LA still uses kappa in model file here
# saveRDS(mod, "output/mod.rds")
# mod$BUGSoutput$summary[c("mu", "sigma"),]
