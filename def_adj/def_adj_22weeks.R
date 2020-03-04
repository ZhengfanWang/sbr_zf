
library(rjags)
library(R2jags)
library(tidyverse)

dat_list <- readRDS("def_adj_data_hic.rds")
dat <- dat_list[[9]] # ge22wks
names(dat)

res <- dat %>%
  select(iso, year, uniqueID, region, nSB, nLB, nSB28, definition_rv,
         definition_raw, ori_def28) %>%
  rename(nsb = nSB, nsb28 = nSB28, lb = nLB, def = definition_rv)

# res$nsb28[253] <- 484 # corrected NLD 2014
head(res)

res[which(res$nsb < res$nsb28), ]
which(is.na(res$nsb))
which(is.na(res$nsb28))

dat2 <- res %>% filter(nsb >= nsb28) 
which(is.na(dat2$nsb)) # also removes NAs

dat2 %>%
  ggplot(aes(nsb, nsb28, color = iso, shape = def)) +
  geom_point()
plot(log(nsb28)~log(nsb), data=dat2); abline(0,1)

n <- dim(dat2)[1]
jags_data <- list(n = n,
                  nsb28 = dat2$nsb28,
                  nsb = as.integer(dat2$nsb))

mod_22_LA <- jags.model(file = "model_subset.txt",
                        data = jags_data,
                        n.chains = 10,
                        n.adapt = 5000)
update(mod_22_LA, 5000)
samp_22_LA <- coda.samples(mod_22_LA, c("mu","sigma","k_s"), n.iter = 50000, thin=20)
summary(samp_22_LA)
plot(samp_22_LA)
summary(samp_22_LA)$quantiles[1,"50%"]
summary(samp_22_LA)$statistics[1,2]
# autocorr.plot(samp_22_LA) # no problem

mod_22_hic <- jags.model(file = "model_subset.txt",
                  data = jags_data,
                  n.chains = 5,
                  n.adapt = 5000)
update(mod_22_hic, 5000)
samp_22_hic <- coda.samples(mod_22_hic, c("mu","sigma","k_s"), n.iter = 100000, thin=10)
# autocorr.plot(samp_22_hic) # also fine
summary(samp_22_hic)
plot(samp_22_hic)
summary(samp_22_hic)$quantiles[1,"50%"]
summary(samp_22_hic)$statistics[1,2]

# gelman.diag(samp_22_hic, autoburnin = F)
# geweke.diag(samp_22_hic)

save(samp_22_LA, samp_22_hic, file="samp_22.Rdata")


#------------------------------------------------#

dat.plot <- dat %>%
  select(iso, country, year, region, nSB, nLB, nSB28, definition_rv,
         definition_raw, ori_def28, SBR, SBR28) %>%
  rename(nsb = nSB, nsb28 = nSB28, lb = nLB, def = definition_rv)

which(dat.plot$SBR < dat.plot$SBR28)
dat.plot[260, ] # nsb > nsb28
dat.plot$SBR28[260] <- 136/59937*1000 # see email re: Norway 2015

dat.plot <- dat.plot %>% mutate(logratio = log(SBR/SBR28))

pdf("logratio_22wks_hic.pdf",width=11)
dat.plot %>%
  ggplot(aes(SBR28, logratio, color = country)) +
  theme_bw() +
  geom_point(size=3) +
  labs(title = "22 weeks HIC", y = "log(SBR22/SBR28)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.position = "bottom",
        legend.title=element_blank())
dev.off()
