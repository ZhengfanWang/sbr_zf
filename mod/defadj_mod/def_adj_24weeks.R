
library(rjags)
library(R2jags)
library(superdiag)

dat_list <- readRDS("def_adj_data_hic.rds")
dat <- dat_list[[1]]

res <- dat %>%
  select(iso, year, region, nSB, nLB, nSB28, definition_rv) %>%
  rename(nsb = nSB, nsb28 = nSB28, lb = nLB, def = definition_rv)

#res %>%
#  ggplot(aes(nsb, nsb28, color = iso, shape = def)) +
#  geom_point()
# table(res$def)
# res[which(res$nsb < res$nsb28), ]

dat2 <- 
  res %>%
  filter(nsb > nsb28)
n <- dim(dat2)[1]

#dat2 %>%
#  ggplot(aes(nsb, nsb28, color = iso, shape = def)) +
#  geom_point()

jags_data <- list(n = n,
                  nsb28 = dat2$nsb28,
                  nsb = as.integer(dat2$nsb))

# testing
#mod_24 <- jags.model(file = "model_subset.txt",
#                  data = jags_data,
#                  n.chains = 4,
#                  n.adapt = 5000)
#samp_24 <- coda.samples(mod_24, c("mu","sigma","k_s"), n.iter = 100000)
#autocorr.plot(samp_24)
#superdiag(samp_24, burnin=1000)
#superdiag(samp_24, burnin=2500)

# actual
mod_24 <- jags.model(file = "model_subset.txt",
                     data = jags_data,
                     n.chains = 4,
                     n.adapt = 5000)
update(mod_24, 5000) # burn-in
samp_24 <- coda.samples(mod_24, c("mu","sigma","k_s"), n.iter = 100000, thin=5)
autocorr.plot(samp_24)

gelman.diag(samp_24, autoburnin = F)
geweke.diag(samp_24)

summary(samp_24)
pdf("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Model/SBR 2020/defadj/output/24wksRplots.pdf")
plot(samp_24)
dev.off()

saveRDS(samp_24, "C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Model/SBR 2020/defadj/output/samp_24_hic.rds")

summary(samp_24)$statistics[1,1:2]
summary(samp_24)$quantiles[1,"50%"]

#------------------------------------------------#

#dat.plot <- dat %>%
#  select(iso, country, year, region, nSB, nLB, nSB28, definition_rv,
#         SBR, SBR28) %>%
#  rename(nsb = nSB, nsb28 = nSB28, lb = nLB, def = definition_rv) %>%
#  filter(nsb > nsb28) %>%
#  mutate(logratio = log(SBR/SBR28))

#pdf("logratio_24wks.pdf",width=11)
#dat.plot %>%
#  ggplot(aes(SBR28, logratio, color = country)) +
#  theme_bw() +
#  geom_point(size=3) +
#  labs(title = "24 weeks", y = "log(SBR24/SBR28)") +
#  theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
#        axis.title.x = element_text(size=20),
#        axis.title.y = element_text(size=20),
#        legend.position = "bottom",
#        legend.title=element_blank())
#dev.off()
