
hq.lmic.def.ori <- openxlsx::read.xlsx("High Quality LMIC data_Definition.xlsx",sheet=1)

hq.lmic.def <- hq.lmic.def.ori %>% 
  mutate(definition_rv = "ge22wks",definition_rv2 = "ge22wks",
         definition_raw = "ge22wks", definition= "ge22wks",
         ori_def28 = "ge28wks", nSB28 = sb_28wks,SBR28 = sbr_28wks,
         SBR = sbr_22wks, nSB = sb_22wks, nLB =lb, source = seriesname) %>%
  mutate(nTB = nLB + nSB) %>%  
  rename("iso" = "iso3","year"="reference_year") %>%
  select(country,iso,year,nSB,SBR,nSB28,SBR28, nLB,
         definition_rv, definition_rv2, definition_raw,definition,
         ori_def28, source) %>%
  arrange(country, year)

dat <- hq.lmic.def %>%
  select(iso, year, nSB, nLB, nSB28, definition_rv,
         definition_raw, ori_def28) %>%
  rename(nsb = nSB, nsb28 = nSB28, lb = nLB, def = definition_rv)

dat %>%
  ggplot(aes(nsb, nsb28, color = iso, shape = def)) +
  geom_point()
plot(log(nsb28)~log(nsb), data=dat); abline(0,1)

dat2 <- dat

n <- dim(dat2)[1]
jags_data <- list(n = n,
                  nsb28 = dat2$nsb28,
                  nsb = as.integer(dat2$nsb))

mod_22_lmic <- jags.model(file = "model_subset.txt",
                  data = jags_data,
                  n.chains = 4,
                  n.adapt = 5000)
update(mod_22_lmic, 5000)
samp_22_lmic <- coda.samples(mod_22_lmic, c("mu","sigma","k_s"), n.iter = 100000, thin=5)

autocorr.plot(samp_22_lmic)

gelman.diag(samp_22_lmic, autoburnin = F)
geweke.diag(samp_22_lmic)

saveRDS(samp_22_lmic, "samp_22_lmic.rds")

summary(samp_22_lmic)
plot(samp_22_lmic)

#------------------------------------------------#

dat.plot <- hq.lmic.def %>%
  select(iso, country, year, nSB, nLB, nSB28, definition_rv,
         definition_raw, ori_def28, SBR, SBR28) %>%
  rename(nsb = nSB, nsb28 = nSB28, lb = nLB, def = definition_rv) %>%
  mutate(logratio = log(SBR/SBR28))

pdf("logratio_22wks_lmic.pdf",width=11)
dat.plot %>%
  ggplot(aes(SBR28, logratio, color = country)) +
  theme_bw() +
  geom_point(size=3) +
  labs(title = "22 weeks LMIC", y = "log(SBR22/SBR28)") +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.position = "bottom",
        legend.title=element_blank())
dev.off()
