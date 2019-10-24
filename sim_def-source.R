

# exploratory
SBR.plot %>% filter(SBR !=0, definition=="ge28wks",
                    context2=="admin VR" | context2=="subnat.admin VR") %>%
  mutate(log_SBR = log(SBR)) %>%
  summarise(mean = mean(log_SBR), sd=sd(log_SBR), n=n())
SBR.plot %>% filter(SBR !=0, definition=="ge28wks", context2=="admin VR") %>%
  mutate(log_SBR = log(SBR)) %>%
  summarise(mean = mean(log_SBR), sd = sd(log_SBR), n=n())
SBR.plot %>% filter(SBR !=0, definition=="ge28wks", context2=="subnat.admin VR") %>%
  mutate(log_SBR = log(SBR)) %>%
  summarise(mean = mean(log_SBR), sd = sd(log_SBR), n=n())
SBR.plot %>% filter(SBR !=0, definition=="ge22wks",
                    context2=="admin VR" | context2=="subnat.admin VR") %>%
  mutate(log_SBR = log(SBR)) %>%
  summarise(mean = mean(log_SBR), sd=sd(log_SBR), n=n())
SBR.plot %>% filter(SBR !=0, definition=="ge22wks", context2=="admin VR") %>%
  mutate(log_SBR = log(SBR)) %>%
  summarise(mean = mean(log_SBR), sd = sd(log_SBR), n=n())
SBR.plot %>% filter(SBR !=0, definition=="ge22wks", context2=="subnat.admin VR") %>%
  mutate(log_SBR = log(SBR)) %>%
  summarise(mean = mean(log_SBR), sd = sd(log_SBR), n=n())

#------------------------------------------------------------------------------------#

# note: y is log(SBR)
# fiddle with the following
mean_28 <- 1.5
A_bias <- 0.03
B_bias <- -0.05
A_sd <- 0.5
B_sd <- 0.5
delta_22 <- 0.25
delta_sd <- 0.05
# hist(delta_22+rnorm(1500, 0, delta_sd))

n.sim <- 1000
alpha_est <- rep(NA, n.sim)
cor_est <- rep(NA, n.sim)

for(i in 1:n.sim){
sim.dat <- data.frame(y_28 = rep(NA, 1500), y_22 = rep(NA, 1500),
                      source = c(rep("A",300), rep("B", 1200)))
sim.dat$y_28[1:300] <- rnorm(300, mean_28+A_bias, A_sd)
sim.dat$y_28[301:1500] <- rnorm(1200, mean_28+B_bias, B_sd)
sim.dat$y_22 <- sim.dat$y_28 + delta_22 + rnorm(1500, 0, delta_sd)
sim.dat$log_rat <- sim.dat$y_22 - sim.dat$y_28

mod <- lm(log_rat~1, data=sim.dat)
alpha_est[i] <- mod$coef
cor_est[i] <- cor(sim.dat$y_28, mod$resid)
}
hist(alpha_est)
hist(cor_est)

par(mar=c(4,4,2,2))
plot(sim.dat$y_28, pch=as.numeric(sim.dat$source))
points(sim.dat$y_22, col="blue", pch=as.numeric(sim.dat$source))
plot(sim.dat$log_rat, col=sim.dat$source)

mod <- lm(log_rat~1, data=sim.dat)
summary(mod)
sim.resid <- mod$resid
cor.test(sim.dat$y_28, sim.resid)$p.value
plot(sim.dat$y_28, sim.resid); abline(h=0, lty=2)
plot(sim.dat$y_22, sim.resid); abline(h=0, lty=2)
plot(exp(sim.dat$y_28), sim.resid); abline(h=0, lty=2)
