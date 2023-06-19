############################################################################
# beta regression with zeros for duration of time spent engaged in behaviors

# required libraries
library(readxl)
library(ggplot2)
library(tidyverse)
library(gamlss)
library(pubh)


#########################
# read in and format data
dat <- read_excel("chicken_durations.xlsx")

dat$SD = as.factor(dat$SD)
levels(dat$SD) = c("High Stocking Density", "Low Stocking Density")
dat$EE = as.factor(dat$EE)
levels(dat$EE) = c("High Complexity", "Low Complexity")
dat$EE <- factor(dat$EE, levels=c('Low Complexity', 'High Complexity'))
dat$pen = as.factor(dat$pen)
dat$tod = as.factor(dat$Time)
dat$location = as.factor(dat$Location)
dat$space = as.factor(dat$space)
dat$week = as.factor(dat$week)
levels(dat$week) = c("2", "4", "7")
dat$active = dat$totalsum - (dat$resting + dat$sitting)
dat$inactive = dat$resting + dat$sitting

dat[, 8:21] = dat[, 8:21] / dat$totalsum
dat[, 25:26] = dat[, 25:26] / dat$totalsum



####################
# active

dat1 = dat %>%
  select(active, EE, SD, week, tod, pen, location)

dat1[which(dat1$active < 0), "active"] = 0

m1 = gamlss(active ~ EE+week+SD, nu.fo = ~EE+week,  
            family = BEINF, data = dat1)

summary(m1)
Rsq(m1)
expCoef.H <- exp(coef((m1)))


p1 = emmip(m1, SD+EE ~ week, CIs = TRUE, type = "response", dodge = 0.5,
      linearg = list(linetype = "blank"),
      dotarg = list(size = 2, alpha = 1),
      CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "") +
  ylim(0, .6) + ylab("Proportion of Time\n Spent Active")+
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

# pairwise comparisons
x = emmeans(m1, ~ week)
comps = pwpm(x)


####################
# foraging
dat2 = dat %>%
  select(foraging, EE, SD, week)

m2 = gamlss(foraging ~ EE+week+SD, nu.fo = ~EE*week + SD*week,  
            family = BEINF, data = dat2)

p2 = emmip(m2,  SD+EE ~ week, CIs = TRUE, type = "response", dodge = 0.5,
      linearg = list(linetype = "blank"),
      dotarg = list(size = 2, alpha = 1),
      CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  ylim(0, .3) +
  theme_classic() +
  xlab("") + ylab("Foraging") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

# pairwise comparisons
x = emmeans(m2, ~ EE+week+SD)
comps = pwpm(x)

####################
# preening
dat3 = dat %>%
  select(preening, EE, SD, week)

m3 = gamlss(preening ~ EE+week+SD, nu.fo = ~week,  
            family = BEINF, data = dat3)

p3 = emmip(m3,  SD+EE ~ week, CIs = TRUE, type = "response", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  ylim(0, .3) +
  theme_classic() +
  xlab("") + ylab("Preening") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

# pairwise comparisons
x = emmeans(m3, ~ EE+week+SD)
comps = pwpm(x)

####################
# locomotion
dat4 = dat %>%
  select(locomotion, EE, SD, week, tod, pen, location)

m4 = gamlss(locomotion ~ week+EE+SD, nu.fo = ~week,  
            family = BEINF, data = dat4)

p4 = emmip(m4,  SD+EE ~ week, CIs = TRUE, type = "response", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  ylim(0, .3) +
  theme_classic() +
  xlab("") + ylab("Locomotion") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) 

# pairwise comparisons
x = emmeans(m4, ~ week)
comps = pwpm(x)

####################
# eating
dat5 = dat %>%
  select(eating, EE, SD, week)

m5 = gamlss(eating ~ week+SD+EE, nu.fo = ~week + SD*EE,  
            family = BEINF, data = dat5)

p5 = emmip(m5,  SD+EE ~ week, CIs = TRUE, type = "response", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  ylim(0, .6) +
  theme_classic() +
  xlab("") + ylab("Eating") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) 

# pairwise comparisons
x = emmeans(m5, ~ week)
comps = pwpm(x)

####################
# drinking
dat6 = dat %>%
  select(drinking, EE, SD, week, tod, pen, location)

m6 = gamlss(drinking ~ week+SD+EE, nu.fo = ~week,  
            family = BEINF, data = dat6)


p6 = emmip(m6,  SD+EE ~ week, CIs = TRUE, type = "response", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  ylim(0, .6) +
  theme_classic() +
  xlab("") + ylab("Drinking") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) 

# pairwise comparisons
x = emmeans(m6, ~ week)
comps = pwpm(x)


####################
# dustbathing
dat7 = dat %>%
  select(dustbathing, EE, SD, week, tod, pen, location)

m7 = gamlss(dustbathing ~ 1, nu.fo = ~1,  
            family = BEINF, data = dat7)

####################
# perching
dat8 = dat %>%
  select(perching, EE, SD, week, tod, pen, location)

m8 = gamlss(perching ~ 1, nu.fo = ~EE*week,  
            family = BEINF, data = dat8)


################
# combine plots

g1 = ggarrange(p1, 
               ggarrange(p5, p6, ncol = 2), 
               ggarrange(p2, p3, p4, ncol = 3),
               nrow = 3,
               common.legend = T)

g1 = annotate_figure(g1,
                bottom = text_grob("Age (Weeks)", color = "black",
                                   vjust = -.5, face = "bold", size = 20),
                left = text_grob("Proportion of Activity", color = "black",
                                 face = "bold", size = 20, rot = 90, hjust = 0.85))


ggsave("chickens_durations.pdf", g1, dpi = 600, width = 10, height = 8, units = "in", device = cairo_pdf)
