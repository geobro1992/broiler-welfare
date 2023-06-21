################
# hurdle models

library(readxl)
library(ggplot2)
library(pubh)
library(tidyverse)
library(pscl)
library(countreg)
library(ggpubr)

#########################
# read in and format data
dat <- read_excel("chicken_dat.xlsx")

dat$SD = as.factor(dat$SD)
levels(dat$SD) = c("High Stocking Density", "Low Stocking Density")
dat$EE = as.factor(dat$EE)
levels(dat$EE) = c("High Complexity", "Low Complexity")
dat$EE <- factor(dat$EE, levels=c('Low Complexity', 'High Complexity'))
dat$pen = as.factor(dat$pen)
dat$tod = as.factor(dat$`time of day`)
dat$location = as.factor(dat$location)
dat$space = as.factor(dat$space)
dat$week = as.factor(dat$week)
levels(dat$week) = c("2", "4", "7")
dat$active = dat$total - (dat$resting + dat$sitting)
dat$inactive = dat$resting + dat$sitting

##################################
# hurdle models for each behaviour
##################################

###################
# active combined
m1 = hurdle(active ~ EE+SD*week | EE+SD*week, data = dat, dist = "negbin", zero.dist = "binomial")

rootogram(m1, max = max(dat$active),main = "All active behaviours") # model fit

p1 = emmip(m1,  SD+EE ~ week, CIs = TRUE, mode = "count", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  theme_Publication() +
  xlab("") + ylab("Active") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  scale_y_continuous(breaks = seq(0, 8,by = 2), labels = c("0", "2", "4", "6", "8"), limits = c(0,8))

p10 = emmip(m1,  SD+EE ~ week, CIs = TRUE, mode = "prob0", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  theme_classic() +
  xlab("") + ylab("Active") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  scale_y_reverse(breaks = seq(1, 0, by = -0.2), labels = c("0", "0.2", "0.4", "0.6", "0.8", "1.0"), limits = c(1,0))

# pairwise comparisons
x = emmeans(m1, ~ SD*week, mode = "count")
comps = pwpm(x)
x = emmeans(m1, ~ EE, mode = "prob0")
comps = pwpm(x)

#######################################
# preening

m2 = hurdle(preening ~ EE*week+SD | EE*week+SD, data = dat, dist = "negbin", zero.dist = "binomial")

p2 = emmip(m2,  SD+EE ~ week, CIs = TRUE, mode = "count", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  theme_Publication() +
  xlab("") + ylab("Preening") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  scale_y_continuous(breaks = seq(0, 6, by = 2), labels = c("0", "2", "4", "6"), limits = c(0,6.5))


p20 = emmip(m2,  SD+EE ~ week, CIs = TRUE, mode = "prob0", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  theme_classic() +
  xlab("") + ylab("Preening") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  scale_y_reverse(breaks = seq(1, 0, by = -0.2), labels = c("0", "0.2", "0.4", "0.6", "0.8", "1.0"), limits = c(1,0))


# pairwise comparisons
x = emmeans(m2, ~ week*EE, mode = "count")
comps = pwpm(x)
x = emmeans(m2, ~ week, mode = "prob0")
comps = pwpm(x)


##############
# dustbathing

m3 = hurdle(dustbathing ~ 1 | 1, data = dat, dist = "negbin", zero.dist = "binomial")


###########
# perching 

m4 = hurdle(perching ~ 1 | EE, data = dat, dist = "negbin", zero.dist = "binomial")

rootogram(m4, max = max(dat$perching), main = "perching") # model fit

# pairwise comparisons
x = emmeans(m4, ~ EE, mode = "prob0")
comps = pwpm(x)


##########
# foraging

m5 = hurdle(foraging ~ EE+week+SD|EE*week+SD*week, data = dat, dist = "negbin", zero.dist = "binomial")

rootogram(m5, max = max(dat$foraging), main = "foraging") # model fit

p3 = emmip(m5,  SD+EE ~ week, CIs = TRUE, mode = "count", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  theme_Publication() +
  xlab("") + ylab("Foraging") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  scale_y_continuous(breaks = seq(0, 6, by = 2), labels = c("0", "2", "4", "6"), limits = c(0,6.5))

p30 = emmip(m5,  SD+EE ~ week, CIs = TRUE, mode = "prob0", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  theme_classic() +
  xlab("") + ylab("Foraging") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  scale_y_reverse(breaks = seq(1, 0, by = -0.2), labels = c("0", "0.2", "0.4", "0.6", "0.8", "1.0"), limits = c(1,0))

# pairwise comparisons
x = emmeans(m3, ~ SD+week+EE, mode = "prob0")
comps = pwpm(x)


#########
# eating 

m6 = hurdle(eating ~ week + EE+SD |week + EE*SD, data = dat, dist = "negbin", zero.dist = "binomial")

rootogram(m6, max = max(dat$eating), main = "eating") # model fit

p4 = emmip(m6,  SD+EE ~ week, CIs = TRUE, mode = "count", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  theme_Publication() +
  xlab("") + ylab("Eating") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  scale_y_continuous(breaks = seq(0, 4, by = 2), labels = c("0", "2", "4"), limits = c(0,4.5))

p40 = emmip(m6,  SD+EE ~ week, CIs = TRUE, mode = "prob0", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  theme_classic() +
  xlab("") + ylab("Eating") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  scale_y_reverse(breaks = seq(1, 0, by = -0.2), labels = c("0", "0.2", "0.4", "0.6", "0.8", "1.0"), limits = c(1,0))

# pairwise comparisons
x = emmeans(m6, ~ SD, mode = "count")
comps = pwpm(x)
x = emmeans(m6, ~ EE+SD, mode = "prob0")
comps = pwpm(x)


#####################################
# drinking 

m7 = hurdle(drinking ~ week+SD+EE|week+SD+EE, data = dat, dist = "negbin", zero.dist = "binomial")

rootogram(m7, max = max(dat$drinking), main = "drinking") # model fit

p5 = emmip(m7,  SD+EE ~ week, CIs = TRUE, mode = "count", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  theme_Publication() +
  xlab("") + ylab("Drinking") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  scale_y_continuous(breaks = seq(0, 4, by = 2), labels = c("0", "2", "4"), limits = c(0,4.5))

p50 = emmip(m7,  SD+EE ~ week, CIs = TRUE, mode = "prob0", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  theme_classic() +
  xlab("") + ylab("Drinking") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  scale_y_reverse(breaks = seq(1, 0, by = -0.2), labels = c("0", "0.2", "0.4", "0.6", "0.8", "1.0"), limits = c(1,0))

# pairwise comparisons
x = emmeans(m7, ~ week, mode = "count")
comps = pwpm(x)
x = emmeans(m7, ~ week, mode = "prob0")
comps = pwpm(x)


###################
# locomotion

m8 = hurdle(locomotion ~ week * SD * EE | week*SD*EE, data = dat, dist = "negbin", zero.dist = "binomial")

rootogram(m8, max = max(dat$locomotion),main = "Locomotion") # model fit


p7 = emmip(m8,  SD+EE ~ week, CIs = TRUE, mode = "count", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  theme_Publication() +
  xlab("") + ylab("Locomotion") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  scale_y_continuous(breaks = seq(0, 6, by = 2), labels = c("0", "2", "4", "6"), limits = c(0,6.5))


p70 = emmip(m8,  SD+EE ~ week, CIs = TRUE, mode = "prob0", dodge = 0.5,
           linearg = list(linetype = "blank"),
           dotarg = list(size = 2, alpha = 1),
           CIarg = list(lwd = 1.5, alpha = 0.8)) %>%
  gf_labs(y = "active", x = "Enrichment Environment") +
  theme_classic() +
  xlab("") + ylab("Locomotion") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  scale_y_reverse(breaks = seq(1, 0, by = -0.25), labels = c("0", "0.25", "0.5", "0.75", "1.0"), limits = c(1,0))


# pairwise comparisons
x = emmeans(m8, ~ SD+week+EE, mode = "count")
comps = pwpm(x)
x = emmeans(m8, ~ week, mode = "prob0")
comps = pwpm(x)


###############
# combine plots

# counts of behaviours
g1 = ggarrange(p1,
               ggarrange(p4, p5, ncol = 2), 
               ggarrange(p3, p2, p7, ncol = 3),
               nrow = 3,
               common.legend = T)

g1 = annotate_figure(g1,
                     bottom = text_grob("Age (Weeks)", color = "black",
                                        vjust = -.5, face = "bold", size = 20),
                     left = text_grob("Predicted Count of Behavior", color = "black",
                                      face = "bold", size = 20, rot = 90))


ggsave("chickens_counts.pdf", g1, dpi = 600, width = 10, height = 8, units = "in", device = cairo_pdf)


# probability of not observing behaviour
g10 = ggarrange(p10,
               ggarrange(p40, p50, ncol = 2), 
               ggarrange(p30, p20, p70, ncol = 3),
               nrow = 3,
               common.legend = T)

g10 = annotate_figure(g10,
                bottom = text_grob("Age (Weeks)", color = "black",
                                   vjust = -.5, face = "bold", size = 20),
                left = text_grob("Probability of Observing Behavior", color = "black",
                                 face = "bold", size = 20, rot = 90))


ggsave("chickens_prob0.pdf", g10, dpi = 600, width = 10, height = 8, units = "in", device = cairo_pdf)









