# examine relationships between rekn masses, hsc egg availability, and survival

library(tidyverse)
library(readxl)

rekn_masses <- read_excel("./data/rekn-masses/rekn_masses.xlsx", sheet = "Main sheet")

rekn_masses <- rekn_masses %>% 
  filter(!Year == 2009)

# survival ~ hsc
s_hsc_plot <- ggplot(data = rekn_masses, aes(x=HSC, y=Survival)) +
  # geom_rect(data = filter(barker_res, parameter == "mean.s"),
  #           aes(xmin=-Inf, xmax=Inf, ymin=lcl, ymax=ucl), fill = "grey50", alpha=0.3) +
  # geom_hline(data = filter(barker_res, parameter == "mean.s"),
  #            aes(yintercept=mean)) +
  # geom_errorbar(data = s_hsc, aes(x=mean_hsc, ymin=lcl, ymax=ucl),
  #               width=0, size=0.5, colour="black", linetype=1) +
  # geom_line(data = s_res, aes(x=as.factor(year), y=mean, group = var),
  #           linetype="dashed", size=0.5) +
  geom_point(size=3) +
  stat_smooth(method = lm) +
  #scale_fill_manual(values = c("black", "white")) +
  #scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  #ylim(0, 1) +
  ylab("Annual survival probability") +
  xlab("Horseshoe crab egg abundance") +
  theme_bw() #+
#theme(axis.title.x = element_blank())

png(filename = "data/rekn-masses/figures/s-hsc-plot.png",
    width=6, height=4, units="in", res=600)

plot(s_hsc_plot)

dev.off()

# survival ~ arrival weight
s_aw_plot <- ggplot(data = rekn_masses, aes(x=`Mean Arrival Weight`, y=Survival)) +
  # geom_rect(data = filter(barker_res, parameter == "mean.s"),
  #           aes(xmin=-Inf, xmax=Inf, ymin=lcl, ymax=ucl), fill = "grey50", alpha=0.3) +
  # geom_hline(data = filter(barker_res, parameter == "mean.s"),
  #            aes(yintercept=mean)) +
  # geom_errorbar(data = s_hsc, aes(x=mean_hsc, ymin=lcl, ymax=ucl),
  #               width=0, size=0.5, colour="black", linetype=1) +
  # geom_line(data = s_res, aes(x=as.factor(year), y=mean, group = var),
  #           linetype="dashed", size=0.5) +
  geom_point(size=3) +
  stat_smooth(method = lm) +
  #scale_fill_manual(values = c("black", "white")) +
  #scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  #ylim(0, 1) +
  ylab("Annual survival probability") +
  xlab("Mean arrival weight") +
  theme_bw() #+
#theme(axis.title.x = element_blank())

png(filename = "data/rekn-masses/figures/s-aw-plot.png",
    width=6, height=4, units="in", res=600)

plot(s_aw_plot)

dev.off()

# survival ~ departure weight
s_dw_plot <- ggplot(data = rekn_masses, aes(x=`Mean Departure Weight`, y=Survival)) +
  # geom_rect(data = filter(barker_res, parameter == "mean.s"),
  #           aes(xmin=-Inf, xmax=Inf, ymin=lcl, ymax=ucl), fill = "grey50", alpha=0.3) +
  # geom_hline(data = filter(barker_res, parameter == "mean.s"),
  #            aes(yintercept=mean)) +
  # geom_errorbar(data = s_hsc, aes(x=mean_hsc, ymin=lcl, ymax=ucl),
  #               width=0, size=0.5, colour="black", linetype=1) +
  # geom_line(data = s_res, aes(x=as.factor(year), y=mean, group = var),
  #           linetype="dashed", size=0.5) +
  geom_point(size=3) +
  stat_smooth(method = lm) +
  #scale_fill_manual(values = c("black", "white")) +
  #scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  #ylim(0, 1) +
  ylab("Annual survival probability") +
  xlab("Mean departure weight") +
  theme_bw() #+
#theme(axis.title.x = element_blank())

png(filename = "data/rekn-masses/figures/s-dw-plot.png",
    width=6, height=4, units="in", res=600)

plot(s_dw_plot)

dev.off()

# survival ~ mass gain
s_mg_plot <- ggplot(data = rekn_masses, aes(x=`Mass Gain`, y=Survival)) +
  # geom_rect(data = filter(barker_res, parameter == "mean.s"),
  #           aes(xmin=-Inf, xmax=Inf, ymin=lcl, ymax=ucl), fill = "grey50", alpha=0.3) +
  # geom_hline(data = filter(barker_res, parameter == "mean.s"),
  #            aes(yintercept=mean)) +
  # geom_errorbar(data = s_hsc, aes(x=mean_hsc, ymin=lcl, ymax=ucl),
  #               width=0, size=0.5, colour="black", linetype=1) +
  # geom_line(data = s_res, aes(x=as.factor(year), y=mean, group = var),
  #           linetype="dashed", size=0.5) +
  geom_point(size=3) +
  stat_smooth(method = lm) +
  #scale_fill_manual(values = c("black", "white")) +
  #scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  #ylim(0, 1) +
  ylab("Annual survival probability") +
  xlab("Mass gain") +
  theme_bw() #+
#theme(axis.title.x = element_blank())

png(filename = "data/rekn-masses/figures/s-mg-plot.png",
    width=6, height=4, units="in", res=600)

plot(s_mg_plot)

dev.off()

# survival ~ p180
s_p180_plot <- ggplot(data = rekn_masses, aes(x=`P180`, y=Survival)) +
  # geom_rect(data = filter(barker_res, parameter == "mean.s"),
  #           aes(xmin=-Inf, xmax=Inf, ymin=lcl, ymax=ucl), fill = "grey50", alpha=0.3) +
  # geom_hline(data = filter(barker_res, parameter == "mean.s"),
  #            aes(yintercept=mean)) +
  # geom_errorbar(data = s_hsc, aes(x=mean_hsc, ymin=lcl, ymax=ucl),
  #               width=0, size=0.5, colour="black", linetype=1) +
  # geom_line(data = s_res, aes(x=as.factor(year), y=mean, group = var),
  #           linetype="dashed", size=0.5) +
  geom_point(size=3) +
  stat_smooth(method = lm) +
  #scale_fill_manual(values = c("black", "white")) +
  #scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  #ylim(0, 1) +
  ylab("Annual survival probability") +
  xlab("P180") +
  theme_bw() #+
#theme(axis.title.x = element_blank())

png(filename = "data/rekn-masses/figures/s-p180-plot.png",
    width=6, height=4, units="in", res=600)

plot(s_p180_plot)

dev.off()

# mass gain ~ hsc
mg_hsc_plot <- ggplot(data = rekn_masses, aes(x=HSC, y=`Mass Gain`)) +
  # geom_rect(data = filter(barker_res, parameter == "mean.s"),
  #           aes(xmin=-Inf, xmax=Inf, ymin=lcl, ymax=ucl), fill = "grey50", alpha=0.3) +
  # geom_hline(data = filter(barker_res, parameter == "mean.s"),
  #            aes(yintercept=mean)) +
  # geom_errorbar(data = s_hsc, aes(x=mean_hsc, ymin=lcl, ymax=ucl),
  #               width=0, size=0.5, colour="black", linetype=1) +
  # geom_line(data = s_res, aes(x=as.factor(year), y=mean, group = var),
  #           linetype="dashed", size=0.5) +
  geom_point(size=3) +
  stat_smooth(method = lm) +
  #scale_fill_manual(values = c("black", "white")) +
  #scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d %b")) +
  #ylim(0, 1) +
  ylab("Mass gain") +
  xlab("Horseshoe crab egg availability") +
  theme_bw() #+
#theme(axis.title.x = element_blank())

png(filename = "data/rekn-masses/figures/mg-hsc-plot.png",
    width=6, height=4, units="in", res=600)

plot(mg_hsc_plot)

dev.off()
