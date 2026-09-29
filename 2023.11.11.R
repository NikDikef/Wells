library(readxl)
library(tidyverse)
library(pROC)
library(epiR)
library(gt)

df <- read_excel("df.xlsx")

coords(roc(DVT ~ Wells_point, df, subset = (Year == "2012"), ci = T), "best")
coords(roc(DVT ~ Wells_point, df, subset = (Year == "2013"), ci = T), "best")
coords(roc(DVT ~ Wells_point, df, subset = (Year == "2014"), ci = T), "best")
coords(roc(DVT ~ Wells_point, df, subset = (Year == "2015"), ci = T), "best")
coords(roc(DVT ~ Wells_point, df, subset = (Year == "2016"), ci = T), "best")
coords(roc(DVT ~ Wells_point, df, subset = (Year == "2017"), ci = T), "best")
coords(roc(DVT ~ Wells_point, df, subset = (Year == "2018"), ci = T), "best")

epi1 <- epi.tests(table(df[df$Year == "2012",]$DVT, df[df$Year == "2012",]$Wells_group), conf.level = 0.95)$detail[11,]$est
epi2 <- epi.tests(table(df[df$Year == "2013",]$DVT, df[df$Year == "2013",]$Wells_group), conf.level = 0.95)$detail[11,]$est
epi3 <- epi.tests(table(df[df$Year == "2014",]$DVT, df[df$Year == "2014",]$Wells_group), conf.level = 0.95)$detail[11,]$est
epi4 <- epi.tests(table(df[df$Year == "2015",]$DVT, df[df$Year == "2015",]$Wells_group), conf.level = 0.95)$detail[11,]$est
epi5 <- epi.tests(table(df[df$Year == "2016",]$DVT, df[df$Year == "2016",]$Wells_group), conf.level = 0.95)$detail[11,]$est
epi6 <- epi.tests(table(df[df$Year == "2017",]$DVT, df[df$Year == "2017",]$Wells_group), conf.level = 0.95)$detail[11,]$est
epi7 <- epi.tests(table(df[df$Year == "2018",]$DVT, df[df$Year == "2018",]$Wells_group), conf.level = 0.95)$detail[11,]$est

epi11 <- epi.tests(table(df[df$Year == "2012",]$DVT, df[df$Year == "2012",]$Wells_group), conf.level = 0.95)$detail[12,]$est
epi21 <- epi.tests(table(df[df$Year == "2013",]$DVT, df[df$Year == "2013",]$Wells_group), conf.level = 0.95)$detail[12,]$est
epi31 <- epi.tests(table(df[df$Year == "2014",]$DVT, df[df$Year == "2014",]$Wells_group), conf.level = 0.95)$detail[12,]$est
epi41 <- epi.tests(table(df[df$Year == "2015",]$DVT, df[df$Year == "2015",]$Wells_group), conf.level = 0.95)$detail[12,]$est
epi51 <- epi.tests(table(df[df$Year == "2016",]$DVT, df[df$Year == "2016",]$Wells_group), conf.level = 0.95)$detail[12,]$est
epi61 <- epi.tests(table(df[df$Year == "2017",]$DVT, df[df$Year == "2017",]$Wells_group), conf.level = 0.95)$detail[12,]$est
epi71 <- epi.tests(table(df[df$Year == "2018",]$DVT, df[df$Year == "2018",]$Wells_group), conf.level = 0.95)$detail[12,]$est

LR_pos <- rbind(epi1,epi2, epi3, epi4, epi5, epi6, epi7)
LR_neg <- rbind(epi11,epi21, epi31, epi41, epi51, epi61, epi71)

epi.tests(table(df$DVT, df$Wells_group), conf.level = 0.95, digits = 4)

rethinking::HPDI(LR_pos, prob = 0.95)
rethinking::HPDI(LR_neg)

df1 <- data.frame(prev = 1:100/100)

df1 <- df1 |> 
  mutate(PPV = ((prev/(1-prev))*11.2756)/(1 + (prev/(1-prev))*11.2756),
         PPV_CI_low = ((prev/(1-prev))*5.690722)/(1 + (prev/(1-prev))*5.690722),
         PPV_CI_high = ((prev/(1-prev))*21.470588)/(1 + (prev/(1-prev))*21.470588),
         NPV = ((prev/(1-prev))*0.0898)/(1 + (prev/(1-prev))*0.0898),
         NPV_CI_low = ((prev/(1-prev))*0.04371585)/(1 + (prev/(1-prev))*0.04371585),
         NPV_CI_high = ((prev/(1-prev))*0.16491228)/(1 + (prev/(1-prev))*0.16491228))

df1 |> 
  ggplot() +
  geom_line(aes(x = prev, y = PPV, color = "ППВ"), size = 1, na.rm = T) +
  theme_minimal() +
  labs(x = "Prevalence", y = "Propability") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) +
  geom_ribbon(aes(x = prev, ymin=PPV_CI_low, ymax=PPV_CI_high, fill = "ППВ"), alpha=0.2) +
  geom_line(aes (x = prev, y = NPV, color = "ОПВ"), size = 1, na.rm = T) +
  geom_ribbon(aes(x = prev, ymin=NPV_CI_low, ymax=NPV_CI_high, fill = "ОПВ"), alpha=0.2) +
  scale_color_manual(name = NULL, 
                     breaks=c('ППВ', 'ОПВ'),
                     values=c('ППВ'='#E67E22', 'ОПВ'='#3498DB')) +
  scale_fill_manual(breaks=c('ППВ', 'ОПВ'),
                    values=c('ППВ'='#E67E22', 'ОПВ'='#3498DB'), 
                    guide = "none") +
  theme(text = element_text(size = 16), legend.position = c(.1, .92))
  
    


gt(round(df1[c(1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95),]*100, digits = 2 )) |> 
  cols_merge(columns = c(PPV, PPV_CI_low, PPV_CI_high),
             pattern = "<<{1} ({2} &mdash; {3})>>") |> 
  cols_merge(columns = c(NPV, NPV_CI_low, NPV_CI_high),
             pattern = "<<{1} ({2} &mdash; {3})>>") |> 
  cols_label(PPV = "PPV<br>(95% CI),%",
             NPV = "NPV<br>(95% CI),%",
             prev = "Pre-test<br>prevalence, %",
             .fn = md) |> 
  cols_align(align = "center") |> 
  tab_footnote(footnote = "CI - Confidence interval",
               locations = cells_column_labels(columns = c(PPV,NPV)))
  
library(riskyr)

epi.tests(table(df$DVT, df$Wells_group), conf.level = 0.95, digits = 4)

hustosis <- riskyr(scen_lbl = "Example", 
                   cond_lbl = "Hustosis",
                   dec_lbl = "Screening",
                   popu_lbl = "Sample", 
                   N = 867,
                   prev = 0.4614, sens = 0.9175, spec = 0.9186)

plot(hustosis, by = "dc")
plot(hustosis, type = "tab")
plot(hustosis, type = "prism", by = "acdc", area = "hr") 
plot(hustosis, type = "curve", uc = 0.05, show_points = F)

summary(hustosis)

data(epi.SClip)
obs <- epi.SClip$cases; pop <- epi.SClip$population

est <- epi.empbayes(obs, pop)
crude.p <- ((obs) / (pop)) * 100000
crude.r <- rank(crude.p)
ebay.p <- ((obs + est[4]) / (pop + est[3])) * 100000

dat.df01 <- data.frame(rank = c(crude.r, crude.r), 
                       Method = c(rep("Crude", times = length(crude.r)), 
                                  rep("Empirical Bayes", times = length(crude.r))),
                       est = c(crude.p, ebay.p)) 

ggplot(dat = dat.df01, aes(x = rank, y = est, colour = Method)) + 
  geom_point() +
  scale_x_continuous(name = "District rank", 
                     breaks = seq(from = 0, to = 60, by = 10), 
                     labels = seq(from = 0, to = 60, by = 10), 
                     limits = c(0,60)) +
  scale_y_continuous(limits = c(0,30), name = "Lip cancer incidence rates 
      (cases per 100,000 person years)") +
  geom_smooth()


