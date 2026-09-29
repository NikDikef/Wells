library(readxl)
library(tidyverse)
library(pROC)
library(epiR)
library(gt)

df_DVT <- read_excel("df.xlsx")

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

epi.tests(table(df_DVT$DVT, df_DVT$Wells_group), conf.level = 0.95, digits = 4)

DVT <- riskyr(scen_lbl = " ", 
                   cond_lbl = "DVT",
                   dec_lbl = "Wells ≥2",
                   popu_lbl = "Sample", 
                   N = 867,
                   prev = 0.4614, sens = 0.9175, spec = 0.9186)

plot(DVT, by = "dc")
plot(DVT, type = "tab")
plot(DVT, type = "prism", by = "acdc", area = "hr") 
plot(DVT, type = "curve", uc = 0.05, show_points = F)

summary(DVT)











library(tidyverse)
library(tidybayes)
library(ggdist)

epi.tests(table(df_DVT$DVT, df_DVT$Wells_group), conf.level = 0.95, digits = 4)

conf_mat <- matrix(
  c(367, 33, 38, 429), nrow = 2,
  dimnames = list(
    c("T+", "T-"),
    c("D+", "D-")
  )
)

conf_mat["T+", "D+"] / sum(conf_mat[, "D+"]) # observed sensitivity

conf_mat["T-", "D-"] / sum(conf_mat[, "D-"]) # observed specificity


(conf_mat["T+", "D+"] * sum(conf_mat[, "D-"])) /
  (sum(conf_mat[, "D+"]) * conf_mat["T+", "D-"]) # observed LR+


(conf_mat["T-", "D+"] * sum(conf_mat[, "D-"])) /
  (sum(conf_mat[, "D+"]) * conf_mat["T-", "D-"]) # observed LR-


conf_mat["T+", "D+"] / sum(conf_mat["T+", ]) # apparent PPV

conf_mat["T-", "D-"] / sum(conf_mat["T-", ]) # apparent NPV


# Prior and posterior distributions for sensitivity and specificity
tribble(
  ~par, ~dist, ~alpha, ~beta,
  "Sensitivity", "Prior", 12, 6,
  "Sensitivity", "Posterior", 12 + conf_mat["T+", "D+"], 6 + conf_mat["T-", "D+"],
  "Specificity", "Prior", 18, 10,
  "Specificity", "Posterior", 18 + conf_mat["T-", "D-"], 10 + conf_mat["T+", "D-"],
) %>% 
  ggplot(aes(xdist = distributional::dist_beta(alpha, beta))) +
  stat_slab(aes(fill = dist), alpha = 0.5) +
  facet_wrap(~par, nrow = 1) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
n_samles <- 10000

df_samples <- tibble(
  Sensitivity = rbeta(n_samles, 12 + conf_mat["T+", "D+"], 6 + conf_mat["T-", "D+"]),
  Specificity = rbeta(n_samles, 18 + conf_mat["T-", "D-"], 10 + conf_mat["T+", "D-"])
) |> 
  mutate(
    `LR+` = Sensitivity / (1 - Specificity),
    `LR-` = (1 - Sensitivity) / Specificity
  )

prev <- seq(0.01, 0.99, length.out = 100)

tibble(
  prev, 
  res = list(df_samples)
) |> 
  unnest(res) |> 
  mutate(
    PPV = plogis(log((`LR+` * prev / (1 - prev)))),
    NPV = plogis(log((`LR-` * prev / (1 - prev))))
  ) |> 
  ggplot(aes(prev)) +
  stat_lineribbon(aes(y = PPV, color = "ППВ", fill = "ППВ"), .width = c(0.89)) +
  stat_lineribbon(aes(y = NPV, color = "ОПВ", fill = "ОПВ"), .width = c(0.89)) +
  theme_minimal() +
  scale_color_manual(name = NULL, 
                     breaks=c('ППВ', 'ОПВ'),
                     values=c('ППВ'='#E67E22', 'ОПВ'='#3498DB')) +
  scale_fill_manual(name = NULL, 
                     breaks=c('ППВ', 'ОПВ'),
                     values=alpha(c('ППВ'='#E67E22', 'ОПВ'='#3498DB'),0.3),
                     guide = "none") +
  theme(text = element_text(size = 16), legend.position = c(.1, .92)) +
  labs(x = "Распространенность", y = "Вероятность") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent)



df <- tibble(
  prev, 
  res = list(df_samples)
) |> 
  unnest(res) |> 
  mutate(
    PPV = plogis(log((`LR+` * prev / (1 - prev)))),
    NPV = plogis(log((`LR-` * prev / (1 - prev)))))

df1 <- df*100 
  
df1 |> 
  mutate(prev = round(prev,0)) |> 
  reframe(PPV1 = round(median(PPV), digits = 2),
          PPV_CI = paste0(round(quantile(PPV, 0.055), digits = 2), " – ", 
                       round(quantile(PPV, 0.945), digits = 2)), 
          NPV1 = round(median(NPV), digits = 2),
          NPV_CI = paste0(round(quantile(NPV, 0.055), digits = 2), " – ", 
                          round(quantile(NPV, 0.945), digits = 2)), 
          .by = prev) |> 
  filter(!(prev %% 5) | prev == 1) |> 
  gt() |> 
  cols_merge(columns = c(PPV1, PPV_CI),
             pattern = "<<{1} ({2})>>") |> 
  cols_merge(columns = c(NPV1, NPV_CI),
             pattern = "<<{1} ({2})>>") |> 
  cols_label(PPV1 = "ППВ<br>(89% ДИ),%",
             NPV1 = "ОПВ<br>(89% ДИ),%",
             prev = "Предтестовая<br>вероятность, %",
             .fn = md) |> 
  cols_align(align = "center") |> 
  tab_footnote(footnote = "ППВ - Положительная посттестовая вероятность",
               locations = cells_column_labels(columns = PPV1)) |> 
  tab_footnote(footnote = "ОПВ - Отрицательная посттестовая вероятность",
               locations = cells_column_labels(columns = NPV1)) |> 
  tab_footnote(footnote = "ДИ - Достоверный интервал",
               locations = cells_column_labels(columns = c(PPV1, NPV1))) |> 
  tab_source_note(source_note = "Предтестовая вероятность соответствует распространенности ТГВ в популяции. 
                  Положительным результатом считалось значение по шкале Уэллса 2 балла и выше.")



