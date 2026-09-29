library(riskyr)

DVT <- riskyr(scen_lbl = " ", 
                   cond_lbl = "DVT",
                   dec_lbl = "Wells ≥2",
                   popu_lbl = "Sample", 
                   N = 867,
                   prev = 0.5386, sens = 0.9286, spec = 0.9062)

plot(DVT, by = "dc")
plot(DVT, type = "tab")
plot(DVT, type = "prism", by = "acdc", area = "hr") 
plot(DVT, type = "curve", uc = 0.05, show_points = F)

summary(DVT)










library(readxl)
library(tidyverse)
library(tidybayes)
library(ggdist)
library(epiR)
library(gt)

df_DVT <- read_excel("df.xlsx")

df_DVT$Wells_group1 <- factor(ifelse(df_DVT$Wells_group == 1, "Yes", "No"), levels = c("Yes", "No"))
df_DVT$DVT1 <- factor(ifelse(df_DVT$DVT == 1, "Yes", "No"), levels = c("Yes", "No"))

test <- epi.tests(table(df_DVT$Wells_group1, df_DVT$DVT1), conf.level = 0.95, digits = 4)

data.frame("Test" = c("+", "-", "Total"),
           "Var1" = c(429, 33, 462),
           "Var2" = c(38,367,405),
           "Total" = c(467,400,867)) |> 
  gt() |> 
  cols_align(align = "center") |> 
  tab_spanner(label = md("**Outcome**"), columns = starts_with(c("Var1", "Var2"))) |> 
  cols_label(Test = md("**Test**"),
             Var1 = md("**+**"),
             Var2 = md("**-**"),
             Total = md("**Total**")) |> 
  tab_style(style = list(
      cell_text(weight = "bold")),
    locations = cells_body(columns = Test)) |> 
  cols_width(everything() ~ px(100))


test$tab

summary(test) |> 
  as_tibble() |> 
  slice(2,3,4,9:12) |> 
  gt() |> 
  fmt_number(rows = 1:5, n_sigfig = 4, scale_by = 100) |> 
  fmt_number(rows = 6, n_sigfig = 4, scale_by = 1) |> 
  fmt_number(rows = 7, n_sigfig = 2, scale_by = 1) |> 
  text_case_match(
    "tp" ~ "Prev, %", 
    "se"~ "Se, %", 
    "sp"~"Sp, %", 
    "pv.pos"~"PPV, %", 
    "pv.neg"~"NPV, %", 
    "lr.pos"~"LR+", 
    "lr.neg"~"LR-") |> 
  cols_merge(columns = c(lower, upper),
             pattern = "{1} &mdash; {2}") |> 
  cols_label(statistic = "Parameters",
             est = "Estimate",
             lower = "95% CI",
             .fn = md) |> 
  cols_align(align = "center")

conf_mat <- matrix(
  c(429, 33, 38, 367), nrow = 2,
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
  "Specificity", "Prior", 47, 5,
  "Specificity", "Posterior", 47 + conf_mat["T-", "D-"], 5 + conf_mat["T+", "D-"],
) %>% 
  ggplot(aes(xdist = distributional::dist_beta(alpha, beta))) +
  stat_slab(aes(fill = dist), alpha = 0.5) +
  facet_wrap(~par, nrow = 1) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  labs(x = "Propability", y = "Density") +
  scale_fill_manual(values = c("#A93226", "#2471A3"))

mean(rbeta(100, 47,5))

n_samles <- 10000

df_samples <- tibble(
  Sensitivity = rbeta(n_samles, 12 + conf_mat["T+", "D+"], 6 + conf_mat["T-", "D+"]),
  Specificity = rbeta(n_samles, 47 + conf_mat["T-", "D-"], 5 + conf_mat["T+", "D-"])
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


n_samles <- 10000

df_samples <- tibble(
  Sensitivity = rbeta(n_samles, 12 + conf_mat["T+", "D+"], 6 + conf_mat["T-", "D+"]),
  Specificity = rbeta(n_samles, 45 + conf_mat["T-", "D-"], 5 + conf_mat["T+", "D-"])
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
  stat_lineribbon(aes(y = PPV, color = "PPV", fill = "PPV"), .width = c(0.89)) +
  stat_lineribbon(aes(y = NPV, color = "NPV", fill = "NPV"), .width = c(0.89)) +
  theme_minimal() +
  scale_color_manual(name = NULL, 
                     breaks=c('PPV', 'NPV'),
                     values=c('PPV'='#E67E22', 'NPV'='#3498DB')) +
  scale_fill_manual(name = NULL, 
                    breaks=c('PPV', 'NPV'),
                    values=alpha(c('PPV'='#E67E22', 'NPV'='#3498DB'),0.3),
                    guide = "none") +
  theme(text = element_text(size = 16), legend.position = c(.1, .92)) +
  labs(x = "Prevalence", y = "Probability") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent)

tribble(
  ~par, ~dist, ~alpha, ~beta,
  "Sensitivity", "Prior", 12, 6,
  "Sensitivity", "Posterior", 12 + conf_mat["T+", "D+"], 6 + conf_mat["T-", "D+"],
  "Specificity", "Prior", 45, 5,
  "Specificity", "Posterior", 45 + conf_mat["T-", "D-"], 5 + conf_mat["T+", "D-"],
) %>% 
  ggplot(aes(xdist = distributional::dist_beta(alpha, beta))) +
  stat_slab(aes(fill = dist), alpha = 0.5) +
  facet_wrap(~par, nrow = 1) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  labs(x = "Propability", y = "Density") +
  scale_fill_manual(values = c("#A93226", "#2471A3"))

