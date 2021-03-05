## CUGH_Poster_Visual_Generation.R ##
## Author: Kees Schipper ##
## Created: 2021-02-05 ##
## Last Updated: 2021-02-05 ##


if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(stringr)) install.packages("stringr"); library(stringr)
if (!require(readxl)) install.packages("readxl"); library(readxl)
if (!require(data.table)) install.packages("data.table"); library(data.table)
if (!require(kza)) install.packages("kza"); library(kza)
if (!require(lubridate)) install.packages("lubridate"); library(lubridate)
if (!require(MASS)) install.packages("MASS"); library(MASS)
if (!require(broom)) install.packages("broom"); library(broom)
if (!require(forecast)) install.packages("forecast"); library(forecast)
if (!require(conflicted)) install.packages("conflicted"); library(conflicted)
if (!require(segmented)) install.packages("segmented"); library(segmented)
if (!require(sjPlot)) install.packages("sjPlot"); library(sjPlot)
if (!require(gt)) install.packages("gt"); library(gt)
if (!require(mgcv)) install.packages("mgcv"); library(mgcv)
if (!require(emmeans)) install.packages("emmeans"); library(emmeans)
if (!require(moments)) install.packages("moments"); library(moments)
if (!require(arsenal)) install.packages("arsenal"); library(arsenal)
if (!require(FSA)) install.packages("FSA"); library(FSA)
if (!require(segmented)) install.packages("segmented"); library(segmented)
if (!require(sjPlot)) install.packages("sjPlot"); library(sjPlot)
if (!require(gt)) install.packages("gt"); library(gt)
if (!require(forestplot)) install.packages("forestplot"); library(forestplot)
if (!require(ggpubr)) install.packages("ggpubr"); library(ggpubr)
if (!require(lmtest)) install.packages("lmtest"); library(lmtest)
if (!require(ggthemes)) install.packages("ggthemes"); library(ggthemes)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


# read data ---------------------------------------------------------------

MD <- read_csv('cleaned data/Middlesex_Analysis_20200205_KS.csv') %>%
  mutate(weekfac = factor(weekfac, 
                          levels = c("Sunday", "Monday", "Tuesday", "Wednesday",
                                              "Thursday", "Friday", "Saturday")),
         weekfac = relevel(weekfac, ref = "Sunday"),
         weekend = ifelse(weekfac == "Sunday" | weekfac == "Sunday", 1, 0),
         weekend = factor(weekend, levels = c(0, 1), labels = c("weekday", "weekend")),
         testlo = lowess(MD$dtper100k ~ MD$T2, f = .20)$y,
         testlo = ifelse(testlo < 0, 0, testlo)) %>%
  mutate(dthpd = ifelse(row_number() < 65, "T1D",
                        ifelse(row_number() < 153 & row_number() >= 65, "T2D",
                               ifelse(row_number() < 230 & row_number() >= 153, "T3D",
                                      ifelse(row_number() >= 230, "T4D", NA)))))

periods <- split(MD, f = MD$period)

T2_df <- periods[[1]]
T3_df <- periods[[2]]
T4_df <- periods[[3]]
T5_df <- periods[[4]]


# create function to generate negative binomial model output --------------

COVID_seg_mod <- function(df, period, formula){
  
  per_cmod <- glm.nb(formula, df)
  tidymod <- tidy(per_cmod, exponentiate = T, conf.int = T)
  # tidymod$period <- period
  # tidymod <- tidymod %>%
  #   filter(grepl("weekfac", term))
  return(per_cmod)
  
}

case_formula <- dcper100k ~ caselo + weekfac + dtper100k
T1_mod <- COVID_seg_mod(df = T2_df, period = "T1", formula = case_formula)
T2_mod <- COVID_seg_mod(df = T3_df, period = "T2", formula = case_formula)
T3_mod <- COVID_seg_mod(df = T4_df, period = "T3", formula = case_formula)
T4_mod <- COVID_seg_mod(df = T5_df, period = "T4", formula = case_formula)


# create color scheme vector
color <- rev(c("#ca0020", "#0571b0", "grey43", "#ca0020")) # red blue
# color <- rev(c("#7b3294", "#008837", "#e66101", "#7b3294")) # purple green orange
# color <- rev(c("#ef8a62", "#67a9cf", "#f7f7f7", "#ef8a62")) # light red, blue, and snow
# color <- rev(c("#010079", "#CD9703", "#636260", "#010079")) # blue, gold, grey, blue colors


caseforest <- plot_models(T1_mod, T2_mod, T3_mod, T4_mod, rm.terms = c("caselo", "dtper100k"),
            show.p = T, m.labels = c("T1", "T2", "T3", "T4"), vline.color = "grey",
            show.values = F, spacing = 0.8, value.size = 3, show.legend = F) +
  ylim(0, 3.5) +
  # labs(color = "Critical Periods", title = "DoW Effects for Cases") +
  scale_color_discrete(name = "Critical Periods", labels = c("T1", "T2", "T3", "T4")) +
  scale_color_manual(values = color) +
  theme(axis.title.x = element_blank())
  # theme_grey() +
  # theme(plot.background = element_rect(fill = "snow"),
  #       panel.background = element_rect(fill = "snow",
  #                                       colour = "snow",
  #                                       size = 0.5, linetype = "solid"))


# repeat for deaths -------------------------------------------------------

deathsplit <- split(MD, f = MD$dthpd)

T1D_df <- periods[[1]]
T2D_df <- periods[[2]]
T3D_df <- periods[[3]]
T4D_df <- periods[[4]]

death_formula = ddper100k ~ deathlo + weekfac + dtper100k
T1D_mod <- COVID_seg_mod(df = T1D_df, period = "T1D", formula = death_formula)
tidy(T1D_mod, exponentiate = T, conf.int = T)
T2D_mod <- COVID_seg_mod(df = T2D_df, period = "T2D", formula = death_formula)
tidy(T2D_mod, exponentiate = T, conf.int = T)
T3D_mod <- COVID_seg_mod(df = T3D_df, period = "T3D", formula = death_formula)
tidy(T3D_mod, exponentiate = T, conf.int = T)
T4D_mod <- COVID_seg_mod(df = T4D_df, period = "T4D", formula = death_formula)


deathforest <- plot_models(T2D_mod, T4D_mod, rm.terms = c("deathlo", "dtper100k"),
            show.p = T, m.labels = c("T2", "T4"), vline.color = "grey",
            show.values = F, spacing = 0.8, value.size = 3, show.legend = F) +
  # labs(color = "Critical Periods", title = "DoW Effects for Deaths") +
  scale_color_discrete(name = "Critical Periods", labels = c("T1", "T2", "T3", "T4")) +
  scale_color_manual(values = c(color[1], color[3])) +
  ylim(0, 100) +
  coord_flip(ylim = c(0, 3.5))
  # theme(axis.title.x = element_blank(),
  #       axis.text.x = element_blank())

T1_test <- COVID_seg_mod(df = T2_df, period = "T1", formula = dtper100k ~ testlo + weekfac)
tidy(T1_test, exponentiate = T, conf.int = T)
T2_test <- COVID_seg_mod(df = T3_df, period = "T2", formula = dtper100k ~ testlo + weekfac)
tidy(T1_test, exponentiate = T, conf.int = T)
T3_test <- COVID_seg_mod(df = T4_df, period = "T3", formula = dtper100k ~ testlo + weekfac)
tidy(T1_test, exponentiate = T, conf.int = T)
T4_test <- COVID_seg_mod(df = T5_df, period = "T4", formula = dtper100k ~ testlo + weekfac)
tidy(T1_test, exponentiate = T, conf.int = T)
all_test <- glm.nb(dtper100k ~ testlo + weekfac + T2 + T3 + T4 + T5 + I(T2**2) + I(T3**2) +
                     I(T4**2) + I(T5**2), data = MD)
tidy(all_test, exponentiat = T, conf.int = T)


testforest <- plot_models(T1_test, T2_test, T3_test, T4_test, rm.terms = c('testlo'),
            show.p = T, m.labels = c("T1", "T2", "T3", "T4"), vline.color = "grey",
            show.values = F, spacing = 0.8, value.size = 3, show.legend = F) +
  # labs(color = "Critical Periods", title = "DoW Effects for Tests") +
  scale_color_discrete(name = "Critical Periods", labels = c("T1", "T2", "T3", "T4")) +
  scale_color_manual(values = color) +
  ylim(0, 100) +
  coord_flip(ylim = c(0, 3.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())


# Arrange forest plots ----------------------------------------------------

COVID_forest <- ggarrange(testforest, caseforest, deathforest, ncol = 1, labels = c("A", "B", "C"),
                          align = "v")
annotate_figure(COVID_forest, top = text_grob("DoW Effect by Critical Period",
                                            just = "centre", color = "black", face = "bold",
                                            size = 14))

