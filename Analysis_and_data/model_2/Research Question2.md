---
title: "Simultaneous language learning"
output:
  pdf_document: default
  html_document:
    df_print: paged
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

#The following packages were used in this analysis:
library(rmarkdown)
library(lme4)
library(emmeans)
library(lmerTest)
library(pbkrtest)
library(MuMIn)
library(car)
library (easystats)
library (performance)
library(dplyr)
library(ggplot2)
library(knitr)
library(sjstats)
library (effectsize)
library(lsr)
options(scipen = 999)
library (moments)
library(ordinal)
library(glmmTMB)
library(scales)





```{r echo = TRUE, comment = NA}
knitr::opts_chunk$set(echo = TRUE)

#English analysis

file_path <- "~/Desktop/Simultaneous_learning.csv"
data <- read.csv(file_path)

data$Time <- factor(data$Time)
data$Type <- factor(data$Target.Control)
data$Categorical_score <- factor(data$Categorical_score, ordered = TRUE, levels = 1:2)
data$Language <- factor(data$Language)


data <- subset(data, Language == "English")


data$Time <- factor(data$Time)
data$Type <- factor(data$Target.Control)
data$Categorical_score <- factor(data$Categorical_score, ordered = TRUE, levels = 1:2)
data$Language <- factor(data$Language)


model_int <- glmer(
  Categorical_score ~ Time * Target.Control + English_proficiency + Dutch_proficiency +
    (1 | Participant) + (1 | ItemID),
  family = binomial, data = data
)
summary (model_int)

r2_model_int <- performance::r2(model_int)
print(r2_model_int)

emm <- emmeans(model_int, ~ Time * Target.Control, type = "response")
summary(emm)
pairs(emm, by = "Target.Control")      # change over Time within each group
pairs(emm, by = "Time") 


library(ggplot2)

emm <- emmeans(model_int, ~ Time * Target.Control)
marginal_means_df <- as.data.frame(emm)

marginal_means_df$Target.Control <- factor(marginal_means_df$Target.Control, labels = c("Control", "Target"))
marginal_means_df$Time <- factor(marginal_means_df$Time,
                                 levels = c(1, 2),
                                 labels = c("Pretest", "Posttest"))

ggplot(marginal_means_df, aes(x = Time, y = emmean, color = Target.Control, group = Target.Control)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  geom_line(aes(linetype = Target.Control), position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.1, position = position_dodge(width = 0.2)) +
  labs(
    title = "(A) English learning outcomes",
    x = "Time",
    y = "Estimated Marginal Means",
    color = "Condition",
    linetype = "Condition"
  ) +
  scale_color_manual(values = c("grey", "black")) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  scale_y_continuous(limits = c(-8, -1)) +
  theme_minimal() +
  theme(text = element_text(size = 14))







```

```{r echo = TRUE, comment = NA}
knitr::opts_chunk$set(echo = TRUE)



#Dutch analysis

file_path <- "~/Desktop/Simultaneous_learning.csv"
data <- read.csv(file_path)

data$Time <- factor(data$Time)
data$Type <- factor(data$Target.Control)
data$Categorical_score <- factor(data$Categorical_score, ordered = TRUE, levels = 1:2)
data$Language <- factor(data$Language)


data <- subset(data, Language == "Dutch")



model_int <- glmer(
  Categorical_score ~ Time * Target.Control + English_proficiency + Dutch_proficiency +
    (1 | Participant) + (1 | ItemID),
  family = binomial, data = data
)
summary (model_int)
r2_model_int <- performance::r2(model_int)
print(r2_model_int)

emm <- emmeans(model_int, ~ Time * Target.Control, type = "response")
summary(emm)
pairs(emm, by = "Target.Control")      # change over Time within each group
pairs(emm, by = "Time") 


library(ggplot2)

emm <- emmeans(model_int, ~ Time * Target.Control)
marginal_means_df <- as.data.frame(emm)

marginal_means_df$Target.Control <- factor(marginal_means_df$Target.Control, labels = c("Control", "Target"))
marginal_means_df$Time <- factor(marginal_means_df$Time,
                                 levels = c(1, 2),
                                 labels = c("Pretest", "Posttest"))

ggplot(marginal_means_df, aes(x = Time, y = emmean, color = Target.Control, group = Target.Control)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  geom_line(aes(linetype = Target.Control), position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.1, position = position_dodge(width = 0.2)) +
  labs(
    title = "(B) Dutch learning outcomes",
    x = "Time",
    y = "Estimated Marginal Means",
    color = "Condition",
    linetype = "Condition"
  ) +
  scale_color_manual(values = c("grey", "black")) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  scale_y_continuous(limits = c(-8, -1)) +
  theme_minimal() +
  theme(text = element_text(size = 14))





```





```{r echo = TRUE, comment = NA}















