library("tidyverse")
library("tidyr")
library("ds4ling")
library("here")
library("lme4")
library("broom")

dat_raw <- read_csv(here("data", "raw", "survey_data.csv"))

head(dat_raw)

dat_raw$q1_time <- dat_raw$q1_1+
  +   dat_raw$q1_2+
  +   dat_raw$q1_3+
  +   dat_raw$q1_4+
  +   dat_raw$q1_5+
  +   dat_raw$q1_6+
  +   dat_raw$q1_7+
  +   dat_raw$q1_8+
  +   dat_raw$q1_9+
  +   dat_raw$q1_10+
  +   dat_raw$q1_11+
  +   dat_raw$q1_12+
  +   dat_raw$q1_13+
  +   dat_raw$q1_14+
  +   dat_raw$q1_15+
  +   dat_raw$q1_16+
  +   dat_raw$q1_17+
  +   dat_raw$q1_18+
  +   dat_raw$q1_19+
  +   dat_raw$q1_20

head(dat_raw)

dat_raw$q1_per <- dat_raw$q1_time / 20
dat_raw$q1_logit <- log((dat_raw$q1_per)/(1-dat_raw$q1_per))

dat_raw |> 
  ggplot() +
  aes(x = sentence, y = q1_logit, fill = survey) +
  geom_boxplot() 

View(dat_raw)

mod1<- lm(q1_logit ~ survey * sentence, data = dat_raw)
summary(mod1)

dat_raw |> 
 relocate(q1_per, q1_logit, .after = sentence)
