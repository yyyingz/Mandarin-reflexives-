library("tidyverse")
library("tidyr")
library("dplyr")
library("scales")
library("ds4ling")
library("here")
library("lme4")
library("broom")

dat_raw <- read_csv(here("data", "raw", "survey_data.csv"))

head(dat_raw)

dat1 <- dat_raw |> 
  pivot_longer(
    cols      = starts_with("item_"),
    names_to  = c("item", "blocking", "question"), 
    names_prefix = "item_",
    names_sep   = "[.]",
    values_to = "response"
  ) |> 
  mutate(
    item = as.integer(item),
    blocking = blocking
  )

dat1 |> 
  write_csv(here("data", "tidy", "survey_data_tidy.csv"))

dat1 |> 
  ggplot() +
  aes(x = sentence, fill = factor(response, labels = c("Non-local","Local")))+
  geom_bar(position = "fill") +
  facet_wrap(~ survey) +
  scale_y_continuous(labels = percent_format())+
  labs(
    x     = "Sentence Structure",
    y     = "Proportion of Binders",
    fill  = "Response",
    title = "Proportion of binders by Sentence & Survey"
  ) 


mod_0 <- glmer(
  formula = response ~ 1 + (1 | id),
  family = binomial(link = "logit"),
  data = dat1
)
summary(mod_0)

mod_1 <- glmer(
  formula = response ~ sentence*survey + (1 | id),
  family = binomial(link = "logit"),
  data = dat1
)

summary(mod_1)
plogis(0.55397)
anova(mod_1, mod_0) 

mod_2 <- glmer(
  formula = response ~ sentence + survey + (1 | id),
  family = binomial(link = "logit"),
  data = dat1
)

summary(mod_2)

anova(mod_2, mod_1)
