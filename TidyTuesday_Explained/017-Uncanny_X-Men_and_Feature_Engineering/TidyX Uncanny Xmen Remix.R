
## Extending Rebecca's analysis!


# simple linear regression
flyinghugging %>%
  lm(hug ~ fly, data = .) %>%
  summary()

# correlation between fly and hug
with(flyinghugging, cor.test(fly, hug))

# r2 between fly and hug -- same as the multiple r2 from the linear regression
with(flyinghugging, cor.test(fly, hug))$estimate ^ 2


# Show how regression with z-scores produces a beta that represents the correlation coefficient
# NOTE: was having an issue with the sd() function working on her dataset -- kept returning NA
flyinghugging %>%
  mutate(hug_z = (hug - 6.9) / 7.04,
         fly_z = (fly - 7.82) / 7.98) %>%
  lm(hug_z ~ fly_z, data = .) %>%
  summary()


## Make pretty colors for our regression output
library(broom)
library(colortable)
library(knitr)

fit <- lm(hug ~ fly, data = flyinghugging)
summary(fit)

# YUK!!
# tidy up the output with the broom package and add colors with colortable!

fit_tbl <- tidy(fit)
fit_tbl %>%
  mutate(p.value = set_styling(x = p.value,
                               idx = p.value < 0.05,
                               background = "palegreen",
                               text_color = "black",
                               style = "bold"))
