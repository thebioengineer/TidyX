
## Episode 183: Within-group regression using {purrr}

library(tidyverse)
library(broom)
library(palmerpenguins)

theme_set(theme_light())

data("penguins")
d <- penguins

d %>%
  head()

### Creating within group linear regression models


group_fit <- d %>%
  split(.$species) %>%
  # map() makes a list of outputs
  map(~ lm(bill_length_mm ~ flipper_length_mm + body_mass_g + sex, data = .x)) %>%
  # map_dfr() converts the list into a data frame of model coefficients, using tidy()
  map_dfr(~ tidy(.), .id = 'species')

group_fit

group_fit %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = estimate, y = species)) +
  geom_point(aes(fill = species), 
             color = "black",
             shape = 21,
             size = 4) +
  geom_errorbar(aes(xmin = estimate - 2*std.error, xmax = estimate + 2*std.error, color = species),
                width = 0.1) +
  facet_wrap(~term, scales = "free")


### storing model coefficients and all predicted values in a list
# nest the data into a list by species
penguins_nest <- penguins %>%
  drop_na() %>%
  group_by(species) %>%
  nest() 

penguins_nest

# fit model and make predictions
penguins_pred <- penguins_nest %>%
  mutate(
    # fit the model on each list
    fit = map(data, 
                ~lm(bill_length_mm ~ flipper_length_mm + body_mass_g + sex, 
                   data = .x)
              ),
    # make predictions within each list
    pred = map(fit, predict)
    )

# now everything is contained in lists within each species
penguins_pred

# check out the predictions
penguins_pred %>%
  unnest(cols = pred)

# check out the model coefficients
penguins_pred %>%
  # first tidy() the coefficients
  mutate(model_fit = map(fit, tidy)) %>%
  # now unnest
  unnest(model_fit)

# model predictions and true values
penguins_pred %>%
  unnest(cols = data) %>%
  select(species, bill_length_mm) %>%
  cbind(penguins_pred %>%
          unnest(cols = pred) %>%
          ungroup() %>%
          select(pred)) %>%
  ggplot(aes(x = pred, y = bill_length_mm, color = species)) +
  geom_point(size = 4) +
  geom_abline(intercept = 0, 
              slope = 1, 
              color = "black",
              size = 1.2,
              linetype = "dashed")


