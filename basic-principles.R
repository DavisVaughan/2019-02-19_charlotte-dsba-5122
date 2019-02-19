# ------------------------------------------------------------------------------

library(AmesHousing)
ames <- make_ames()

# ------------------------------------------------------------------------------

library(tidymodels)
library(tidyverse)
theme_set(theme_bw())

# ------------------------------------------------------------------------------

library(purrr)

mini_ames <- ames %>%
  select(Alley, Sale_Price, Year_Sold) %>%
  filter(Alley != "No_Alley_Access") %>%
  mutate(Alley = fct_drop(Alley))

head(mini_ames, n = 5)

# ------------------------------------------------------------------------------

by_alley <- split(mini_ames, mini_ames$Alley)
map(by_alley, head, n = 2)

# ------------------------------------------------------------------------------

map(by_alley, nrow)

map_int(by_alley, nrow)

map(
  by_alley, 
  ~summarise(.x, max_price = max(Sale_Price))
)

# ------------------------------------------------------------------------------

ames_lst_col <- nest(mini_ames, -Alley)
ames_lst_col

ames_lst_col <- ames_lst_col %>%
  mutate(
    model = map(
      .x = data,
      .f =  ~lm(Sale_Price ~ Year_Sold, data = .x)
    ),
    perf  = map(model, broom::tidy)
  )

ames_lst_col

# ------------------------------------------------------------------------------

unnest(ames_lst_col, perf)

# ------------------------------------------------------------------------------

library(AmesHousing)

# Remove quality-related predictors
ames <- dplyr::select(ames, -matches("Qu"))

nrow(ames)

# Make sure that you get the same random numbers
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

nrow(ames_train) / nrow(ames)

# ------------------------------------------------------------------------------

# result of initial_split()
# <training / testing / total>
data_split

training(data_split)

# ------------------------------------------------------------------------------

ggplot(ames_train, aes(x = Sale_Price)) + 
  geom_line(stat = "density", trim = TRUE) + 
  geom_line(data = ames_test, 
            stat = "density", 
            trim = TRUE, col = "red") 

# ------------------------------------------------------------------------------

simple_lm <- lm(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)

simple_lm_values <- augment(simple_lm)
names(simple_lm_values)

# ------------------------------------------------------------------------------

# # optional plots 
# 
# simple_lm_values %>%
#   ggplot(aes(x = 10 ^ .fitted, y = 10 ^ `log10.Sale_Price.`)) +
#   geom_point(alpha = .3) +
#   geom_abline(col = "green", alpha = .5) +
#   geom_smooth(se = FALSE, col = "red", lty = 2, lwd = 1, alpha = .5)
# 
# simple_lm_values %>%
#   ggplot(aes(x = .fitted, y = .std.resid)) +
#   geom_point(alpha = .3) +
#   geom_hline(col = "green", alpha = .5, yintercept = 0) +
#   geom_smooth(se = FALSE, col = "red", lty = 2, lwd = 1, alpha = .5)
# 
# # Non linear relationship w/ longitude and latitude
# simple_lm_values %>%
#   ggplot(aes(x = Longitude, y = .std.resid)) +
#   geom_point(alpha = .3) +
#   geom_hline(col = "green", alpha = .5, yintercept = 0) +
#   geom_smooth(se = FALSE, col = "red", lty = 2, lwd = 1, alpha = .5)
# 
# 
# simple_lm_values %>%
#   ggplot(aes(x = Latitude, y = .std.resid)) +
#   geom_point(alpha = .3) +
#   geom_hline(col = "green", alpha = .5, yintercept = 0) +
#   geom_smooth(se = FALSE, col = "red", lty = 2, lwd = 1, alpha = .5)

# ------------------------------------------------------------------------------

spec_lin_reg <- linear_reg()
spec_lin_reg

spec_lm <- set_engine(spec_lin_reg, "lm")
spec_lm

fit_lm <- fit(
  spec_lm,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)

fit_lm

# ------------------------------------------------------------------------------

ames_train_log <- ames_train %>%
  mutate(Sale_Price_Log = log10(Sale_Price))

fit_xy(
  spec_lm,
  y = ames_train_log$Sale_Price_Log,
  x = ames_train_log[, c("Latitude", "Longitude")]
)

# ------------------------------------------------------------------------------

# # optional:
# 
# library(rstanarm)
# # reset the theme since rstanarm changes it :-(
# theme_set(theme_bw())
# 
# spec_stan <- 
#   spec_lin_reg %>%
#   # Engine specific arguments are passed through here
#   set_engine("stan", chains = 4, iter = 1000)
# 
# # Otherwise, looks exactly the same!
# fit_stan <- fit(
#   spec_stan,
#   log10(Sale_Price) ~ Longitude + Latitude,
#   data = ames_train
# )
# 
# coef(fit_stan$fit)
# 
# coef(fit_lm$fit)

# ------------------------------------------------------------------------------

summary(fit_lm$fit)

# ------------------------------------------------------------------------------

set.seed(2453)
cv_splits <- vfold_cv(
  data = ames_train, 
  v = 10, 
  strata = "Sale_Price"
)
cv_splits %>% slice(1:6)

cv_splits$splits[[1]]

cv_splits$splits[[1]] %>% analysis() %>% dim()
cv_splits$splits[[1]] %>% assessment() %>% dim()

# ------------------------------------------------------------------------------

geo_form <- log10(Sale_Price) ~ Latitude + Longitude

# Fit on a single analysis resample
fit_model <- function(split, spec) {
  fit(
    object = spec, 
    formula = geo_form,
    data = analysis(split) # <- pull out training set
  )
}

# For each resample, call fit_model()
cv_splits <- cv_splits %>% 
  mutate(models_lm = map(splits, fit_model, spec_lm))

cv_splits

# ------------------------------------------------------------------------------

compute_pred <- function(split, model) {
  
  # Extract the assessment set
  assess <- assessment(split) %>%
    mutate(Sale_Price_Log = log10(Sale_Price))
  
  # Compute predictions (a df is returned)
  pred <- predict(model, new_data = assess)
  
  bind_cols(assess, pred)
}

cv_splits <- cv_splits %>%
  mutate(pred_lm = map2(splits, models_lm, compute_pred))

cv_splits

# ------------------------------------------------------------------------------

compute_perf <- function(pred_df) {
  
  # Create a function that calculates
  # rmse and rsq and returns a data frame
  numeric_metrics <- metric_set(rmse, rsq)
  
  numeric_metrics(
    pred_df, 
    truth = Sale_Price_Log, 
    estimate = .pred
  )
}

cv_splits <- cv_splits %>%
  mutate(perf_lm = map(pred_lm, compute_perf))

cv_splits

# ------------------------------------------------------------------------------

cv_splits$perf_lm[[1]]

cv_splits %>%
  unnest(perf_lm) %>%
  group_by(.metric) %>%
  summarise(.estimate = mean(.estimate))

# ------------------------------------------------------------------------------

holdout_results <- 
  cv_splits %>%
  unnest(pred_lm) %>%
  mutate(.resid = Sale_Price_Log - .pred)

holdout_results %>% dim()
ames_train %>% dim()
