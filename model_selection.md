Model Selection
================
Peilin Zhou

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(modelr)
library(broom)
```

    ## 
    ## Attaching package: 'broom'

    ## The following object is masked from 'package:modelr':
    ## 
    ##     bootstrap

``` r
#install.packages("olsrr")
library(olsrr)
```

    ## 
    ## Attaching package: 'olsrr'

    ## The following object is masked from 'package:datasets':
    ## 
    ##     rivers

``` r
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
cdi = read.csv("./data/cdi.csv")
```

## Model selection

Examining 7 candidate models using some measures of goodness, including
Cp, AIC, BIC, adjusted R^2, partial F-test for nested models, and
bootstrap.

``` r
cdi = 
  cdi %>% 
  mutate(
    # crm_1000 is already generated in Xiao's part
    crm_1000 = crimes/pop * 1000,
    poparea = pop/area,
    docs = docs/pop * 1000,
    beds = beds/pop * 1000,
    # mutatation for the region needs to stay here (not in Xiao's part)
    region = as.factor(region)
  ) %>% 
  select(-id,-cty,-state,-crimes)
```

``` r
#List of fitted models

#full model
model1 = lm(crm_1000 ~ ., data = cdi)
#Based on observation-> Base Model
model2 = lm(crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region + hsgrad + pop65, data = cdi)
#stepwise from 2
model3 = lm(crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region, data = cdi)
#interaction+stepwise from 3
model4 = lm(crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region + poparea*region + poparea*poverty + poparea*beds, data = cdi)
#Based on internet searching
model5 = lm(crm_1000 ~ pop18 + beds + hsgrad + poverty + unemp + pcincome + poparea, data = cdi)
#stepwise from 5
model6 = lm(crm_1000 ~ pop18 + beds + poverty + unemp + pcincome + poparea, data = cdi)
#stepwise from full model
model7 = lm(crm_1000 ~ pop + pop18 + beds + hsgrad + bagrad + poverty + pcincome + totalinc + poparea + region, data = cdi)
```

### Interaction

Model 4 is the model with interaction terms after backward selection.
The interaction terms include number of people per square mile with
number of hospital beds per thousand people, number of people per square
mile with percent of population with income below poverty level, and
number of people per square mile with region. Since the difference
between model 4 and model 3 is the added interaction terms, performing a
partial F-test can verify whether the interaction terms significantly
improve our model or not. If the p-value is less than *α* = 0.05, it
indicates that the interaction terms do help to improve the model and we
should include them.

``` r
anova(model3, model4) %>% knitr::kable(caption = "ANOVA Table", digits = 3 )
```

| Res.Df |      RSS |  Df | Sum of Sq |      F | Pr(&gt;F) |
|-------:|---------:|----:|----------:|-------:|----------:|
|    431 | 140871.9 |  NA |        NA |     NA |        NA |
|    426 | 125410.5 |   5 |  15461.39 | 10.504 |         0 |

ANOVA Table

Based on the ANOVA table, the p-value is extremely close to 0,
indicating that the interaction terms should be preserved and model 4 is
relatively better than model 3. However, results of comparisons based on
other criteria will be considered as well.

``` r
model_list = list(model1 = model1, model2 = model2, model3 = model3, model4 = model4, model5 = model5, model6 = model6, model7 = model7)

output_list = lapply(model_list, glance)
output_df = bind_rows(output_list) %>%
  select(adj.r.squared, AIC, BIC)

#write a function to get Mallow's Cp for every model.
Cp_func = function(x) {
  Cp = ols_mallows_cp(x, model1)
}

Cp_val = map(model_list, Cp_func) %>% bind_rows() %>% 
  pivot_longer(
    1:7,
    names_to = "Model",
    names_prefix = "model",
    values_to = "Cp"
  )

#Get number of parameters
output = vector(length = 7)
for (i in 1:7) {
  output[i] = length(coef(model_list[[i]]))
}
output = output %>% as_tibble() %>% rename(Num_Parameters = value)

#Make Comparison Table
output_df = bind_cols(output_df, Cp_val) %>% 
  relocate(Model) %>% 
  mutate(Cp = round(Cp, digits = 3)) %>% 
  rename(Adj_R_Sq = adj.r.squared) %>% 
  mutate(Adj_R_Sq = round(Adj_R_Sq, digits = 3))

output_df_final = bind_cols(output_df, output) %>%
  knitr::kable(col.names = c("Model", "Adjusted $R^2$", "AIC", "BIC", "Cp", "Number of Parameters"), captions = "Model Comparison Table")

output_df_final
```

| Model | Adjusted *R*<sup>2</sup> |      AIC |      BIC |      Cp | Number of Parameters |
|:------|-------------------------:|---------:|---------:|--------:|---------------------:|
| 1     |                    0.608 | 3766.076 | 3839.638 |  13.000 |                   17 |
| 2     |                    0.561 | 3810.282 | 3859.323 |  58.637 |                   11 |
| 3     |                    0.562 | 3806.952 | 3847.820 |  55.370 |                    9 |
| 4     |                    0.606 | 3765.798 | 3827.100 |   8.537 |                   14 |
| 5     |                    0.445 | 3910.270 | 3947.051 | 187.549 |                    8 |
| 6     |                    0.444 | 3910.173 | 3942.868 | 188.200 |                    7 |
| 7     |                    0.611 | 3759.247 | 3816.462 |   6.128 |                   13 |

The optimal model is selected based on various criteria. We use 5
methods to compare different candidate models. The adjusted
*R*<sup>2</sup> is an improved version of *R*<sup>2</sup>, which only
increases if the predictor is significant. In this case, it measures the
percentage of change in crime rate per 1000 people that the predictors
can explain collectively in a model. Higher adjusted *R*<sup>2</sup>
value represents smaller difference between the observed and fitted
value. Thus, based on the table, model 1, 4, and 7 have relatively
highest *R*<sup>2</sup> values, but since model 1 is the full model, we
choose to neglect it and focus on reduced models with similar strength
instead. AIC and BIC are both information criteria which they penalize
for complexity and reward for goodness of fit. Lower AIC/BIC values are
preferred. Based on the table, model 4 and 7 have the lowest AIC and BIC
values. Mallow’s Cp compares the reduced model with the full model and
measures the amount of error unexplained by the reduced model. Model
with lower Cp or Cp close to the number of parameters is preferred. We
can compare the number of parameters listed in the table with the Cp
value for each model. It is quite obvious to observe that Model 4 and 7
have the lowest Cp and the finding is consistent with other criteria.

The criterion above helped us to find two best candidate models,
however, these measures are all internal, which only provide information
of how well the model fits the training data. To evaluate the
performance of the model, and since our models are fitted using the
whole dataset, we choose to apply bootstrap resampling method to measure
the accuracy. By evaluating the model on every randomly selected sample
of the original dataset, we can get an averaged value of the standard
error.

``` r
set.seed(1)
# Define training control
train.control <- trainControl(method = "boot", number = 1000)
# Train the model
boot_model7 = train(crm_1000 ~ pop + pop18 + beds + hsgrad + bagrad + poverty + pcincome + totalinc + poparea + region, data = cdi, method = "lm", trControl = train.control)

boot_model4 = train(crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region + poparea*region + poparea*poverty + poparea*beds, data = cdi, method = "lm", trControl = train.control)
# Summarize the results
res_4 = boot_model4$results %>% as_tibble() %>% select(RMSE) %>% mutate(Model = "Model4")

res_7 = boot_model7$results %>% as_tibble() %>% select(RMSE) %>% 
  mutate(Model = "Model7")

res = rbind(res_4,res_7) %>% relocate(Model) %>% 
  knitr::kable(caption = "RMSE Table", col.names = c("Model", "RMSE"))

res
```

| Model  |     RMSE |
|:-------|---------:|
| Model4 | 18.90105 |
| Model7 | 19.01657 |

RMSE Table

The 1000 bootstrapping samples of the original data provide us the
estimated Root Mean Squared Error of the model, which measures the
average prediction error made of the model in predicting the outcome and
tell us how accurate the predictions are. In the table above, model 4
has a smaller RMSE value but the difference is rather small. However,
since model 7 is purely based on stepwise selection, it only provides us
as a guide of what a “good” model under automatic procedure looks like.
Thus, based on the above observation, model 4 is considered as the
optimal model for predicting the crime rate per 1000 people for each
county for now.
