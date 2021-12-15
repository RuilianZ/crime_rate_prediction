model\_diagnostics
================
Yida Wang
12/12/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(modelr)
library(patchwork)
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:patchwork':
    ## 
    ##     area

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(ggplot2)
library(leaps)
library(performance)
```

    ## 
    ## Attaching package: 'performance'

    ## The following objects are masked from 'package:modelr':
    ## 
    ##     mae, mse, rmse

## import data

``` r
cdi = read.csv("./data/cdi.csv")

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
  dplyr::select(-id,-cty,-state,-crimes)
```

### Model 1: Full model

Let’s first fit the model with all the predictors:

``` r
model1 = lm(crm_1000 ~ ., data = cdi)

broom::tidy(model1) %>% 
  knitr::kable()
```

| term        |    estimate |  std.error |  statistic |   p.value |
|:------------|------------:|-----------:|-----------:|----------:|
| (Intercept) | -95.5189646 | 26.6636149 | -3.5823711 | 0.0003801 |
| area        |  -0.0003144 |  0.0006703 | -0.4690426 | 0.6392809 |
| pop         |   0.0000812 |  0.0000127 |  6.4106097 | 0.0000000 |
| pop18       |   0.8704346 |  0.3191779 |  2.7271140 | 0.0066549 |
| pop65       |  -0.0580139 |  0.2952493 | -0.1964914 | 0.8443200 |
| docs        |  -0.4149594 |  0.9805516 | -0.4231898 | 0.6723719 |
| beds        |   2.7526148 |  0.7668761 |  3.5893864 | 0.0003703 |
| hsgrad      |   0.5090193 |  0.2591722 |  1.9640197 | 0.0501822 |
| bagrad      |  -0.5422934 |  0.2855774 | -1.8989367 | 0.0582530 |
| poverty     |   1.9594621 |  0.3721326 |  5.2654943 | 0.0000002 |
| unemp       |   0.4436160 |  0.5117840 |  0.8668031 | 0.3865413 |
| pcincome    |   0.0026666 |  0.0005272 |  5.0584493 | 0.0000006 |
| totalinc    |  -0.0036681 |  0.0006068 | -6.0449586 | 0.0000000 |
| region2     |   9.3014507 |  2.6258092 |  3.5423178 | 0.0004408 |
| region3     |  27.2932342 |  2.5581241 | 10.6692379 | 0.0000000 |
| region4     |  21.3813487 |  3.2291786 |  6.6212965 | 0.0000000 |
| poparea     |   0.0042337 |  0.0004608 |  9.1868758 | 0.0000000 |

We can see that the variables `area`, `pop65`, `docs`, `bagrad`, `unemp`
are all not very significant with a p-value larger than 0.05.

### Model 2: Model found by looking at correlation clusters

Then, plot a heatmap for the correlations among all the variables.

``` r
res = cor(cdi %>% dplyr::select(-region))
round(res, 2) %>%
  knitr::kable()
```

|           |  area |   pop | pop18 | pop65 |  docs |  beds | hsgrad | bagrad | poverty | unemp | pcincome | totalinc | crm\_1000 | poparea |
|:----------|------:|------:|------:|------:|------:|------:|-------:|-------:|--------:|------:|---------:|---------:|----------:|--------:|
| area      |  1.00 |  0.17 | -0.05 |  0.01 | -0.12 | -0.14 |  -0.10 |  -0.14 |    0.17 |  0.20 |    -0.19 |     0.13 |      0.04 |   -0.16 |
| pop       |  0.17 |  1.00 |  0.08 | -0.03 |  0.17 |  0.02 |  -0.02 |   0.15 |    0.04 |  0.01 |     0.24 |     0.99 |      0.28 |    0.32 |
| pop18     | -0.05 |  0.08 |  1.00 | -0.62 |  0.24 |  0.03 |   0.25 |   0.46 |    0.03 | -0.28 |    -0.03 |     0.07 |      0.19 |    0.13 |
| pop65     |  0.01 | -0.03 | -0.62 |  1.00 |  0.02 |  0.25 |  -0.27 |  -0.34 |    0.01 |  0.24 |     0.02 |    -0.02 |     -0.07 |    0.03 |
| docs      | -0.12 |  0.17 |  0.24 |  0.02 |  1.00 |  0.67 |   0.14 |   0.44 |    0.06 | -0.25 |     0.36 |     0.20 |      0.31 |    0.32 |
| beds      | -0.14 |  0.02 |  0.03 |  0.25 |  0.67 |  1.00 |  -0.21 |  -0.05 |    0.37 | -0.06 |    -0.05 |     0.01 |      0.36 |    0.21 |
| hsgrad    | -0.10 | -0.02 |  0.25 | -0.27 |  0.14 | -0.21 |   1.00 |   0.71 |   -0.69 | -0.59 |     0.52 |     0.04 |     -0.23 |   -0.10 |
| bagrad    | -0.14 |  0.15 |  0.46 | -0.34 |  0.44 | -0.05 |   0.71 |   1.00 |   -0.41 | -0.54 |     0.70 |     0.22 |      0.04 |    0.16 |
| poverty   |  0.17 |  0.04 |  0.03 |  0.01 |  0.06 |  0.37 |  -0.69 |  -0.41 |    1.00 |  0.44 |    -0.60 |    -0.04 |      0.47 |    0.13 |
| unemp     |  0.20 |  0.01 | -0.28 |  0.24 | -0.25 | -0.06 |  -0.59 |  -0.54 |    0.44 |  1.00 |    -0.32 |    -0.03 |      0.04 |    0.02 |
| pcincome  | -0.19 |  0.24 | -0.03 |  0.02 |  0.36 | -0.05 |   0.52 |   0.70 |   -0.60 | -0.32 |     1.00 |     0.35 |     -0.08 |    0.23 |
| totalinc  |  0.13 |  0.99 |  0.07 | -0.02 |  0.20 |  0.01 |   0.04 |   0.22 |   -0.04 | -0.03 |     0.35 |     1.00 |      0.23 |    0.32 |
| crm\_1000 |  0.04 |  0.28 |  0.19 | -0.07 |  0.31 |  0.36 |  -0.23 |   0.04 |    0.47 |  0.04 |    -0.08 |     0.23 |      1.00 |    0.48 |
| poparea   | -0.16 |  0.32 |  0.13 |  0.03 |  0.32 |  0.21 |  -0.10 |   0.16 |    0.13 |  0.02 |     0.23 |     0.32 |      0.48 |    1.00 |

``` r
col <- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)
```

![](model_diagnostics_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

According to the clusters generated by R, we then choose variables that
are not highly related with each other and are highly related to the
outcome `crm_1000`.

``` r
model2 = lm(crm_1000 ~ pop18 + pcincome + hsgrad + pop65 + poverty + beds + poparea + region, data = cdi)

broom::tidy(model2) %>% 
  knitr::kable()
```

| term        |    estimate |  std.error |  statistic |   p.value |
|:------------|------------:|-----------:|-----------:|----------:|
| (Intercept) | -23.6681871 | 19.4889031 | -1.2144443 | 0.2252462 |
| pop18       |   0.4635470 |  0.2890869 |  1.6034866 | 0.1095632 |
| pcincome    |   0.0006934 |  0.0003131 |  2.2147905 | 0.0272992 |
| hsgrad      |   0.1432941 |  0.2092863 |  0.6846795 | 0.4939159 |
| pop65       |  -0.1037995 |  0.3079349 | -0.3370825 | 0.7362195 |
| poverty     |   1.8043209 |  0.3444210 |  5.2387073 | 0.0000003 |
| beds        |   2.5129533 |  0.5467889 |  4.5958385 | 0.0000057 |
| poparea     |   0.0051797 |  0.0004486 | 11.5462151 | 0.0000000 |
| region2     |  10.3028384 |  2.7239226 |  3.7823536 | 0.0001774 |
| region3     |  26.1491461 |  2.5354946 | 10.3132330 | 0.0000000 |
| region4     |  22.4005096 |  3.0880693 |  7.2538883 | 0.0000000 |

### Model 3: Step-wise model based on model 2

Conduct the automatic step-wise process on the predictors used in model
2.

``` r
mult.fit = lm(crm_1000 ~ pop18 + pcincome + hsgrad + pop65 + poverty + beds + poparea + region, data = cdi)
step(mult.fit, direction = "both")
```

    ## Start:  AIC=2559.62
    ## crm_1000 ~ pop18 + pcincome + hsgrad + pop65 + poverty + beds + 
    ##     poparea + region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - pop65     1        37 140695 2557.7
    ## - hsgrad    1       154 140811 2558.1
    ## <none>                  140658 2559.6
    ## - pop18     1       843 141501 2560.2
    ## - pcincome  1      1608 142266 2562.6
    ## - beds      1      6925 147583 2578.8
    ## - poverty   1      8998 149656 2584.9
    ## - region    3     39727 180384 2663.1
    ## - poparea   1     43710 184368 2676.7
    ## 
    ## Step:  AIC=2557.73
    ## crm_1000 ~ pop18 + pcincome + hsgrad + poverty + beds + poparea + 
    ##     region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - hsgrad    1       177 140872 2556.3
    ## <none>                  140695 2557.7
    ## + pop65     1        37 140658 2559.6
    ## - pop18     1      1638 142333 2560.8
    ## - pcincome  1      1667 142362 2560.9
    ## - beds      1      7553 148248 2578.7
    ## - poverty   1      9579 150273 2584.7
    ## - region    3     39987 180682 2661.8
    ## - poparea   1     43746 184440 2674.8
    ## 
    ## Step:  AIC=2556.29
    ## crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## <none>                  140872 2556.3
    ## + hsgrad    1       177 140695 2557.7
    ## + pop65     1        61 140811 2558.1
    ## - pcincome  1      2150 143022 2560.9
    ## - pop18     1      2633 143505 2562.4
    ## - beds      1      7827 148699 2578.1
    ## - poverty   1     11675 152547 2589.3
    ## - region    3     41931 182803 2664.9
    ## - poparea   1     43963 184835 2673.8

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + 
    ##     region, data = cdi)
    ## 
    ## Coefficients:
    ## (Intercept)        pop18     pcincome      poverty         beds      poparea  
    ##  -1.823e+01    5.955e-01    7.669e-04    1.691e+00    2.480e+00    5.117e-03  
    ##     region2      region3      region4  
    ##   1.099e+01    2.648e+01    2.330e+01

``` r
# Obtain a new model 3
model3 = lm(crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region, data = cdi)

broom::tidy(model3) %>% 
  knitr::kable()
```

| term        |    estimate | std.error | statistic |   p.value |
|:------------|------------:|----------:|----------:|----------:|
| (Intercept) | -18.2293958 | 9.5192418 | -1.915005 | 0.0561535 |
| pop18       |   0.5954624 | 0.2097831 |  2.838467 | 0.0047473 |
| pcincome    |   0.0007669 | 0.0002990 |  2.564681 | 0.0106647 |
| poverty     |   1.6914100 | 0.2829998 |  5.976718 | 0.0000000 |
| beds        |   2.4799771 | 0.5067843 |  4.893555 | 0.0000014 |
| poparea     |   0.0051174 | 0.0004412 | 11.597621 | 0.0000000 |
| region2     |  10.9946235 | 2.5734568 |  4.272317 | 0.0000238 |
| region3     |  26.4793530 | 2.4966602 | 10.605910 | 0.0000000 |
| region4     |  23.2995439 | 2.8713894 |  8.114380 | 0.0000000 |

Model 3 is rather good, and with a smaller number of predictors.

Then we need to see the residual plot for model 3 in order to get some
insight of some potential transformation.

``` r
cdi %>% 
  add_predictions(model3) %>% 
  add_residuals(model3) %>% 
  ggplot(aes(x = pred, y = resid)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(se = F, color = "red", method = "lm") +
  labs(title = "Risidual Plot for model 3",
       x = "Fitted Value", 
       y = "Residual")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](model_diagnostics_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

The residuals are randomly scattered so we may suppose that linearity
has already been achieved.

### Model 4: Try to add interaction terms in model 3

We can then consider adding some potential interaction terms to model 3.
So what interaction terms can we add?

We suppose the terms that both have significant main effect on the
outcomes may interact together on the outcomes, so we add the pairwise
interactions of `poparea`, `poverty`, `region` and `beds` into model 3.
Next, Apply step-wise process on it to come up with a new model.

``` r
mult.fit = lm(crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region + poparea*poverty + poparea*region + poverty*region + beds*poparea + beds*region + beds*poverty, data = cdi)
step(mult.fit, direction = "both")
```

    ## Start:  AIC=2522.11
    ## crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region + 
    ##     poparea * poverty + poparea * region + poverty * region + 
    ##     beds * poparea + beds * region + beds * poverty
    ## 
    ##                   Df Sum of Sq    RSS    AIC
    ## - beds:region      3     494.8 123919 2517.9
    ## - poverty:beds     1      44.8 123469 2520.3
    ## - poverty:region   3    1479.0 124904 2521.3
    ## - poparea:region   3    1503.3 124928 2521.4
    ## <none>                         123424 2522.1
    ## - beds:poparea     1    1230.0 124654 2524.5
    ## - pop18            1    5323.5 128748 2538.7
    ## - pcincome         1    6101.1 129526 2541.3
    ## - poverty:poparea  1    6572.5 129997 2542.9
    ## 
    ## Step:  AIC=2517.87
    ## crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region + 
    ##     poverty:poparea + poparea:region + poverty:region + beds:poparea + 
    ##     poverty:beds
    ## 
    ##                   Df Sum of Sq    RSS    AIC
    ## - poverty:beds     1      13.7 123933 2515.9
    ## - poverty:region   3    1467.0 125386 2517.1
    ## - poparea:region   3    1544.3 125464 2517.3
    ## <none>                         123919 2517.9
    ## - beds:poparea     1    1622.6 125542 2521.6
    ## + beds:region      3     494.8 123424 2522.1
    ## - pop18            1    5363.0 129282 2534.5
    ## - pcincome         1    6003.7 129923 2536.7
    ## - poverty:poparea  1    6656.2 130576 2538.9
    ## 
    ## Step:  AIC=2515.92
    ## crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region + 
    ##     poverty:poparea + poparea:region + poverty:region + beds:poparea
    ## 
    ##                   Df Sum of Sq    RSS    AIC
    ## - poverty:region   3    1477.5 125411 2515.1
    ## - poparea:region   3    1542.0 125475 2515.4
    ## <none>                         123933 2515.9
    ## + poverty:beds     1      13.7 123919 2517.9
    ## + beds:region      3     463.7 123469 2520.3
    ## - beds:poparea     1    2661.5 126594 2523.3
    ## - pop18            1    5364.6 129298 2532.6
    ## - pcincome         1    6115.2 130048 2535.1
    ## - poverty:poparea  1    7232.6 131166 2538.9
    ## 
    ## Step:  AIC=2515.13
    ## crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region + 
    ##     poverty:poparea + poparea:region + beds:poparea
    ## 
    ##                   Df Sum of Sq    RSS    AIC
    ## <none>                         125411 2515.1
    ## + poverty:region   3    1477.5 123933 2515.9
    ## + poverty:beds     1      24.2 125386 2517.1
    ## - poparea:region   3    2658.5 128069 2518.4
    ## + beds:region      3     480.8 124930 2519.4
    ## - beds:poparea     1    2413.5 127824 2521.5
    ## - pop18            1    5063.1 130474 2530.6
    ## - pcincome         1    6335.9 131746 2534.8
    ## - poverty:poparea  1    7067.1 132478 2537.2

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + 
    ##     region + poverty:poparea + poparea:region + beds:poparea, 
    ##     data = cdi)
    ## 
    ## Coefficients:
    ##     (Intercept)            pop18         pcincome          poverty  
    ##      -3.755e+01        8.416e-01        1.471e-03        1.586e+00  
    ##            beds          poparea          region2          region3  
    ##       3.118e+00       -4.149e-04        7.591e+00        2.754e+01  
    ##         region4  poverty:poparea  poparea:region2  poparea:region3  
    ##       2.468e+01        4.929e-04        4.945e-03       -4.732e-04  
    ## poparea:region4     beds:poparea  
    ##      -9.677e-04       -5.161e-04

``` r
model4 = lm(crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region + poparea*region + poparea*poverty + poparea*beds, data = cdi)

broom::tidy(model4) %>% 
  knitr::kable()
```

| term            |    estimate | std.error |  statistic |   p.value |
|:----------------|------------:|----------:|-----------:|----------:|
| (Intercept)     | -37.5488169 | 9.6968176 | -3.8722825 | 0.0001248 |
| pop18           |   0.8415873 | 0.2029341 |  4.1470967 | 0.0000407 |
| pcincome        |   0.0014707 | 0.0003170 |  4.6392019 | 0.0000047 |
| poverty         |   1.5858679 | 0.2709116 |  5.8538209 | 0.0000000 |
| beds            |   3.1175379 | 0.5576291 |  5.5907014 | 0.0000000 |
| poparea         |  -0.0004149 | 0.0019254 | -0.2154843 | 0.8294929 |
| region2         |   7.5908714 | 2.8938734 |  2.6230834 | 0.0090264 |
| region3         |  27.5420151 | 2.6753565 | 10.2947086 | 0.0000000 |
| region4         |  24.6767136 | 2.9702723 |  8.3078961 | 0.0000000 |
| poparea:region2 |   0.0049452 | 0.0019081 |  2.5916330 | 0.0098803 |
| poparea:region3 |  -0.0004732 | 0.0012779 | -0.3702908 | 0.7113499 |
| poparea:region4 |  -0.0009677 | 0.0013935 | -0.6944421 | 0.4877835 |
| poverty:poparea |   0.0004929 | 0.0001006 |  4.8995814 | 0.0000014 |
| beds:poparea    |  -0.0005161 | 0.0001802 | -2.8632393 | 0.0044004 |

### Model 5 & 6: Model based on internet searching result

In addition, we can try to find other models based on the searching
results of the potentially related factors of crime rates: age, medical
resources, educational level, poverty, unemployment rate, personal
income and population density per square mile.

``` r
model5 = lm(crm_1000 ~ pop18 + beds + hsgrad + poverty + unemp + pcincome + poparea, data = cdi)

broom::tidy(model5) %>% 
  knitr::kable()
```

| term        |    estimate |  std.error | statistic |   p.value |
|:------------|------------:|-----------:|----------:|----------:|
| (Intercept) | -22.6348024 | 20.6352727 | -1.096899 | 0.2732968 |
| pop18       |   0.5136253 |  0.2615950 |  1.963437 | 0.0502364 |
| beds        |   1.3959722 |  0.5617699 |  2.484954 | 0.0133344 |
| hsgrad      |   0.3162693 |  0.2310961 |  1.368562 | 0.1718476 |
| poverty     |   3.1510026 |  0.3572178 |  8.820955 | 0.0000000 |
| unemp       |  -1.1140543 |  0.5478396 | -2.033541 | 0.0426086 |
| pcincome    |   0.0006137 |  0.0003425 |  1.791894 | 0.0738497 |
| poparea     |   0.0046179 |  0.0004994 |  9.247351 | 0.0000000 |

Then we use step-wise process based on the predictors used in model 4.

``` r
mult.fit = lm(crm_1000 ~ pop18 + beds + hsgrad + poverty + unemp + pcincome + poparea, data = cdi)
step(mult.fit, direction = "both")
```

    ## Start:  AIC=2659.6
    ## crm_1000 ~ pop18 + beds + hsgrad + poverty + unemp + pcincome + 
    ##     poparea
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - hsgrad    1       776 179745 2659.5
    ## <none>                  178969 2659.6
    ## - pcincome  1      1330 180299 2660.9
    ## - pop18     1      1597 180566 2661.5
    ## - unemp     1      1713 180682 2661.8
    ## - beds      1      2558 181527 2663.8
    ## - poverty   1     32235 211203 2730.5
    ## - poparea   1     35426 214395 2737.1
    ## 
    ## Step:  AIC=2659.51
    ## crm_1000 ~ pop18 + beds + poverty + unemp + pcincome + poparea
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## <none>                  179745 2659.5
    ## + hsgrad    1       776 178969 2659.6
    ## - pcincome  1      1995 181739 2662.4
    ## - beds      1      2382 182126 2663.3
    ## - pop18     1      2591 182336 2663.8
    ## - unemp     1      3059 182803 2664.9
    ## - poverty   1     33862 213606 2733.4
    ## - poparea   1     34659 214403 2735.1

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop18 + beds + poverty + unemp + pcincome + 
    ##     poparea, data = cdi)
    ## 
    ## Coefficients:
    ## (Intercept)        pop18         beds      poverty        unemp     pcincome  
    ##   0.4516689    0.6229671    1.3439240    2.9531064   -1.3866805    0.0007286  
    ##     poparea  
    ##   0.0045065

``` r
model6 = lm(crm_1000 ~ pop18 + beds + poverty + unemp + pcincome + poparea, data = cdi)

broom::tidy(model6) %>% 
  knitr::kable()
```

| term        |   estimate |  std.error | statistic |   p.value |
|:------------|-----------:|-----------:|----------:|----------:|
| (Intercept) |  0.4516689 | 11.8966833 |  0.037966 | 0.9697323 |
| pop18       |  0.6229671 |  0.2493467 |  2.498397 | 0.0128454 |
| beds        |  1.3439240 |  0.5610458 |  2.395391 | 0.0170265 |
| poverty     |  2.9531064 |  0.3269711 |  9.031706 | 0.0000000 |
| unemp       | -1.3866805 |  0.5108523 | -2.714445 | 0.0069039 |
| pcincome    |  0.0007286 |  0.0003324 |  2.192205 | 0.0288964 |
| poparea     |  0.0045065 |  0.0004932 |  9.137395 | 0.0000000 |

Draw the residual plots:

``` r
res5 = 
  cdi %>% 
  add_predictions(model5) %>% 
  add_residuals(model5) %>% 
  ggplot(aes(x = pred, y = resid)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(se = F, color = "red", method = "lm") +
  labs(title = "Risidual Plot for model 5",
       x = "Fitted Value", 
       y = "Residual")

res6 = 
  cdi %>% 
  add_predictions(model6) %>% 
  add_residuals(model6) %>% 
  ggplot(aes(x = pred, y = resid)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(se = F, color = "red", method = "lm") +
  labs(title = "Risidual Plot for model 6",
       x = "Fitted Value", 
       y = "Residual")

res5 + res6
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](model_diagnostics_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

By the residual plot for the above two models, we cannot see a big
difference.

### Model 7: Step-wise model generated with all the variables

``` r
# Use step-wise regression to try to find a mlr model
mult.fit = lm(crm_1000 ~ ., data = cdi)
step(mult.fit, direction = "both")
```

    ## Start:  AIC=2515.41
    ## crm_1000 ~ area + pop + pop18 + pop65 + docs + beds + hsgrad + 
    ##     bagrad + poverty + unemp + pcincome + totalinc + region + 
    ##     poparea
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - pop65     1        11 123801 2513.4
    ## - docs      1        52 123843 2513.6
    ## - area      1        64 123854 2513.6
    ## - unemp     1       220 124010 2514.2
    ## <none>                  123790 2515.4
    ## - bagrad    1      1055 124845 2517.1
    ## - hsgrad    1      1129 124919 2517.4
    ## - pop18     1      2176 125967 2521.1
    ## - beds      1      3770 127560 2526.6
    ## - pcincome  1      7488 131278 2539.2
    ## - poverty   1      8114 131904 2541.3
    ## - totalinc  1     10694 134484 2549.9
    ## - pop       1     12027 135817 2554.2
    ## - poparea   1     24699 148489 2593.5
    ## - region    3     37378 161168 2625.5
    ## 
    ## Step:  AIC=2513.45
    ## crm_1000 ~ area + pop + pop18 + docs + beds + hsgrad + bagrad + 
    ##     poverty + unemp + pcincome + totalinc + region + poparea
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - docs      1        51 123853 2511.6
    ## - area      1        69 123871 2511.7
    ## - unemp     1       211 124012 2512.2
    ## <none>                  123801 2513.4
    ## - bagrad    1      1057 124858 2515.2
    ## + pop65     1        11 123790 2515.4
    ## - hsgrad    1      1143 124945 2515.5
    ## - pop18     1      3134 126935 2522.4
    ## - beds      1      3993 127795 2525.4
    ## - pcincome  1      7618 131419 2537.7
    ## - poverty   1      8607 132408 2541.0
    ## - totalinc  1     10842 134643 2548.4
    ## - pop       1     12180 135981 2552.7
    ## - poparea   1     24841 148642 2591.9
    ## - region    3     37399 161200 2623.6
    ## 
    ## Step:  AIC=2511.63
    ## crm_1000 ~ area + pop + pop18 + beds + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + totalinc + region + poparea
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - area      1        72 123925 2509.9
    ## - unemp     1       207 124060 2510.4
    ## <none>                  123853 2511.6
    ## + docs      1        51 123801 2513.4
    ## + pop65     1        10 123843 2513.6
    ## - hsgrad    1      1188 125041 2513.8
    ## - bagrad    1      1253 125105 2514.1
    ## - pop18     1      3084 126937 2520.4
    ## - beds      1      7105 130957 2534.2
    ## - pcincome  1      7567 131420 2535.7
    ## - poverty   1      8679 132531 2539.4
    ## - totalinc  1     10924 134776 2546.8
    ## - pop       1     12259 136112 2551.2
    ## - poparea   1     24888 148741 2590.2
    ## - region    3     37348 161201 2621.6
    ## 
    ## Step:  AIC=2509.89
    ## crm_1000 ~ pop + pop18 + beds + hsgrad + bagrad + poverty + unemp + 
    ##     pcincome + totalinc + region + poparea
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - unemp     1       196 124120 2508.6
    ## <none>                  123925 2509.9
    ## + area      1        72 123853 2511.6
    ## + docs      1        54 123871 2511.7
    ## + pop65     1        15 123910 2511.8
    ## - hsgrad    1      1219 125144 2512.2
    ## - bagrad    1      1253 125178 2512.3
    ## - pop18     1      3072 126996 2518.7
    ## - beds      1      7166 131091 2532.6
    ## - pcincome  1      7507 131431 2533.8
    ## - poverty   1      8630 132554 2537.5
    ## - totalinc  1     11073 134998 2545.6
    ## - pop       1     12545 136470 2550.3
    ## - poparea   1     26974 150899 2594.5
    ## - region    3     37292 161217 2619.6
    ## 
    ## Step:  AIC=2508.58
    ## crm_1000 ~ pop + pop18 + beds + hsgrad + bagrad + poverty + pcincome + 
    ##     totalinc + region + poparea
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## <none>                  124120 2508.6
    ## + unemp     1       196 123925 2509.9
    ## + area      1        60 124060 2510.4
    ## - hsgrad    1      1080 125200 2510.4
    ## + docs      1        50 124070 2510.4
    ## + pop65     1         4 124116 2510.6
    ## - bagrad    1      1524 125644 2511.9
    ## - pop18     1      3070 127190 2517.3
    ## - beds      1      7090 131210 2531.0
    ## - pcincome  1      8195 132315 2534.7
    ## - poverty   1     10410 134531 2542.0
    ## - totalinc  1     11238 135358 2544.7
    ## - pop       1     12697 136817 2549.4
    ## - poparea   1     26824 150944 2592.7
    ## - region    3     39000 163120 2622.8

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop + pop18 + beds + hsgrad + bagrad + 
    ##     poverty + pcincome + totalinc + region + poparea, data = cdi)
    ## 
    ## Coefficients:
    ## (Intercept)          pop        pop18         beds       hsgrad       bagrad  
    ##  -9.164e+01    8.049e-05    8.877e-01    2.376e+00    4.860e-01   -6.182e-01  
    ##     poverty     pcincome     totalinc      region2      region3      region4  
    ##   2.065e+00    2.717e-03   -3.646e-03    9.213e+00    2.674e+01    2.052e+01  
    ##     poparea  
    ##   4.240e-03

According the above results of step-wise regression in R, the predicting
model of `crm_1000` contains the continuous predictors `pop`, `pop18`,
`beds`, `hsgrad`, `bagrad`, `poverty`, `pcincome`, `totalinc`, `poparea`
and the categorical predictor `region`.

Then we fit the multiple linear regression model 6 for crime rates:

``` r
model7 = lm(crm_1000 ~ pop + pop18 + beds + hsgrad + bagrad + poverty + pcincome + totalinc + poparea + region, data = cdi)

broom::tidy(model7) %>% 
  knitr::kable()
```

| term        |    estimate |  std.error | statistic |   p.value |
|:------------|------------:|-----------:|----------:|----------:|
| (Intercept) | -91.6353305 | 23.7812999 | -3.853251 | 0.0001345 |
| pop         |   0.0000805 |  0.0000122 |  6.609150 | 0.0000000 |
| pop18       |   0.8877337 |  0.2731604 |  3.249862 | 0.0012460 |
| beds        |   2.3758522 |  0.4810686 |  4.938697 | 0.0000011 |
| hsgrad      |   0.4860489 |  0.2522149 |  1.927122 | 0.0546275 |
| bagrad      |  -0.6181563 |  0.2699901 | -2.289552 | 0.0225344 |
| poverty     |   2.0645606 |  0.3449861 |  5.984475 | 0.0000000 |
| pcincome    |   0.0027166 |  0.0005116 |  5.309527 | 0.0000002 |
| totalinc    |  -0.0036463 |  0.0005864 | -6.217787 | 0.0000000 |
| poparea     |   0.0042404 |  0.0004414 |  9.606180 | 0.0000000 |
| region2     |   9.2132247 |  2.5440962 |  3.621414 | 0.0003282 |
| region3     |  26.7390709 |  2.4159770 | 11.067602 | 0.0000000 |
| region4     |  20.5204410 |  2.9390087 |  6.982096 | 0.0000000 |

So we try to compare the models by ploting their RMSE values.

``` r
# I've only chose model 3, 4, 7 here to compare their RMSE values
cv = crossv_mc(cdi, 100) %>% 
    mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  ) %>% 
  mutate(
    model_3 = map(train, ~lm(crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region, data = cdi)),
    model_4 = map(train, ~lm(crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region + poparea*region + poparea*poverty + poparea*beds, data = cdi)),
    model_7 = map(train, ~lm(crm_1000 ~ pop + pop18 + beds + hsgrad + bagrad + poverty + pcincome + totalinc + poparea + region, data = cdi))) %>% 
  mutate(
    rmse_model3 = map2_dbl(model_3, test, ~rmse(model = .x)),
    rmse_model4 = map2_dbl(model_4, test, ~rmse(model = .x)),
    rmse_model7 = map2_dbl(model_7, test, ~rmse(model = .x))
    )

cv %>% 
  dplyr::select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_" 
  ) %>% 
  ggplot(aes(x = model, y = rmse)) +  geom_violin(fill = "orange",alpha = 0.4) +
  geom_boxplot(alpha = 0.5, color = "red") 
```

![](model_diagnostics_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## diagnostics

residual vs fitted plot

``` r
par(mfrow = c(2,3))

plot(model2, which = 1)
plot(model3, which = 1)
plot(model4, which = 1)
plot(model5, which = 1)
plot(model6, which = 1)
plot(model7, which = 1)
```

![](model_diagnostics_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

QQ plot

``` r
par(mfrow = c(2,3))

plot(model2, which = 2)
plot(model3, which = 2)
plot(model4, which = 2)
plot(model5, which = 2)
plot(model6, which = 2)
plot(model7, which = 2)
```

![](model_diagnostics_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Scale-Location

``` r
par(mfrow = c(2,3))

plot(model2, which = 3)
plot(model3, which = 3)
plot(model4, which = 3)
plot(model5, which = 3)
plot(model6, which = 3)
plot(model7, which = 3)
```

![](model_diagnostics_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Residual

``` r
par(mfrow = c(2,3))

plot(model2, which = 5)
plot(model3, which = 5)
plot(model4, which = 5)
plot(model5, which = 5)
plot(model6, which = 5)
plot(model7, which = 5)
```

![](model_diagnostics_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## Box-Cox transformation

``` r
boxcox(model2, lambda = seq(-3, 3, by = 0.25))
```

![](model_diagnostics_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
boxcox(model3, lambda = seq(-3, 3, by = 0.25))
```

![](model_diagnostics_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
boxcox(model4, lambda = seq(-3, 3, by = 0.25))
```

![](model_diagnostics_files/figure-gfm/unnamed-chunk-19-3.png)<!-- -->

``` r
boxcox(model5, lambda = seq(-3, 3, by = 0.25))
```

![](model_diagnostics_files/figure-gfm/unnamed-chunk-19-4.png)<!-- -->

``` r
boxcox(model6, lambda = seq(-3, 3, by = 0.25))
```

![](model_diagnostics_files/figure-gfm/unnamed-chunk-19-5.png)<!-- -->

``` r
boxcox(model7, lambda = seq(-3, 3, by = 0.25))
```

![](model_diagnostics_files/figure-gfm/unnamed-chunk-19-6.png)<!-- -->

## outliers

6 and 215 are most obvious two

``` r
sum_cdi = cdi
summary(sum_cdi)
```

    ##       area              pop              pop18           pop65       
    ##  Min.   :   15.0   Min.   : 100043   Min.   :16.40   Min.   : 3.000  
    ##  1st Qu.:  451.2   1st Qu.: 139027   1st Qu.:26.20   1st Qu.: 9.875  
    ##  Median :  656.5   Median : 217280   Median :28.10   Median :11.750  
    ##  Mean   : 1041.4   Mean   : 393011   Mean   :28.57   Mean   :12.170  
    ##  3rd Qu.:  946.8   3rd Qu.: 436064   3rd Qu.:30.02   3rd Qu.:13.625  
    ##  Max.   :20062.0   Max.   :8863164   Max.   :49.70   Max.   :33.800  
    ##       docs              beds             hsgrad          bagrad     
    ##  Min.   : 0.3559   Min.   : 0.1649   Min.   :46.60   Min.   : 8.10  
    ##  1st Qu.: 1.2127   1st Qu.: 2.1972   1st Qu.:73.88   1st Qu.:15.28  
    ##  Median : 1.7509   Median : 3.3287   Median :77.70   Median :19.70  
    ##  Mean   : 2.1230   Mean   : 3.6493   Mean   :77.56   Mean   :21.08  
    ##  3rd Qu.: 2.4915   3rd Qu.: 4.5649   3rd Qu.:82.40   3rd Qu.:25.32  
    ##  Max.   :17.0377   Max.   :19.6982   Max.   :92.90   Max.   :52.30  
    ##     poverty           unemp           pcincome        totalinc      region 
    ##  Min.   : 1.400   Min.   : 2.200   Min.   : 8899   Min.   :  1141   1:103  
    ##  1st Qu.: 5.300   1st Qu.: 5.100   1st Qu.:16118   1st Qu.:  2311   2:108  
    ##  Median : 7.900   Median : 6.200   Median :17759   Median :  3857   3:152  
    ##  Mean   : 8.721   Mean   : 6.597   Mean   :18561   Mean   :  7869   4: 77  
    ##  3rd Qu.:10.900   3rd Qu.: 7.500   3rd Qu.:20270   3rd Qu.:  8654          
    ##  Max.   :36.300   Max.   :21.300   Max.   :37541   Max.   :184230          
    ##     crm_1000          poparea        
    ##  Min.   :  4.601   Min.   :   13.26  
    ##  1st Qu.: 38.102   1st Qu.:  192.34  
    ##  Median : 52.429   Median :  335.91  
    ##  Mean   : 57.286   Mean   :  888.44  
    ##  3rd Qu.: 72.597   3rd Qu.:  756.55  
    ##  Max.   :295.987   Max.   :32403.72

``` r
sum_cdi_df = sum_cdi[-c(6,215),]
```

for model 2

``` r
old2 = lm(crm_1000 ~ pop18 + pcincome + hsgrad + pop65 + poverty + beds + poparea + region, data = cdi)
new2 = lm(crm_1000 ~ pop18 + pcincome + hsgrad + pop65 + poverty + beds + poparea + region, data = sum_cdi_df)
summary(old2); summary(new2)
```

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop18 + pcincome + hsgrad + pop65 + poverty + 
    ##     beds + poparea + region, data = cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -54.643 -11.196  -0.546  10.744  74.250 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -2.367e+01  1.949e+01  -1.214 0.225246    
    ## pop18        4.635e-01  2.891e-01   1.603 0.109563    
    ## pcincome     6.934e-04  3.131e-04   2.215 0.027299 *  
    ## hsgrad       1.433e-01  2.093e-01   0.685 0.493916    
    ## pop65       -1.038e-01  3.079e-01  -0.337 0.736220    
    ## poverty      1.804e+00  3.444e-01   5.239 2.54e-07 ***
    ## beds         2.513e+00  5.468e-01   4.596 5.68e-06 ***
    ## poparea      5.180e-03  4.486e-04  11.546  < 2e-16 ***
    ## region2      1.030e+01  2.724e+00   3.782 0.000177 ***
    ## region3      2.615e+01  2.535e+00  10.313  < 2e-16 ***
    ## region4      2.240e+01  3.088e+00   7.254 1.91e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.11 on 429 degrees of freedom
    ## Multiple R-squared:  0.571,  Adjusted R-squared:  0.561 
    ## F-statistic: 57.09 on 10 and 429 DF,  p-value: < 2.2e-16

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop18 + pcincome + hsgrad + pop65 + poverty + 
    ##     beds + poparea + region, data = sum_cdi_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -45.823 -10.998  -0.473  10.414  53.487 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -3.628e+01  1.838e+01  -1.973 0.049107 *  
    ## pop18        7.903e-01  2.785e-01   2.837 0.004767 ** 
    ## pcincome     1.251e-03  3.127e-04   4.000 7.45e-05 ***
    ## hsgrad       1.509e-02  1.996e-01   0.076 0.939759    
    ## pop65        3.485e-02  2.903e-01   0.120 0.904493    
    ## poverty      1.850e+00  3.236e-01   5.718 2.03e-08 ***
    ## beds         2.962e+00  5.193e-01   5.704 2.19e-08 ***
    ## poparea      2.316e-03  6.284e-04   3.686 0.000258 ***
    ## region2      1.180e+01  2.566e+00   4.600 5.57e-06 ***
    ## region3      2.744e+01  2.388e+00  11.492  < 2e-16 ***
    ## region4      2.439e+01  2.913e+00   8.373 8.11e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.01 on 427 degrees of freedom
    ## Multiple R-squared:  0.5385, Adjusted R-squared:  0.5277 
    ## F-statistic: 49.83 on 10 and 427 DF,  p-value: < 2.2e-16

for model 3

``` r
old3 = lm(crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region, data = cdi)
new3 = lm(crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region, data = sum_cdi_df)
summary(old3); summary(new3)
```

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + 
    ##     region, data = cdi)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -55.43 -11.37  -0.55  10.22  76.03 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.823e+01  9.519e+00  -1.915  0.05615 .  
    ## pop18        5.955e-01  2.098e-01   2.838  0.00475 ** 
    ## pcincome     7.669e-04  2.990e-04   2.565  0.01066 *  
    ## poverty      1.691e+00  2.830e-01   5.977 4.78e-09 ***
    ## beds         2.480e+00  5.068e-01   4.894 1.40e-06 ***
    ## poparea      5.117e-03  4.412e-04  11.598  < 2e-16 ***
    ## region2      1.099e+01  2.573e+00   4.272 2.38e-05 ***
    ## region3      2.648e+01  2.497e+00  10.606  < 2e-16 ***
    ## region4      2.330e+01  2.871e+00   8.114 5.15e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.08 on 431 degrees of freedom
    ## Multiple R-squared:  0.5703, Adjusted R-squared:  0.5623 
    ## F-statistic: 71.51 on 8 and 431 DF,  p-value: < 2.2e-16

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + 
    ##     region, data = sum_cdi_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -45.753 -10.981  -0.476  10.433  53.438 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -3.426e+01  9.333e+00  -3.671 0.000272 ***
    ## pop18        7.768e-01  1.992e-01   3.899 0.000112 ***
    ## pcincome     1.253e-03  2.938e-04   4.263 2.48e-05 ***
    ## poverty      1.832e+00  2.671e-01   6.858 2.45e-11 ***
    ## beds         2.987e+00  4.829e-01   6.186 1.44e-09 ***
    ## poparea      2.315e-03  6.065e-04   3.818 0.000155 ***
    ## region2      1.178e+01  2.421e+00   4.867 1.60e-06 ***
    ## region3      2.743e+01  2.350e+00  11.676  < 2e-16 ***
    ## region4      2.442e+01  2.701e+00   9.043  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.97 on 429 degrees of freedom
    ## Multiple R-squared:  0.5385, Adjusted R-squared:  0.5299 
    ## F-statistic: 62.58 on 8 and 429 DF,  p-value: < 2.2e-16

for model 4

``` r
old4 = lm(crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region + poparea*region + poparea*poverty + poparea*beds, data = cdi)
new4 = lm(crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + region + poparea*region + poparea*poverty + poparea*beds, data = sum_cdi_df)
summary(old4); summary(new4)
```

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + 
    ##     region + poparea * region + poparea * poverty + poparea * 
    ##     beds, data = cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -43.405 -11.295  -0.824  10.180  65.892 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -3.755e+01  9.697e+00  -3.872 0.000125 ***
    ## pop18            8.416e-01  2.029e-01   4.147 4.07e-05 ***
    ## pcincome         1.471e-03  3.170e-04   4.639 4.66e-06 ***
    ## poverty          1.586e+00  2.709e-01   5.854 9.60e-09 ***
    ## beds             3.118e+00  5.576e-01   5.591 4.05e-08 ***
    ## poparea         -4.149e-04  1.925e-03  -0.215 0.829493    
    ## region2          7.591e+00  2.894e+00   2.623 0.009026 ** 
    ## region3          2.754e+01  2.675e+00  10.295  < 2e-16 ***
    ## region4          2.468e+01  2.970e+00   8.308 1.31e-15 ***
    ## poparea:region2  4.945e-03  1.908e-03   2.592 0.009880 ** 
    ## poparea:region3 -4.732e-04  1.278e-03  -0.370 0.711350    
    ## poparea:region4 -9.677e-04  1.393e-03  -0.694 0.487784    
    ## poverty:poparea  4.929e-04  1.006e-04   4.900 1.37e-06 ***
    ## beds:poparea    -5.161e-04  1.802e-04  -2.863 0.004400 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.16 on 426 degrees of freedom
    ## Multiple R-squared:  0.6175, Adjusted R-squared:  0.6058 
    ## F-statistic:  52.9 on 13 and 426 DF,  p-value: < 2.2e-16

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop18 + pcincome + poverty + beds + poparea + 
    ##     region + poparea * region + poparea * poverty + poparea * 
    ##     beds, data = sum_cdi_df)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -43.66 -10.98  -0.38  10.18  52.51 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -3.492e+01  9.534e+00  -3.663 0.000281 ***
    ## pop18            8.231e-01  1.991e-01   4.133 4.31e-05 ***
    ## pcincome         1.329e-03  3.131e-04   4.246 2.68e-05 ***
    ## poverty          1.655e+00  2.721e-01   6.082 2.65e-09 ***
    ## beds             2.950e+00  5.520e-01   5.345 1.48e-07 ***
    ## poparea          4.355e-04  1.908e-03   0.228 0.819598    
    ## region2          8.136e+00  2.845e+00   2.859 0.004457 ** 
    ## region3          2.780e+01  2.646e+00  10.510  < 2e-16 ***
    ## region4          2.490e+01  2.935e+00   8.482 3.72e-16 ***
    ## poparea:region2  5.009e-03  1.873e-03   2.674 0.007780 ** 
    ## poparea:region3 -1.348e-04  1.278e-03  -0.105 0.916054    
    ## poparea:region4 -7.440e-04  1.384e-03  -0.537 0.591220    
    ## poverty:poparea  2.540e-04  1.583e-04   1.605 0.109295    
    ## beds:poparea    -2.326e-04  2.352e-04  -0.989 0.323223    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.83 on 424 degrees of freedom
    ## Multiple R-squared:  0.5512, Adjusted R-squared:  0.5375 
    ## F-statistic: 40.06 on 13 and 424 DF,  p-value: < 2.2e-16

for model 5

``` r
old5 = lm(crm_1000 ~ pop18 + beds + hsgrad + poverty + unemp + pcincome + poparea, data = cdi)
new5 = lm(crm_1000 ~ pop18 + beds + hsgrad + poverty + unemp + pcincome + poparea, data = sum_cdi_df)
summary(old5); summary(new5)
```

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop18 + beds + hsgrad + poverty + unemp + 
    ##     pcincome + poparea, data = cdi)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -59.37 -12.78  -1.84  12.43  67.70 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -2.263e+01  2.064e+01  -1.097   0.2733    
    ## pop18        5.136e-01  2.616e-01   1.963   0.0502 .  
    ## beds         1.396e+00  5.618e-01   2.485   0.0133 *  
    ## hsgrad       3.163e-01  2.311e-01   1.369   0.1718    
    ## poverty      3.151e+00  3.572e-01   8.821   <2e-16 ***
    ## unemp       -1.114e+00  5.478e-01  -2.034   0.0426 *  
    ## pcincome     6.137e-04  3.425e-04   1.792   0.0738 .  
    ## poparea      4.618e-03  4.994e-04   9.247   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.35 on 432 degrees of freedom
    ## Multiple R-squared:  0.4541, Adjusted R-squared:  0.4453 
    ## F-statistic: 51.34 on 7 and 432 DF,  p-value: < 2.2e-16

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop18 + beds + hsgrad + poverty + unemp + 
    ##     pcincome + poparea, data = sum_cdi_df)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -53.12 -12.60  -1.32  12.09  57.02 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -3.122e+01  1.993e+01  -1.567 0.117915    
    ## pop18        7.275e-01  2.576e-01   2.824 0.004958 ** 
    ## beds         1.843e+00  5.504e-01   3.348 0.000886 ***
    ## hsgrad       2.296e-01  2.250e-01   1.020 0.308235    
    ## poverty      3.228e+00  3.443e-01   9.375  < 2e-16 ***
    ## unemp       -1.109e+00  5.281e-01  -2.100 0.036279 *  
    ## pcincome     1.082e-03  3.496e-04   3.095 0.002095 ** 
    ## poparea      2.034e-03  7.170e-04   2.836 0.004777 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.6 on 430 degrees of freedom
    ## Multiple R-squared:  0.3828, Adjusted R-squared:  0.3728 
    ## F-statistic: 38.11 on 7 and 430 DF,  p-value: < 2.2e-16

for model 6

``` r
old6 = lm(crm_1000 ~ pop18 + beds + poverty + unemp + pcincome + poparea, data = cdi)
new6 = lm(crm_1000 ~ pop18 + beds + poverty + unemp + pcincome + poparea, data = sum_cdi_df)
summary(old6); summary(new6)
```

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop18 + beds + poverty + unemp + pcincome + 
    ##     poparea, data = cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -60.200 -13.267  -1.592  12.479  70.000 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.4516689 11.8966833   0.038   0.9697    
    ## pop18        0.6229671  0.2493467   2.498   0.0128 *  
    ## beds         1.3439240  0.5610458   2.395   0.0170 *  
    ## poverty      2.9531064  0.3269711   9.032   <2e-16 ***
    ## unemp       -1.3866805  0.5108523  -2.714   0.0069 ** 
    ## pcincome     0.0007286  0.0003324   2.192   0.0289 *  
    ## poparea      0.0045065  0.0004932   9.137   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.37 on 433 degrees of freedom
    ## Multiple R-squared:  0.4517, Adjusted R-squared:  0.4441 
    ## F-statistic: 59.46 on 6 and 433 DF,  p-value: < 2.2e-16

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop18 + beds + poverty + unemp + pcincome + 
    ##     poparea, data = sum_cdi_df)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -53.68 -12.78  -0.99  12.19  55.54 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.499e+01  1.200e+01  -1.249 0.212274    
    ## pop18        8.130e-01  2.436e-01   3.337 0.000919 ***
    ## beds         1.820e+00  5.500e-01   3.309 0.001014 ** 
    ## poverty      3.087e+00  3.155e-01   9.785  < 2e-16 ***
    ## unemp       -1.299e+00  4.941e-01  -2.630 0.008855 ** 
    ## pcincome     1.180e-03  3.360e-04   3.513 0.000490 ***
    ## poparea      1.884e-03  7.018e-04   2.684 0.007550 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.6 on 431 degrees of freedom
    ## Multiple R-squared:  0.3814, Adjusted R-squared:  0.3727 
    ## F-statistic: 44.28 on 6 and 431 DF,  p-value: < 2.2e-16

for model 7

``` r
old7 = lm(crm_1000 ~ pop + pop18 + beds + hsgrad + bagrad + poverty + pcincome + totalinc + poparea + region, data = cdi)
new7 = lm(crm_1000 ~ pop + pop18 + beds + hsgrad + bagrad + poverty + pcincome + totalinc + poparea + region, data = sum_cdi_df)
summary(old7); summary(new7)
```

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop + pop18 + beds + hsgrad + bagrad + 
    ##     poverty + pcincome + totalinc + poparea + region, data = cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -62.356 -10.092  -0.996   9.624  65.040 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -9.164e+01  2.378e+01  -3.853 0.000134 ***
    ## pop          8.049e-05  1.218e-05   6.609 1.15e-10 ***
    ## pop18        8.877e-01  2.732e-01   3.250 0.001246 ** 
    ## beds         2.376e+00  4.811e-01   4.939 1.13e-06 ***
    ## hsgrad       4.860e-01  2.522e-01   1.927 0.054628 .  
    ## bagrad      -6.182e-01  2.700e-01  -2.290 0.022534 *  
    ## poverty      2.065e+00  3.450e-01   5.984 4.60e-09 ***
    ## pcincome     2.717e-03  5.116e-04   5.310 1.77e-07 ***
    ## totalinc    -3.646e-03  5.864e-04  -6.218 1.20e-09 ***
    ## poparea      4.240e-03  4.414e-04   9.606  < 2e-16 ***
    ## region2      9.213e+00  2.544e+00   3.621 0.000328 ***
    ## region3      2.674e+01  2.416e+00  11.068  < 2e-16 ***
    ## region4      2.052e+01  2.939e+00   6.982 1.12e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.05 on 427 degrees of freedom
    ## Multiple R-squared:  0.6214, Adjusted R-squared:  0.6108 
    ## F-statistic: 58.41 on 12 and 427 DF,  p-value: < 2.2e-16

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop + pop18 + beds + hsgrad + bagrad + 
    ##     poverty + pcincome + totalinc + poparea + region, data = sum_cdi_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -43.289 -10.070  -0.575   8.997  56.301 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -9.030e+01  2.257e+01  -4.000 7.46e-05 ***
    ## pop          6.715e-05  1.168e-05   5.750 1.71e-08 ***
    ## pop18        1.055e+00  2.647e-01   3.984 7.96e-05 ***
    ## beds         2.889e+00  4.641e-01   6.224 1.16e-09 ***
    ## hsgrad       3.533e-01  2.396e-01   1.475  0.14106    
    ## bagrad      -5.603e-01  2.574e-01  -2.176  0.03007 *  
    ## poverty      2.044e+00  3.265e-01   6.258 9.51e-10 ***
    ## pcincome     2.829e-03  4.947e-04   5.719 2.02e-08 ***
    ## totalinc    -2.981e-03  5.628e-04  -5.297 1.89e-07 ***
    ## poparea      1.771e-03  6.004e-04   2.950  0.00336 ** 
    ## region2      1.033e+01  2.412e+00   4.283 2.28e-05 ***
    ## region3      2.776e+01  2.290e+00  12.122  < 2e-16 ***
    ## region4      2.211e+01  2.790e+00   7.924 2.04e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.13 on 425 degrees of freedom
    ## Multiple R-squared:  0.5869, Adjusted R-squared:  0.5753 
    ## F-statistic: 50.33 on 12 and 425 DF,  p-value: < 2.2e-16

## colinearility

``` r
cdi %>% 
  dplyr::select(-crm_1000) %>% 
  mutate(
    region = factor(region)) %>%
  GGally::ggcorr(label = TRUE, label_size = 2, hjust = 0.8)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## Warning in GGally::ggcorr(., label = TRUE, label_size = 2, hjust = 0.8): data in
    ## column(s) 'region' are not numeric and were ignored

![](model_diagnostics_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->
pop and totalinc(1) pcincome and bagrad(0.7) hsgrad and bagrad(0.7) beds
and docs(0.7)

``` r
check_collinearity(model2)
```

    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##      Term  VIF Increased SE Tolerance
    ##     pop18 1.93         1.39      0.52
    ##  pcincome 1.01         1.01      0.99
    ##    hsgrad 1.96         1.40      0.51
    ##     pop65 2.02         1.42      0.49
    ##   poverty 1.97         1.40      0.51
    ##      beds 1.52         1.23      0.66
    ##   poparea 1.01         1.00      0.99
    ##    region 1.51         1.23      0.66

``` r
check_collinearity(model3)
```

    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##      Term  VIF Increased SE Tolerance
    ##     pop18 1.02         1.01      0.98
    ##  pcincome 1.04         1.02      0.96
    ##   poverty 1.39         1.18      0.72
    ##      beds 1.32         1.15      0.76
    ##   poparea 1.03         1.02      0.97
    ##    region 1.24         1.11      0.81

``` r
check_collinearity(model4)
```

    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##             Term  VIF Increased SE Tolerance
    ##            pop18 1.06         1.03      0.95
    ##         pcincome 1.07         1.03      0.94
    ##          poverty 1.35         1.16      0.74
    ##             beds 1.39         1.18      0.72
    ##          poparea 1.32         1.15      0.76
    ##           region 4.73         2.18      0.21
    ##   poparea:region 4.01         2.00      0.25
    ##  poverty:poparea 1.00         1.00      1.00
    ##     beds:poparea 1.01         1.00      0.99

``` r
check_collinearity(model5)
```

    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##      Term  VIF Increased SE Tolerance
    ##     pop18 1.25         1.12      0.80
    ##      beds 1.27         1.13      0.79
    ##    hsgrad 1.89         1.38      0.53
    ##   poverty 1.68         1.30      0.60
    ##     unemp 1.54         1.24      0.65
    ##  pcincome 1.01         1.01      0.99
    ##   poparea 1.01         1.00      0.99

``` r
check_collinearity(model6)
```

    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##      Term  VIF Increased SE Tolerance
    ##     pop18 1.14         1.07      0.88
    ##      beds 1.27         1.13      0.79
    ##   poverty 1.45         1.20      0.69
    ##     unemp 1.35         1.16      0.74
    ##  pcincome 1.03         1.02      0.97
    ##   poparea 1.03         1.01      0.98

``` r
check_collinearity(model7)
```

    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##      Term  VIF Increased SE Tolerance
    ##       pop 1.00         1.00      1.00
    ##     pop18 1.94         1.39      0.52
    ##      beds 1.32         1.15      0.75
    ##    hsgrad 3.14         1.77      0.32
    ##    bagrad 3.35         1.83      0.30
    ##   poverty 2.16         1.47      0.46
    ##  pcincome 1.03         1.01      0.97
    ##  totalinc 1.00         1.00      1.00
    ##   poparea 1.01         1.00      0.99
    ##    region 1.63         1.28      0.61
