
# Flusurvey contacts model

To set up copy `contacts.rds` into `data/` and run:

``` r
renv::restore() ## install required packages
```

Run the baseline model (intercept only):

``` r
source("R/model.R")
```

To run any other models, set `opts()`, e.g.

``` r
opts <- list(
  type = "physical",
  model = "individual"
)
source("R/model.R")
```
