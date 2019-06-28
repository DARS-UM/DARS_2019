Pillar 1 - Student Topic Profile
================
DARS
2019-06-28

-   [Helper Functions](#helper-functions)
-   [Save](#save)

Helper Functions
================

``` r
collapse_or       <- function(string) string %>% str_c(collapse = "|")
collapse_space    <- function(string) string %>% str_c(collapse = " ")
```

``` r
find_df <- function(course)  d_student_model %>% filter(`Course ID` == course)
```

``` r
my_cv.glmnet <- function(df, alpha = 1, predictors){
  
  df <- df %>% select(Grade, matches(predictors))
  
  y <- df %>% select(Grade             ) %>% as.matrix
  x <- df %>% select(matches(predictors)) %>% as.matrix
  
  nfold <- min(10, floor(nrow(df) / 3))
  
  cv.glmnet(x, y, nfolds = nfold, type.measure = "mae", alpha = alpha)
  
}
```

Save
====

``` r
save(list = ls(), file = "Output/useful functions.RDATA")
```
