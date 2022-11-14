# Extended Glmnet Package 

## Project Description 
This R package is my AMS 597, Statistical Computing, Final Project. The detailed description can be found in the *AMS597ProjectSpring2022.pdf*. 

extendedglmnetGroup18 Package is a extended version of the `glmnet` package that implements random lasso regression, a model created by Wang et.al (*ScientificPaper_ProjectSpring2022.pdf*). The `extended_glmnet` function in the package takes the following input. 

* `y` a (non-empty) numeric vector or factor with two levels with some missing value (NA).
* `X` a (non-empty) data frame or matrix where each column is a variable/predictor with some missing value (NA).For more information, see details section below.
* `model` a character string specifying the type of model to fit, must be one of "linear" (default), "ridge", "lasso", or "random_lasso".
* `prob` a numeric value of the proportion of data (0-1) to use for training, 0.7 (default).
* `len` a numeric value of the number of tuning parameter combinations for random lasso regression, 3 (default).For more information, see details section below.
* `nfold` a numeric value of the number of folds for cross validation for random lasso regression, 5 (default).For more information, see Details section below.




```
* Linear Regression/ Logistic regression
* Ridge Regression
* Lasso Regression
* Random Lasso Regression
```

The function  


## How to use the package


### Executing Program 
