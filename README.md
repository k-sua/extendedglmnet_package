# Extended Glmnet Package 

## Project Description 
extendedglmnetGroup18 Package is my AMS 597, Statistical Computing, Final Project. It is a extended version of the `glmnet` package that implements random lasso regression, a model created by Wang et.al (*ScientificPaper_ProjectSpring2022.pdf*). The `extended_glmnet` function of the package takes as an input a response variable $y$ (binary or continuous) and a set of candidate predictors/independent variables $X$. Then, it splits the data into training and testing data, and fits the training data to the model that the user specified. Then it calculates, and visualizes the model's prediction performance on the test data. The model options are given below.

* Linear Regression/ Logistic regression
* Ridge Regression
* Lasso Regression
* Random Lasso Regression


## How to use the package
First, read docstrings from the `extended_glm.R` file and *vignettes* file to see detailed instruction on how and when to use the `extended_glmnet` function. Download the `extendedglmnetGroup18_0.1.0.tar.gz` file and import the extendedglmnetGroup18 package and glmnet package(>= 4.1-2). 


### Executing Program 
* Download R version (>=4.1.0)
* Imports: glmnet (>= 4.1-2)

## Directories 

```
├── extendedglmnetGroup18
    ├── R
        ├── extended_glm.R
    ├── vignettes
        ├── extendedglmnetGroup18.R
        ├── extendedglmnetGroup18.Rmd
        ├── extendedglmnetGroup18.html
```
