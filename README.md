# Extended Glmnet Package 

## Project Description 
This R package is my AMS 597, Statistical Computing, Final Project. The detailed description can be found in the *AMS597ProjectSpring2022.pdf*. 

extendedglmnetGroup18 Package is a extended version of the `glmnet` package that implements random lasso regression, a model created by Wang et.al (*ScientificPaper_ProjectSpring2022.pdf*). The `extended_glmnet` takes as an input a response variable $y$ (binary or continuous) and a set of candidate predictors/independent variables $X$. Then, it fits the model the user specified, calculates, and visualizes the model performance on the test data or the entire input data, depending on the user input on the size of test data. For some models, the number of predictors can be larger than the sample size but for others, the function will throw an error if so. The model options are given below.


```
* Linear Regression/ Logistic regression
* Ridge Regression
* Lasso Regression
* Random Lasso Regression
```

.
+-- _config.yml
+-- _drafts
|   +-- begin-with-the-crazy-ideas.textile
|   +-- on-simplicity-in-technology.markdown
+-- _includes
|   +-- footer.html
|   +-- header.html
+-- _layouts
|   +-- default.html
|   +-- post.html
+-- _posts
|   +-- 2007-10-29-why-every-programmer-should-play-nethack.textile
|   +-- 2009-04-26-barcamp-boston-4-roundup.textile
+-- _data
|   +-- members.yml
+-- _site
+-- index.html 

## How to use the package


### Executing Program 
