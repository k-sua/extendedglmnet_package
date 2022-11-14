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

├── app
│   ├── css
│   │   ├── **/*.css
│   ├── favicon.ico
│   ├── images
│   ├── index.html
│   ├── js
│   │   ├── **/*.js
│   └── partials/template
├── dist (or build)
├── node_modules
├── bower_components (if using bower)
├── test
├── Gruntfile.js/gulpfile.js
├── README.md
├── package.json
├── bower.json (if using bower)
└── .gitignore

## How to use the package


### Executing Program 
