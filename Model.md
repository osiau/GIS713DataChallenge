# Modeling approaches

### Finding the best regression fit

Regression model outcomes yielded significant correlations (but not causations) between outcome variables (confirmed cases, deaths, and unemployment), and predictor variables. The predictor variables initially considered in the state- and county-level models include: 

#### State-level: 
Proportion of total votes earned by Hillary, number of hospital beds, number of hospitals, number of arriving commercial flights, number of governmental measures in response to COVID-19, median age, median income, and the proportions of the population that are white, black, American Indian, Asian, Pacific Islander, “other” races, under 18, over 65, and insured.

#### County-level: 
Proportion of total votes earned by Hillary, max AQI, median AQI, proportions of the population that are low, medium, and high risk, number of hospitals, number of hospital beds, median age, median income, and the proportions of the population that are white, black, American Indian, Asian, Pacific Islander, “other” races, under 18, over 65, insured, and reliant on public transport. 

Insignificant predictors and those with multicollinearity were removed from the initial linear models, and AIC tests were used to identify the best sets of predictors for each response variable. Transformations were applied to normalize the distributions of response variables, and to predictor variables. Quas-binomial, beta, and quadratic regressions were also explored to determine the optimal model for each combination of predictors and spatial scales. The models with the best fits are as follows:

![residuals_vs_fitted](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/resid_vs_fitted.png)

*The multilinear model with the best regression fit uses the percents of a given state's population who are black, under 18, over 65, and insured, respectively, to predict per-capita COVID-19 cases by state. % insured is the only predictor that was shown to have a negative relationship with COVID-19 cases.*

#### State-Level Models: 
* **Cases per capita** = 0.0037 log(% black population) + 0.1783 (% population under 18) + 0.0492 sqrt(% population over 65) - 0.1650 log(% insured) - 0.0327; **Adjusted R-squared: 0.5972, p-value: 1.877e-09**
* Log(**deaths per capita**) = 5.046 sqrt(% “other races” in population) - 8.572; **Adjusted R-squared: 0.1236, p-value: 0.006601**
* Log(**unemployment**) = 1.904e-03 (hospitals per state) - 3.539e-03 (number of government measures in response to COVID-19) - 3.379e-05 (median income) + 5.893e-02 log(% black population) + 9.058 (% Pacific Islander population) - 8.006 (% “other races” in population) + 14.02 log(% insured) + 13.26 (% dependent on public transport) + 3.545; **Adjusted R-squared: 0.3985, p-value: 0.03579**

#### County-Level Models: 
* Log(**cases per capita**) = -4.538e+00 + 1.344e-01 (number of government measures in response to COVID-19) + 1.166e-02 (hospitals per state) -1.314e-05 (median income) -1.761e+01  (% Pacific Islander population) + 1.821e+00 (% “other races” in population) +  9.886e+00 (% population under 18) -1.762e+00 (% insured) + 3.554e+00 (% dependent on public transport); **Multiple R-squared:  0.2663,	Adjusted R-squared:  0.2604 , p-value: < 2.2e-16** 
* Log(**deaths per capita**) = 1.253e-04 + 1.875e-04 (number of government measures in response to COVID-19) -2.087e-07 (max AQ index) -1.287e-08 (median income) -5.600e-04 (% Asian population) -6.308e-03 (% Pacific Islander population) + 3.060e-03 (% population under 18) + 8.037e-03 (% dependent on public transport); **Multiple R-squared:  0.2774,	Adjusted R-squared:  0.2723, p-value: < 2.2e-16**                   
* Log(**unemployment**) = -3.531e+00 + 8.356e-05 (median income) + 4.863e+00 (% Asian population) + 1.111e+02 (% Pacific Islander population) -1.255e+01 (% “other races” in population) +  6.662e+00 (% population under 18) + 4.222e+00 (% insured) -9.136e+00 (% dependent on public transport); **Multiple R-squared:  0.2107,	Adjusted R-squared:  0.2052, p-value: < 2.2e-16**

Preliminary findings from these models were used to inform the final random forest model which predictor variables may be important in predicting outcome variables. We can visualize these correlations with state and county level correlelograms.

### Making decisions with Random Forest

Random Forest Regression was used to identify features that contribute most to COVID-19 cases, deaths, and changes in unemployment rates at the county scale.  All predictors and response variables were normalized per capita. Random Forest is a robust ensemble model that is able to handle large dimensions of data. Initial analyses included Moran's I spatial autocorrelation; spatial autocorrelation was significant in all three cases (COVID-19 cases, deaths, and changes in unemployment). The model was trained using a 70/30 training/testing split, and validated with the full data set. The coefficients of determination (CODs) were < 0.10 which means the model did not account for much variability. Median absolute error (MAE) values of 0.26, 0.26, and 0.31 suggest high levels of error. However, all three models accounted for spatial relationships as follows: 

* Counts: significant relationship to not significant relationship with p-value of almost 0 to 0.63
* Deaths: significant relationship to borderline significant relationship (depends on level of significance chosen) with p-value of almost 0 to 0.046. A better model should account for more of the spatial variance, but a spatial model might be needed. Regardless, the 0.05 significance value is highly debated due to false discovery rate and many corrections lower this to 0.01.
* Unemployment: significant relationship to borderline significant relationship (depends on level of significance chosen) with p-value of almost 0 to 0.18.

The features were finally selected using outside of bag errors.

![random](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/randomforest.png)

### Limitations
* The test and validation errors were high,
* the coefficient of determination was low,
* temporal effects wer not considered,
* interpreting node purity is not straightforward

Visit [our conclusions](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/the-end) to understand the broader significance of these explorations and how they relate to President Trump's statement.


##### Find our analysis, data and methods on github: 
![github](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/octocat.svg) [GitHub](https://github.ncsu.edu/chaedri/Data-Challenge-GIS713)
