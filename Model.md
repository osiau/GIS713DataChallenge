# Modeling approaches

#### Correlation And Causality
Correlations between these three outcomes (unemployment, death rate and positive cases) and voting data may be attributed to other covariates.  For example, states that voted for Trump tended to be more rural than those that voted for Hillary. Therefore, we collected a dataset of covariates that might cause this pattern. They included:
* Air Quality Index
* Percent of population at high, medium and low risk (from Community Resilience Data)
* Fraction of the population that is white, black, asian, pacific islander, or other
* Fraction of the population over 65
* Fraction of the population that is insured
* Fraction residents who depend on public transport

Pairwise linear regression model outcomes yielded significant correlations (but not causations) between outcome variables (confirmed cases, deaths, and unemployment), and predictor variables. Preliminary findings from these models were used to inform the final random forest model which predictor variables may be important in predicting outcome variables. We can visualize these correlations with state and county level correlelograms.

![boxplot](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/countylevelcorr.png)
*County-Level Correlellograms between possible COVID-19 outcome covariates.*

![boxplot](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/statelevelcorr.png)
*State-Level Correlellograms between possible COVID-19 outcome covariates.*

### Making decisions with Random Forest

Random Forest Regression was used to identify features that contribute most to COVID-19 cases, deaths, and changes in unemployment rates at the county scale.  All predictors and response variables were normalized per capita. Random Forest is a robust ensemble model that is able to handle large dimensions of data. Initial analyses included Moran's I spatial autocorrelation; spatial autocorrelation was significant in all three cases (COVID-19 cases, deaths, and changes in unemployment). The model was trained using a 70/30 training/testing split, and validated with the full data set. The coefficients of determination (CODs) were 0.64,0.66, and 0.67, respectively. Median absolute error (MAE) values of  0.26, 0.26, and 0.31 suggest high levels of error. However, all three models accounted for spatial relationships as follows:
* Counts: significant relationship to not significant relationship with p-value of almost 0 to 0.63
* Deaths: significant relationship to borderline significant relationship (depends on level of significance chosen) with p-value of almost 0 to 0.046. A better model should account for more of the spatial variance, but a spatial model might be needed.  Regardless, the 0.05 significance value is highly debated due to false discovery rate and many corrections lower this to 0.01.
* Unemployment: significant relationship to borderline significant relationship (depends on level of significance chosen) with p-value of almost 0 to 0.18

![random](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/randomforest.PNG)

Limitations:
* The test and validation errors were high, possibly due to the use of Median Absolute Error opposed to Mean Squared Error.
* The test COD was low, but the validation COD was strong
* Temporal effect was not considered
* Interpreting node purity is not straightforward, but does show republican and democrat vote percentages have the same impact measure on the models

Visit our Conclusions page to understand the broader significance of these explorations and how they relate to President Trump's statement.


##### Find our analysis, data and methods on github: 
![github](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/octocat.svg) [GitHub](https://github.ncsu.edu/chaedri/Data-Challenge-GIS713)
s
