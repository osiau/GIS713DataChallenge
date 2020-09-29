# Modeling approaches

### Finding the best regression fit

Regression model outcomes yielded significant correlations (but not causations) between outcome variables (confirmed cases, deaths, and unemployment), and predictor variables. The best models for each combination of predictors and spatial scales are: 

State-Level Models: 
* **Cases per capita** = log(% black population) + (% population under 18) + sqrt(% population over 65) - log(% insured); **Adjusted R-squared: 0.5972, p-value: 1.877e-09**
* Log(deaths per capita) = sqrt(% “other races” in population); **Adjusted R-squared: 0.1236, p-value: 0.006601**
* Log(unemployment) = (hospitals per state) - (number of government measures in response to COVID-19) - (median income) + log(% black population) + (% Pacific Islander population) - (% “other races” in population) + log(% insured) + (% dependent on public transport); **Adjusted R-squared: 0.3985, p-value: 0.03579**

County-Level Models: 
* Log(cases per capita) = (number of government measures in response to COVID-19) + (hospitals per state) + (median income) +  (% Pacific Islander population) + (% “other races” in population) +  (% population under 18) + log(% insured) + (% dependent on public transport); **Multiple R-squared:  0.3184,	Adjusted R-squared:  0.3074** 
* Log(deaths per capita) = (number of government measures in response to COVID-19) + (max AQ index) + (median income) + (% Asian population) + (% Pacific Islander population) + (% “other races” in population) +  (% population under 18) + log(% insured) + (% dependent on public transport); **Multiple R-squared:  0.2774,	Adjusted R-squared:  0.2723**                   
* Log(unemployment) = (median income) + (% Asian population) + (% Pacific Islander population) + (% “other races” in population) +  (% population under 18) + log(% insured) + (% dependent on public transport); **Multiple R-squared:  0.2107,	Adjusted R-squared:  0.2052**

Preliminary findings from these models were used to inform the final random forest model which predictor variables may be important in predicting outcome variables. We can visualize these correlations with state and county level correlelograms.

![boxplot](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/countylevelcorr.png)
*County-Level Correlellograms between possible COVID-19 outcome covariates.*

![boxplot](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/statelevelcorr.png)
*State-Level Correlellograms between possible COVID-19 outcome covariates.*

### Making decisions with Random Forest

Random Forest Regression was used to identify features that contribute most to COVID-19 cases, deaths, and changes in unemployment rates at the county scale.  All predictors and response variables were normalized per capita. Random Forest is a robust ensemble model that is able to handle large dimensions of data. Initial analyses included Moran's I spatial autocorrelation; spatial autocorrelation was significant in all three cases (COVID-19 cases, deaths, and changes in unemployment). The model was trained using a 70/30 training/testing split, and validated with the full data set. The coefficients of determination (CODs) were 0.64,0.66, and 0.67, respectively. Median absolute error (MAE) values of  0.26, 0.26, and 0.31 suggest high levels of error. However, all three models accounted for spatial relationships as follows:
* Counts: significant relationship to not significant relationship with p-value of almost 0 to 0.63
* Deaths: significant relationship to borderline significant relationship (depends on level of significance chosen) with p-value of almost 0 to 0.046. A better model should account for more of the spatial variance, but a spatial model might be needed.  Regardless, the 0.05 significance value is highly debated due to false discovery rate and many corrections lower this to 0.01.
* Unemployment: significant relationship to borderline significant relationship (depends on level of significance chosen) with p-value of almost 0 to 0.18

![random](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/randomforest.png)

### Limitations
* The test and validation errors were high, possibly due to the use of Median Absolute Error opposed to Mean Squared Error.
* The test COD was low, but the validation COD was strong
* Temporal effect was not considered
* Interpreting node purity is not straightforward, but does show republican and democrat vote percentages have the same impact measure on the models

Visit our Conclusions page to understand the broader significance of these explorations and how they relate to President Trump's statement.


##### Find our analysis, data and methods on github: 
![github](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/octocat.svg) [GitHub](https://github.ncsu.edu/chaedri/Data-Challenge-GIS713)
s
