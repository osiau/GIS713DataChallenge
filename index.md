
# Have places that voted for Trump in 2016 had better COVID-19 outcomes than those that voted for Clinton?

No. We found that blue states did not have worse outcomes than red states.

We considered three outcome metrics for COVID-19: deaths per capita, cases per capita and unemployment. 

For each of these three outcome metrics, we compare

A brief description of our findings; the twitter map and one sentence conclusion
twitter-worthy map; Caption by Muyiwa.

![centroid](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/covid_percapita_gif.gif)


### Preliminary Analysis
To better visualize differences between Red and Blue areas, we can look at box and scatter plots of their outcomes.
We did this at both the state and county level.
![boxplot](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/cntylvlcovid_sumplots.png)
*County-Level Boxplots comparing COVID-19 outcomes and polical affiliation.*

![boxplot](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/statelvlcovid_sumplots.png)
*State-Level Boxplots comparing COVID-19 outcomes and polical affiliation.*

#### Correlation And Causality:
Correlations between these three outcomes (unemployment, death rate and positive cases) and voting data may be attributed to other covariates.  For example, states that voted for Trump tended to be more rural than those that voted for Hilary. Therefore, we collected a dataset of covariates that might cause this pattern They included
* Air Quality Index
* Percent of population at high, medium and low risk (CITATION)
* 
* Fraction of the population that is white, black, asian, pacific islander, or other
* Fraction of the population over 65
* Fraction of the population that is insured
* Fraction pubtransport <- WHAT DOES THIS MEAN?


A pairwise linear regression model outcomes yielded significant correlations (but not causations) between outcome variables (confirmed cases, deaths, and unemployment), and predictor variables. Preliminary findings from these models were used to inform the final random forest model which predictor variables may be important in predicting outcome variables. We can visualize these correlations with state and county level correlelograms.

![boxplot](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/countylevelcorr.png)
*County-Level Correlellograms between possible COVID-19 outcome covariates.*

![boxplot](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/statelevelcorr.png)
*State-Level Correlellograms between possible COVID-19 outcome covariates.*

#### Covariate Data

##### Unemployment
Caption by Uchenna
{% include streamgraphBasic.html %}

##### Map of Results of Voting by County
![picture](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/Votes_county.png)


#### Random Forest
We tried several different autocorrelation models and found Random Forest worked best.

None of the models were high effective but they all identified that political party doesn’t effect Covid counts or deaths.  The random forest error was high and the cod was very low on the test data.  The modeled validation data looked good but 70% of it was used for training.

Also, that is a piece I forgot to include in the notes -> the model was trained on 70% data and tested with 30% and then predictions were made on all of the data (validation). 
![random](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/randomforest.PNG)


##### Find our analysis, data and methods on github: 
![github](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/octocat.svg) [GitHub](https://github.ncsu.edu/chaedri/Data-Challenge-GIS713)
