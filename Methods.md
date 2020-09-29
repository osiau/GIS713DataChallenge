
# Analysis methods

*Have places that voted for Trump in 2016 had better COVID-19 outcomes than those that voted for Clinton?*

#### Is there truth to Trump's statement? 

We began our exploration by visualizing 2016 general election votes and COVID-19 data provided by our professor, Dr. Josh Gray. Boxplots and scatterplots were used to gauge, at a very surface level, if there is discrepancy in COVID-19 cases and deaths per capita among regions of the U.S. in which Trump or Clinton won the vote.

![boxplot](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/statelvlcovid_sumplots.png)
*State-Level Boxplots comparing COVID-19 outcomes and polical affiliation.*

Though the differences are marginal, these plots suggest that blue counties and states may be faring worse than red counties and states. However, correlation does not imply causation; COVID-19 poses a complex issue, and the visualizations only tell part of the whole story. 

![boxplot](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/cntylvlcovid_sumplots.png)
*County-Level Boxplots comparing COVID-19 outcomes and polical affiliation.*

The impacts of COVID-19 span much farther than those who have been infected; millions of people in the United States are feeling the adverse economic impacts of the disease. Corrplots and regression models were used to analyze associations between health, demographic, environmental, and social factors and COVID-19. Unemployment rates in each county and state were also assessed to quantify the negative economic impacts of the virus, given the unique social, demographic, spatial, and environmental factors in each county and state.

#### What other factors could be impacting COVID-19 statistics? 

We hypothesized several other factors that may be affecting COVID-19 cases and deaths in a given location: government interventions as a response to COVID-19, hospital capacities, community resilience, commercial flight arrivals, air quality, and demographic statistics. 

After acquiring [these data](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/data) from several reliable sources, corrplots were developed to compare the relative correlations that the factors have with COVID-19 cases and mortalities. Linear, quasi-binomial, binary, and quadratic regression models were conducted and optimized to explore the relationships that these “predictors” may have on COVID-19 cases and deaths per capita. These models, despite being significant, suggested weak relationships between the predictor variables and COVID-19 cases, deaths, and unemployment per capita. 

Correlations between these three outcomes (unemployment, death rate and positive cases) and voting data may be attributed to other covariates.  For example, states that voted for Trump tended to be more rural than those that voted for Hillary. Therefore, we collected a dataset of covariates that might cause this pattern. They included:
* Air Quality Index
* Percent of population at high, medium and low risk (from Community Resilience Data)
* Fraction of the population that is white, black, asian, pacific islander, or other
* Fraction of the population over 65
* Fraction of the population that is insured
* Fraction residents who depend on public transport

To analyze this, we examined a few approaches: Regression Analyses, Random Forest, and Spatial Autocorrelation.

![boxplot](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/countylevelcorr.png)
*County-Level Correlellograms between possible COVID-19 outcome covariates.*

![boxplot](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/statelevelcorr.png)
*State-Level Correlellograms between possible COVID-19 outcome covariates.*

##### Find our analysis, data and methods on github: 
![github](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/octocat.svg) [GitHub](https://github.ncsu.edu/chaedri/Data-Challenge-GIS713)
