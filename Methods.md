
# Methods

*"Did parts of the nation that voted for President Trump in 2016 have better outcomes regarding COVID-19 than areas that voted for Hillary Clinton?”*

## Is there truth to Trump's statement? 

We began our exploration by visualizing 2016 general election votes and covid data provided by our professor, Dr. Josh Gray. Boxplots and scatterplots were used to gauge, at a very surface level, if there is discrepancy in COVID-19 cases and deaths per capita among regions of the U.S. in which Trump or Clinton won the vote.

Though the differences are marginal, these plots suggest that blue counties and states may be faring worse than red counties and states. However, correlation does not imply causation; COVID-19 poses a complex issue, and the visualizations only tell part of the whole story. 

The impacts of COVID-19 span much farther than those who have been infected; millions of people in the United States are feeling the adverse economic impacts of the disease. Corrplots and regression models were used to analyze associations between health, demographic, environmental, and social factors and COVID-19. Unemployment rates in each county and state were also assessed to quantify the negative economic impacts of the virus, given the unique social, demographic, spatial, and environmental factors in each county and state.

## What other factors could be impacting COVID-19 statistics? 

We hypothesized several other factors that may be affecting COVID-19 cases and deaths in a given location: government interventions as a response to COVID-19, hospital capacities, community resilience, commercial flight arrivals, air quality, and demographic statistics. 

After acquiring these data from several reliable sources, corrplots were developed to compare the relative correlations that the factors have with COVID-19 cases and mortalities. Linear, quasi-binomial, binary, and quadratic regression models were conducted and optimized to explore the relationships that these “predictors” may have on COVID-19 cases and deaths per capita. These models, despite being significant, suggested weak relationships between the predictor variables and COVID-19 cases, deaths, and unemployment per capita. 

## Making decisions with Random Forest

Random Forest Regression was used to identify features that contribute most to COVID-19 cases and deaths at the county scale. Random Forest is an ensemble model that is robust and able to handle large dimensions of data. Initial analyses included Moran’s I spatial autocorrelation analysis. Spatial autocorrelation was found to be significant in predicting cases, deaths, and unemployment rates. The model was trained using a 70/30 split between training and testing data, and validated with the full data set.  


##### Find our analysis, data and methods on github: 
![github](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/octocat.svg) [GitHub](https://github.ncsu.edu/chaedri/Data-Challenge-GIS713)

#### Data References
Citation for all of our data
