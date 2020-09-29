
# Have places that voted for Trump in 2016 had better COVID-19 outcomes than those that voted for Clinton?

No. We found that blue states did not have worse health outcomes than red states.

A brief description of our findings; the twitter map and one sentence conclusion
twitter-worthy map; Caption by Muyiwa

![centroid_gif](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/covid_percapita_gif.gif)

![centroid](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/centroids.png)


### Is this difference even statistically significant?
Basic box plots; not sure significant...
![boxplot](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/boxplot.png)

#### But could that be correlation, not causality?
These are all the covariates we considered. Along with some images of these datasets (non-exhaustive)

![covar](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/covar.png)

##### Unemployment
Caption by Uchenna
{% include streamgraphBasic.html %}



##### Map of Results of Voting by County
![pic](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/Votes_county.png)


#### Random Forest
We tried several different autocorrelation models and found Random Forest worked best.
![random](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/randomforest.PNG)


Conclusion: yes we are sure!

##### Find our analysis, data and methods on github: 
![github](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/octocat.svg) [GitHub](https://github.ncsu.edu/chaedri/Data-Challenge-GIS713)
