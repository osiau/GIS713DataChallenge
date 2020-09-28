
# Have places that voted for Trump in 2016 had better COVID-19 outcomes than those that voted for Clinton?

Yes. We found that blue states has worse health outcomes than red states.

A brief description of our findings; the twitter map and one sentence conclusion
twitter-worthy map; Caption by Muyiwa
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


##### Legislative Map
Caption by Uchenna
{% include map/index.html %}

#### Random Forest
We tried several different autocorrelation models and found Random Forest worked best.



Conclusion: yes we are sure!

##### Find our analysis, data and methods on github: 
![github](https://pages.github.ncsu.edu/chaedri/Data-Challenge-GIS713/images/octocat.svg) [GitHub](https://github.ncsu.edu/chaedri/Data-Challenge-GIS713)
