# Excess mortality rate, using Serfling regression 

*Step 1*: fit all data to a cyclic regression model and calculate upper bound of 95%CI, values above upr are considered as epidemic period and excluded from data set.

*Step 2*: fit new data set (with NAs) to another cyclic regression model and calculate 95% upr. Values above this limit are considered as excess mortality.

*Step 3*: Where excess mortality > 0, and time period falls into the intervals between Oct of a year to March (influenza season) of the following year is our interest. Total excess mortality over an interval are summed together.

*Step 4*: Calculate denominator (person-time), sum values corresponding to interval in Step 3 (condition: where excess mortality > 0 & epi==1 & ((1<= nperiod <=3) | (10 <= nperiod <=12)))

*Step 5*: Calculate excess mortality rate (per 100,000 persons-year), divide obtained result in Step 3 by Step 4

*Step 6*: Visualize result by age-specific (young- 60&70; old- 80&90) and edu-specific


#### Regression models considered:
+ Traditional approach of Serfling regression is using linear regression.
+ Here Poisson and Negative Binomial Regression are also considered.
