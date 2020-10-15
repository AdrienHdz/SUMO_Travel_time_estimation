# traveltimeCLT

## Installation

The package is still under development in the Alpha stage.

Install from [GitHub](https://github.com/AdrienHdz/SUMO_Travel_time_estimation/traveltimeCLT) with:

``` r
# install.packages("devtools")
devtools::install_github("AdrienHdz/SUMO_Travel_time_estimation/traveltimeCLT")
```

## Example

This package includes a small data set (`tripset`) that aggregates
map-matched anonymized mobile phone GPS data collected in Quebec city in
2014 using the Mon Trajet smartphone application developed by [Brisk
Synergies Inc](https://brisksynergies.com/). The precise duration of the
time period is kept confidential.

View the data with:

``` r
library(traveltimeCLT)
library(traveltimeHMM)
library(data.table)

data(trips)
head(trips)
#>   tripID linkID timeBin logspeed traveltime    length                time
#> 1   2700  10469 Weekday 1.692292  13.000000  70.61488 2014-04-28 03:07:27
#> 2   2700  10444 Weekday 2.221321  18.927792 174.50487 2014-04-28 03:07:41
#> 3   2700  10460 Weekday 2.203074   8.589937  77.76295 2014-04-28 03:07:58
#> 4   2700  10462 Weekday 1.924290  14.619859 100.15015 2014-04-28 03:08:07
#> 5   2700  10512 Weekday 1.804293   5.071986  30.81574 2014-04-28 03:08:21
#> 6   2700   5890 Weekday 2.376925  31.585355 340.22893 2014-04-28 03:08:26
```
Transforming the variables

``` r
trips$speed <- exp(trips$logspeed)
```
Splittig data into train and test sets.

``` r
test.trips <- create_test_trips(M = 500, trips, min.n = 1)
test = trips[trip %in% test.trips]
train = trips[!trip %in% test.trips]

```
Creating rules and time-bins

``` r
myrules = list(
  list(start='6:30', end= '9:00', days = 0:6, tag='MorningRush'),
  list(start='15:00', end= '18:00', days = 0:6, tag='EveningRush'))

mytimebins = c("MorningRush", "EveningRush", "Other")
```

Fit the model using the following code:

``` r
ttCLTmodel <- traveltimeCLT(data.train = train, 
			    M = 1000,
			    L = 2,
			    bin = "MorningRush",
			    rules = myrules,
			    data.timebins = mytimebins)
```
To predict on the test set and get estimations:

``` r
ttCLTresults <- predict_traveltimeCLT(obj.traveltime = ttCLTmodel,
				      data.test = test,
				      bin = "MorningRush",
				      rules = myrules)
```
ttCLTresults <- predict_traveltimeCLT(ttCLTmodel, test, "MorningRush", myrules)


## References

Woodard, D., Nogin, G., Koch, P., Racz, D., Goldszmidt, M., Horvitz, E.,
2017. “Predicting travel time reliability using mobile phone GPS data”.
*Transportation Research Part C*, 75, 30-44.
<http://dx.doi.org/10.1016/j.trc.2016.10.011>