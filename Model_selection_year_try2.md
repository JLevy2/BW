Model Selectin v0.2
================

------------------------------------------------------------------------

### Load data files and install libraries for data processing. Look at the dataset. Note that quality checks were performed on the file. All model/day combinations are present in this file.

------------------------------------------------------------------------

    ##   task_id model_id job_id doy year yield_predicted yield us_error     area
    ## 1   27572        1  57000 201 2004          162.48 160.3     2.18 73631000
    ## 2   27572        1  57000 201 2005          160.10 147.9    12.20 75117000
    ## 3   27572        1  57000 201 2006          159.85 149.1    10.75 70638000
    ## 4   27572        1  57000 201 2007          160.75 150.7    10.05 86520000
    ## 5   27572        1  57000 201 2008          159.80 153.3     6.50 78570000
    ## 6   27572        1  57000 201 2009          161.34 164.4    -3.06 79490000
    ##    rmse rmse_weighted_by_area rmse_weighted_by_product      mae
    ## 1 17.23              15.45871                 15.36390 13.81063
    ## 2 17.37              15.63807                 15.40105 13.45584
    ## 3 16.78              11.31882                 10.69352 11.84839
    ## 4 22.40              16.25795                 15.71576 17.18918
    ## 5 20.65              16.84056                 16.51763 15.82404
    ## 6 19.08              16.64344                 16.06246 15.30492
    ##   mae_weighted_by_area mae_weighted_by_product
    ## 1            13.028737               12.973604
    ## 2            12.361433               12.191406
    ## 3             8.261863                7.908787
    ## 4            12.871790               12.513247
    ## 5            13.666342               13.504645
    ## 6            13.249679               12.838014

------------------------------------------------------------------------

### Calculate RMSE for each model-year combination based on weighted days.

------------------------------------------------------------------------

``` r
# Calculate the the weighted rmse error for each model and year
wt<-c(.1,.35,.1,.35,.05,.05)

# Order days from lowest to hightest so weights are applied appropriately
order<-x[order(x$model_id,x$year,x$doy),]
order<-order[,c(2,4:5,12)]

# Calcualte weighted mean for each model-year combination
Results<-
        order %>%
        group_by(model_id,year) %>%
        summarise(
                model_RMSE= weighted.mean(rmse_weighted_by_product,wt)
        )
```

### Look at the Results file. Note that a check on the weighted mean calculation was performed and the code worked.

    ## Source: local data frame [15 x 3]
    ## Groups: model_id [2]
    ## 
    ##    model_id  year model_RMSE
    ##       <int> <int>      <dbl>
    ## 1         1  2004   14.90476
    ## 2         1  2005   15.22471
    ## 3         1  2006   10.98453
    ## 4         1  2007   15.28425
    ## 5         1  2008   16.86689
    ## 6         1  2009   16.14002
    ## 7         1  2010   13.69126
    ## 8         1  2011   13.65506
    ## 9         1  2012   41.41121
    ## 10        1  2013   12.73188
    ## 11        1  2014   18.38758
    ## 12        1  2015   16.06843
    ## 13        1  2016   19.42135
    ## 14        2  2004   13.22285
    ## 15        2  2005   12.66983

### Merge model RMSE values to the dataset containing model settings. Rank the models according to lowest RMSE value within each yaer. Look at the data file to see the structure. Not that a check confirmed that there are 13 instances with rank of 1. This is one rank per year.

    ##       model_id     corn_id_model         ml_models
    ## 12624      972 [xgb, '4', '100'] [xgb, '4', '200']
    ## 9140       704 [xgb, '4', '100'] [xgb, '4', '200']
    ## 11896      916 [xgb, '4', '100'] [xgb, '4', '200']
    ## 17278     1330 [xgb, '4', '100'] [xgb, '8', '200']
    ## 11636      896 [xgb, '4', '100'] [xgb, '4', '200']
    ## 24103     1855 [xgb, '4', '100'] [xgb, '4', '200']
    ##                                                             dataset
    ## 12624 [evi, evi_delta, evi_std, lstd, lstn, _year, yield_average]\n
    ## 9140  [evi, evi_delta, evi_std, lstd, lstn, _year, yield_average]\n
    ## 11896 [evi, evi_delta, evi_std, lstd, lstn, _year, yield_average]\n
    ## 17278 [evi, evi_delta, evi_std, lstd, lstn, _year, yield_average]\n
    ## 11636 [evi, evi_delta, evi_std, lstd, lstn, _year, yield_average]\n
    ## 24103 [evi, evi_delta, evi_std, lstd, lstn, _year, yield_average]\n
    ##       crop_threshold year model_RMSE rank
    ## 12624            220 2004   8.853447    1
    ## 9140              50 2004   8.954107    2
    ## 11896            240 2004   8.972934    3
    ## 17278             75 2004   9.036532    4
    ## 11636            260 2004   9.081445    5
    ## 24103             75 2004   9.084789    6

### Look at the range of model RMSE values for each year.2012 has the largest variation in model performance and 2010 has the smallest variation in model performance.Interestingly, 2012-2016 timeframe has the largest variability in RMSE.

    ##    year  min_RMSE max_RMSE delta_RMSE
    ## 1  2004  8.853447 20.58426  11.730817
    ## 2  2005 10.364830 22.05273  11.687902
    ## 3  2006  8.162080 24.62581  16.463726
    ## 4  2007  9.427091 22.83311  13.406024
    ## 5  2008 10.126317 25.30370  15.177382
    ## 6  2009  8.120115 22.38734  14.267225
    ## 7  2010  9.389797 18.86864   9.478846
    ## 8  2011  9.965247 24.87715  14.911903
    ## 9  2012 15.719976 47.66504  31.945061
    ## 10 2013 10.505482 31.22186  20.716381
    ## 11 2014 12.269260 29.75573  17.486470
    ## 12 2015 10.456845 27.51158  17.054739
    ## 13 2016 12.001237 33.84744  21.846206

### Use the top 5% of models in each year, based on RMSE, to narrow down model selection. To the left of the red lines shows the top 5% of models in each year.

![](Model_selection_year_try2_files/figure-markdown_github/unnamed-chunk-7-1.png)![](Model_selection_year_try2_files/figure-markdown_github/unnamed-chunk-7-2.png)![](Model_selection_year_try2_files/figure-markdown_github/unnamed-chunk-7-3.png)![](Model_selection_year_try2_files/figure-markdown_github/unnamed-chunk-7-4.png)

### Check to see if any of the same models make it into the top 5% for each year

    ##     Model_top5_Percent Frequency Percent occurance
    ## 85                 299         8              0.64
    ## 296                972         8              0.64
    ## 307                997         8              0.64
    ## 416               1360         8              0.64
    ## 460               1502         8              0.64
    ## 551               1773         8              0.64
    ## 133                439         7              0.56
    ## 178                614         7              0.56
    ## 210                704         7              0.56
    ## 220                739         7              0.56

### For each model-year combination, how does the RMSE value compare to the top performing model? Model ID's are labeled on the right.This first set of graphs is for models that made the top 5% for 8 years. The patterns for models that made the top 5% for 7 yrs and 6 yrs were evaluted. OVerall, the models all perform poorly in 2012. Because results were similar, only models occuring 8/13 yrs will be shown.

![](Model_selection_year_try2_files/figure-markdown_github/unnamed-chunk-9-1.png)

### Look at the top 10 models that performed well in 2012. 2012 was a drought year.They perform poorly in 2016. 2014-2016 were high yield years.

![](Model_selection_year_try2_files/figure-markdown_github/unnamed-chunk-10-1.png)

### Find 2016 delta RMSE for all models that performed well in 2012. The top models in this group will be looked at for further consideration.

![](Model_selection_year_try2_files/figure-markdown_github/unnamed-chunk-13-1.png)

### look at 5 models in top 5% for 2012 with the lowest RMSE for 2016.

![](Model_selection_year_try2_files/figure-markdown_github/unnamed-chunk-14-1.png)

### Compare model performance for the top 20 models in the top 5% for 2012, with the lowest RMSE for 2016. Look at how many years each model has the best and worst RMSE in the selected group. Model 1775 is never the worst and is the best in the group for 4 of the 13 yrs.

    ##    model_id Worst Performance Best performance
    ## 1       572                 2               NA
    ## 2      1063                 4               NA
    ## 3      1235                 1                2
    ## 4      1283                 2               NA
    ## 5      1607                 4               NA
    ## 6      1797                 1                1
    ## 7         6                NA                1
    ## 8       160                NA                2
    ## 9       272                NA                1
    ## 10     1177                NA                2
    ## 11     1334                NA                1
    ## 12     1775                NA                4

### Model 1775 is the compromise for a model that does realtively well predicting the drought year and not too bad predicting other years, including the high yield years of 2014-2016. An alternative option is a model with the profile of 972 that performs well in most years except 2012. Looking at the profiles side by side, the magnitude of the error in the worst year is in the same ballpark but model 972 does better in all other years. Model 972 was the top ranking model when the years were averaged. The county level residuals for 972 can be found at the bottom of

<https://github.com/JLevy2/BW/blob/master/Model_selection_test.md>

### This might come down to which is more important to get right, the high yield years or the low yield years.

![](Model_selection_year_try2_files/figure-markdown_github/unnamed-chunk-16-1.png)

How close do these two models come to matching the actual national harvest? I do not have this information yet.

What is the mean residual? This is the mean residual for all county-level residuals from 2004-2016, for all days, for model 972. The residuals do not cancel eachother out to produce a 0 overall residual.

    ## [1] -178611

This is the mean residual for all county-level residuals for all days for each year (model 972).

    ##    year mean_residuals
    ## 1  2004      114553.36
    ## 2  2005     -331348.71
    ## 3  2006     -107640.27
    ## 4  2007      -89387.97
    ## 5  2008      180697.28
    ## 6  2009      130787.94
    ## 7  2010     -195089.62
    ## 8  2011     -359968.59
    ## 9  2012    -1520693.28
    ## 10 2013     -499970.21
    ## 11 2014      421597.79
    ## 12 2015     -145052.86
    ## 13 2016      340492.27

This is the mean residual for all county-level residuals from 2004-2016, for all days, for model 1775. The residuals do not cancel eachother out to produce a 0 overall residual.

    ## [1] 142282.9

This is the mean residual for all county-level residuals for all days for each year (model 1775).

    ##    year mean_residuals
    ## 1  2004      -275209.3
    ## 2  2005      -674503.7
    ## 3  2006      -342562.3
    ## 4  2007      -190171.9
    ## 5  2008      -103046.0
    ## 6  2009      -196940.8
    ## 7  2010      -378872.4
    ## 8  2011      -398816.4
    ## 9  2012       195041.8
    ## 10 2013       580106.8
    ## 11 2014       929153.8
    ## 12 2015       639226.7
    ## 13 2016      1356797.6
