Selection Report Draft
================
Jennifer Levy
May 24, 2017

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

### Calculate RMSE based on weighted mean using days.

------------------------------------------------------------------------

``` r
# Calculate RMSE average. This averages the RMSE for each year of the LOOCV analysis for each day and model
rmse_avg<-aggregate(x = x$rmse_weighted_by_product, by = list(x$model_id,x$doy), FUN=mean)  
colnames(rmse_avg)<-c("model_id","doy","rmse_avg")

# Calculate the the weighted rmse error for each model
wt<-c(.1,.35,.1,.35,.05,.05)

# Order days from lowest to hightest so weights are applied appropriately
rmse_avg<-rmse_avg[order(rmse_avg$model_id,rmse_avg$doy),]

# Calcualte weighted mean for each model
Results<-
        rmse_avg %>%
        group_by(model_id) %>%
        summarise(
                model_RMSE= weighted.mean(rmse_avg,wt)
        )
```

### Look at the Results file. Note that a check on the weighted mean calculation was performed and the code worked.

    ## # A tibble: 6 × 2
    ##   model_id model_RMSE
    ##      <int>      <dbl>
    ## 1        1   17.29015
    ## 2        2   15.56832
    ## 3        3   17.62566
    ## 4        4   24.08215
    ## 5        5   12.90771
    ## 6        6   14.98433

### Merge model RMSE values to the dataset containing model settings. Rank the models according to lowest RMSE value. Look at the data file to see the structure.

    ##      model_id     corn_id_model         ml_models
    ## 972       972 [xgb, '4', '100'] [xgb, '4', '200']
    ## 916       916 [xgb, '4', '100'] [xgb, '4', '200']
    ## 1408     1408 [xgb, '4', '100'] [xgb, '4', '200']
    ## 1896     1896 [xgb, '4', '100'] [xgb, '8', '200']
    ## 1360     1360 [xgb, '4', '100'] [xgb, '4', '100']
    ## 1097     1097 [xgb, '4', '100'] [xgb, '4', '200']
    ##                                                            dataset
    ## 972  [evi, evi_delta, evi_std, lstd, lstn, _year, yield_average]\n
    ## 916  [evi, evi_delta, evi_std, lstd, lstn, _year, yield_average]\n
    ## 1408             [evi, evi_delta, evi_std, _year, yield_average]\n
    ## 1896             [evi, evi_delta, evi_std, _year, yield_average]\n
    ## 1360 [evi, evi_delta, evi_std, lstd, lstn, _year, yield_average]\n
    ## 1097       [evi, evi_delta, evi_std, lstd, _year, yield_average]\n
    ##      crop_threshold model_RMSE rank
    ## 972             220   12.35160    1
    ## 916             240   12.43483    2
    ## 1408            220   12.43862    3
    ## 1896            220   12.49363    4
    ## 1360            220   12.54658    5
    ## 1097            220   12.57986    6

### Look at the range of model RMSE values.

    ## [1] 12.35160 24.41769

### Use the top 5% of models based on RMSE to narrow down model selection. Models producing values to the left of the red line will be considered for the best model. Model settings that are not found in the top 5% will be omitted from further model testing.

![](https://github.com/JLevy2/BW/blob/master/unnamed-chunk-8-1.png)

### Create a new dataset containing only the top 5% of models.Identify how many unique datasets are in this group.

Varibales that vary are

-   dataset
-   machine learning algorithm and settings
-   crop mask algorithm
-   crop mask threshold settings

### Theoretically the biggest driver of model performance should be based on the input dataset rather than variation in model settings. Therefore, for each dataset, select the corn\_id\_model, machine learning settings, and crop\_threshold combination that that produces the lowest RMSE.

### Here are the settings and percent of occurance in the top 5% of models.

    ##   Crop threshold Frequency Percent occurance
    ## 2            220        37             38.54
    ## 3            240        32             33.33
    ## 1             50        20             20.83
    ## 4            260         7              7.29

    ##      Corn algorithm Frequency Percent occurance
    ## 5 [xgb, '4', '100']        78             81.25
    ## 1       [rf, '160']        14             14.58
    ## 2        [rf, '80']         4              4.17
    ## 3 [xgb, '1', '200']         0              0.00
    ## 4 [xgb, '2', '100']         0              0.00
    ## 6 [xgb, '8', '200']         0              0.00

    ##         ML settings Frequency Percent occurance
    ## 7 [xgb, '4', '200']        32             33.33
    ## 8 [xgb, '8', '200']        26             27.08
    ## 6 [xgb, '4', '100']        13             13.54
    ## 3       [rf, '200']         9              9.38
    ## 1       [rf, '100']         8              8.33
    ## 2       [rf, '160']         8              8.33
    ## 4 [xgb, '1', '100']         0              0.00
    ## 5 [xgb, '1', '200']         0              0.00

    ##                                                                            Dataset
    ## 1        [evi, evi_average, evi_average_for_doys, evi_std, _year, yield_average]\n
    ## 6                                [evi, evi_delta, evi_std, _year, yield_average]\n
    ## 8                    [evi, evi_delta, evi_std, lstd, lstn, _year, yield_average]\n
    ## 7                          [evi, evi_delta, evi_std, lstd, _year, yield_average]\n
    ## 2  [evi, evi_average, evi_average_for_doys, evi_std, lstd, _year, yield_average]\n
    ## 5                        [evi, evi_average, evi_std, _year, yield_average, lstd]\n
    ## 3                        [evi, evi_average, evi_average_for_doys, evi_std, lstd]\n
    ## 4                              [evi, evi_average, evi_average_for_doys, evi_std]\n
    ## 9                                                    [evi, evi_std, evi_average]\n
    ## 10                                               [evi, evi_std, lstd, evi_delta]\n
    ##    Frequency Percent occurance
    ## 1         22             22.92
    ## 6         20             20.83
    ## 8         20             20.83
    ## 7         19             19.79
    ## 2         11             11.46
    ## 5          4              4.17
    ## 3          0              0.00
    ## 4          0              0.00
    ## 9          0              0.00
    ## 10         0              0.00

### Create a shortlist of models to examine based on the best combination of model settings for each dataset

    ##      model_id     corn_id_model         ml_models
    ## 972       972 [xgb, '4', '100'] [xgb, '4', '200']
    ## 1408     1408 [xgb, '4', '100'] [xgb, '4', '200']
    ## 1097     1097 [xgb, '4', '100'] [xgb, '4', '200']
    ## 951       951 [xgb, '4', '100'] [xgb, '8', '200']
    ## 439       439 [xgb, '4', '100'] [xgb, '8', '200']
    ## 1480     1480 [xgb, '4', '100'] [xgb, '8', '200']
    ##                                                                              dataset
    ## 972                    [evi, evi_delta, evi_std, lstd, lstn, _year, yield_average]\n
    ## 1408                               [evi, evi_delta, evi_std, _year, yield_average]\n
    ## 1097                         [evi, evi_delta, evi_std, lstd, _year, yield_average]\n
    ## 951        [evi, evi_average, evi_average_for_doys, evi_std, _year, yield_average]\n
    ## 439  [evi, evi_average, evi_average_for_doys, evi_std, lstd, _year, yield_average]\n
    ## 1480                       [evi, evi_average, evi_std, _year, yield_average, lstd]\n
    ##      crop_threshold model_RMSE rank
    ## 972             220   12.35160    1
    ## 1408            220   12.43862    3
    ## 1097            220   12.57986    6
    ## 951             220   12.59888    8
    ## 439             240   12.79988   26
    ## 1480            240   13.25827   67

### Notice that all of the model settings are consistant with the settings that appear most often in the top 5% . Since the most frequent settings in the top 5% of models produce the best RMSE for each dataset, this proivdes some confidence for the model settings under considerations based on consistent performance relative to the other options.

### To start, the residuals from the top 4 models (containing ranks 1,3,6 and 8) will be considered.

    ##      model_id     corn_id_model         ml_models
    ## 972       972 [xgb, '4', '100'] [xgb, '4', '200']
    ## 1408     1408 [xgb, '4', '100'] [xgb, '4', '200']
    ## 1097     1097 [xgb, '4', '100'] [xgb, '4', '200']
    ## 951       951 [xgb, '4', '100'] [xgb, '8', '200']
    ##                                                                        dataset
    ## 972              [evi, evi_delta, evi_std, lstd, lstn, _year, yield_average]\n
    ## 1408                         [evi, evi_delta, evi_std, _year, yield_average]\n
    ## 1097                   [evi, evi_delta, evi_std, lstd, _year, yield_average]\n
    ## 951  [evi, evi_average, evi_average_for_doys, evi_std, _year, yield_average]\n
    ##      crop_threshold model_RMSE rank
    ## 972             220   12.35160    1
    ## 1408            220   12.43862    3
    ## 1097            220   12.57986    6
    ## 951             220   12.59888    8

### Results for all four models look similar so I will just show the results for the top ranking model.

### First, Check predicted production vs observed production for each day. The results look good.

![](https://github.com/JLevy2/BW/blob/master/unnamed-chunk-14-1.png)![](https://github.com/JLevy2/BW/blob/master/unnamed-chunk-14-2.png)![](https://github.com/JLevy2/BW/blob/master/unnamed-chunk-14-3.png)

### Check histogram distribution for each day. The residuals are centered around 0 but there are some outliers beyond the 2-4 standard deviation expected. The red lines show the maximum andminimum residual values.

![](https://github.com/JLevy2/BW/blob/master/unnamed-chunk-15-1.png)![](https://github.com/JLevy2/BW/blob/master/unnamed-chunk-15-2.png)![](https://github.com/JLevy2/BW/blob/master/unnamed-chunk-15-3.png)

### Q-Q plots for each day

![](https://github.com/JLevy2/BW/blob/master/unnamed-chunk-16-1.png)
