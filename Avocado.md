Avocado Data Statistics
================
Deondre Brown
2024-03-30

This is a project documenting my statistical steps and analysis
regarding a dataset for Avocado sales.

Step 1.Read in the data

``` r
avocado <- read.csv("Module2_Avocado_Data.csv")
```

Step 2. Load in the necessary packages we need

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.3.3

    ## Warning: package 'ggplot2' was built under R version 4.3.3

    ## Warning: package 'tibble' was built under R version 4.3.3

    ## Warning: package 'tidyr' was built under R version 4.3.3

    ## Warning: package 'readr' was built under R version 4.3.3

    ## Warning: package 'purrr' was built under R version 4.3.3

    ## Warning: package 'dplyr' was built under R version 4.3.3

    ## Warning: package 'stringr' was built under R version 4.3.3

    ## Warning: package 'forcats' was built under R version 4.3.3

    ## Warning: package 'lubridate' was built under R version 4.3.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.3.3

    ## 
    ## Attaching package: 'psych'
    ## 
    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

Step 3. Use these functions to make the sure data was loaded in
properly.

``` r
dim(avocado) # Shows the numbers of rows and columns in the dataset
```

    ## [1] 17490     9

``` r
names(avocado) # Names all the variables in the dataset
```

    ## [1] "observationid" "region"        "year"          "month"        
    ## [5] "quarter"       "type"          "averageprice"  "totalvolume"  
    ## [9] "supplierid"

``` r
view(avocado) # Displays the dataset in a spreadsheet format in another tab
```

``` r
head(avocado, 10) # Prints the first 10 rows of the data
```

    ##    observationid               region year month quarter    type averageprice
    ## 1          17038 Miami/Ft. Lauderdale 2016    10       4 Organic         1.58
    ## 2           6381         Jacksonville 2018    11       4 Organic         1.65
    ## 3           6392              Orlando 2018    11       4 Organic         1.39
    ## 4          16826 Miami/Ft. Lauderdale 2016    10       4 Organic         1.49
    ## 5          10471   New Orleans/Mobile 2017     6       2 Organic         2.19
    ## 6          16985 Miami/Ft. Lauderdale 2016    10       4 Organic         1.58
    ## 7          15109                Boise 2016     2       1 Organic         1.67
    ## 8          17019    Buffalo/Rochester 2016    10       4 Organic         1.63
    ## 9          15162                Boise 2016     2       1 Organic         1.84
    ## 10         10153   New Orleans/Mobile 2017     4       2 Organic         1.95
    ##    totalvolume supplierid
    ## 1       385.55          A
    ## 2       404.62          C
    ## 3       405.29          C
    ## 4       472.82          A
    ## 5       515.01          C
    ## 6       542.85          A
    ## 7       562.64          A
    ## 8       563.06          A
    ## 9       566.57          A
    ## 10      634.09          C

``` r
str(avocado) # Get an overview of the variables and their first rows of values
```

    ## 'data.frame':    17490 obs. of  9 variables:
    ##  $ observationid: int  17038 6381 6392 16826 10471 16985 15109 17019 15162 10153 ...
    ##  $ region       : chr  "Miami/Ft. Lauderdale" "Jacksonville" "Orlando" "Miami/Ft. Lauderdale" ...
    ##  $ year         : int  2016 2018 2018 2016 2017 2016 2016 2016 2016 2017 ...
    ##  $ month        : int  10 11 11 10 6 10 2 10 2 4 ...
    ##  $ quarter      : int  4 4 4 4 2 4 1 4 1 2 ...
    ##  $ type         : chr  "Organic" "Organic" "Organic" "Organic" ...
    ##  $ averageprice : num  1.58 1.65 1.39 1.49 2.19 1.58 1.67 1.63 1.84 1.95 ...
    ##  $ totalvolume  : num  386 405 405 473 515 ...
    ##  $ supplierid   : chr  "A" "C" "C" "A" ...

Step 4. Create a new data column to hold dollar sales information. To
get the dollar sales information we are going to multiple the average
price by the total volume.

``` r
avocado$totalsales <- avocado$averageprice*avocado$totalvolume
```

Step 5. Look at basic summaries of the data.

``` r
summary(avocado) #R built-in function for summary statistics
```

    ##  observationid      region               year          month       quarter     
    ##  Min.   :    1   Length:17490       Min.   :2016   Min.   : 1   Min.   :1.000  
    ##  1st Qu.: 4373   Class :character   1st Qu.:2016   1st Qu.: 3   1st Qu.:1.000  
    ##  Median : 8746   Mode  :character   Median :2017   Median : 6   Median :2.000  
    ##  Mean   : 8746                      Mean   :2017   Mean   : 6   Mean   :2.357  
    ##  3rd Qu.:13118                      3rd Qu.:2018   3rd Qu.: 9   3rd Qu.:3.000  
    ##  Max.   :17490                      Max.   :2019   Max.   :12   Max.   :4.000  
    ##      type            averageprice    totalvolume        supplierid       
    ##  Length:17490       Min.   :0.440   Min.   :     386   Length:17490      
    ##  Class :character   1st Qu.:1.120   1st Qu.:   14415   Class :character  
    ##  Mode  :character   Median :1.360   Median :  120285   Mode  :character  
    ##                     Mean   :1.393   Mean   :  600147                     
    ##                     3rd Qu.:1.620   3rd Qu.:  449951                     
    ##                     Max.   :3.250   Max.   :25394903                     
    ##    totalsales      
    ##  Min.   :     563  
    ##  1st Qu.:   23080  
    ##  Median :  160817  
    ##  Mean   :  665447  
    ##  3rd Qu.:  546286  
    ##  Max.   :40361458

``` r
describe(avocado) #Better than summary() function and gives more statistics
```

    ##               vars     n      mean         sd    median   trimmed       mad
    ## observationid    1 17490   8745.50    5049.07   8745.50   8745.50   6482.67
    ## region*          2 17490     27.00      15.30     27.00     27.00     19.27
    ## year             3 17490   2017.13       0.94   2017.00   2017.07      1.48
    ## month            4 17490      6.00       3.46      6.00      5.93      4.45
    ## quarter          5 17490      2.36       1.13      2.00      2.32      1.48
    ## type*            6 17490      1.50       0.50      1.50      1.50      0.74
    ## averageprice     7 17490      1.39       0.39      1.36      1.37      0.37
    ## totalvolume      8 17490 600147.13 1385429.67 120285.18 238594.63 167772.12
    ## supplierid*      9 17490      2.50       0.86      3.00      2.62      0.00
    ## totalsales      10 17490 665447.32 1519115.25 160817.47 277170.69 220030.78
    ##                   min         max       range  skew kurtosis       se
    ## observationid    1.00    17490.00    17489.00  0.00    -1.20    38.18
    ## region*          1.00       53.00       52.00  0.00    -1.20     0.12
    ## year          2016.00     2019.00        3.00  0.27    -1.00     0.01
    ## month            1.00       12.00       11.00  0.12    -1.24     0.03
    ## quarter          1.00        4.00        3.00  0.15    -1.37     0.01
    ## type*            1.00        2.00        1.00  0.00    -2.00     0.00
    ## averageprice     0.44        3.25        2.81  0.66     0.72     0.00
    ## totalvolume    385.55 25394902.82 25394517.27  4.42    32.59 10475.86
    ## supplierid*      1.00        3.00        2.00 -1.15    -0.68     0.01
    ## totalsales     563.35 40361458.14 40360894.79  6.55   110.62 11486.71

``` r
avocado |>
  select(totalvolume, totalsales) |>
  describe() # Describes statistics for the total volume and total sales variables
```

    ##             vars     n     mean      sd   median  trimmed      mad    min
    ## totalvolume    1 17490 600147.1 1385430 120285.2 238594.6 167772.1 385.55
    ## totalsales     2 17490 665447.3 1519115 160817.5 277170.7 220030.8 563.35
    ##                  max    range skew kurtosis       se
    ## totalvolume 25394903 25394517 4.42    32.59 10475.86
    ## totalsales  40361458 40360895 6.55   110.62 11486.71

**Using these statistics we can find out the minimum, maximum, and
average sales**

Minimum: **563.35** Maximum: **40,361,458** and Average: **665,447.3**

Step 6. Look at a data distribution using a histogram. For this
histogram we can display the frequency regarding the average price of
avocados.

``` r
hist(avocado$averageprice, 
     col = "red",
     xlab = "Average Price",
     ylab = "Frequency")
```

![](Avocado_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

**Using this histogram, we can make the observation that the data is
positively skewed**

Step 7. We can find the correlation value between average price and
sales volume to see how they relate to each other.

``` r
cor(avocado$averageprice,avocado$totalvolume)
```

    ## [1] -0.3158095

**We get a value of -0.31 indicating that the average price and sales
volume are inversely related**

Step 8. We can generate many more insights by gathering summary
statistics by grouping.

**Get the Summary statistics for price by each supplier**

``` r
describeBy(averageprice ~ supplierid, mat=T, data=avocado) #Mat function transposes the data
```

    ##               item group1 vars     n     mean        sd median  trimmed
    ## averageprice1    1      A    1  4335 1.441232 0.3769172  1.410 1.415630
    ## averageprice2    2      B    1   128 1.166172 0.2572726  1.155 1.161154
    ## averageprice3    3      C    1 13027 1.379806 0.3941439  1.350 1.355955
    ##                    mad  min  max range      skew   kurtosis          se
    ## averageprice1 0.340998 0.56 3.25  2.69 0.6910307 0.75940546 0.005724682
    ## averageprice2 0.266868 0.54 1.88  1.34 0.2340183 0.01419964 0.022739896
    ## averageprice3 0.385476 0.44 3.17  2.73 0.6643521 0.71053120 0.003453287

**Average price of avocados by month?**

``` r
describeBy(averageprice ~ month, mat=T, data=avocado)
```

    ##                item group1 vars    n     mean        sd median  trimmed
    ## averageprice1     1      1    1 2014 1.291326 0.3384133   1.27 1.276427
    ## averageprice2     2      2    1 1697 1.242068 0.3618871   1.23 1.227483
    ## averageprice3     3      3    1 1697 1.293447 0.3488425   1.26 1.272811
    ## averageprice4     4      4    1 1484 1.355330 0.3818613   1.33 1.331069
    ## averageprice5     5      5    1 1378 1.327286 0.3984560   1.31 1.300453
    ## averageprice6     6      6    1 1270 1.380315 0.3888111   1.32 1.348760
    ## averageprice7     7      7    1 1590 1.434811 0.3670059   1.39 1.406965
    ## averageprice8     8      8    1 1272 1.509835 0.3995163   1.46 1.477967
    ## averageprice9     9      9    1 1378 1.574753 0.4082758   1.53 1.548886
    ## averageprice10   10     10    1 1484 1.567796 0.4043240   1.53 1.539781
    ## averageprice11   11     11    1 1272 1.476627 0.3626503   1.45 1.457800
    ## averageprice12   12     12    1  954 1.360776 0.3539983   1.34 1.344005
    ##                     mad  min  max range      skew    kurtosis          se
    ## averageprice1  0.355824 0.51 2.70  2.19 0.4685901  0.21133836 0.007540805
    ## averageprice2  0.385476 0.46 2.59  2.13 0.4075879 -0.06878999 0.008784807
    ## averageprice3  0.326172 0.44 3.05  2.61 0.7839616  1.55643913 0.008468150
    ## averageprice4  0.385476 0.50 3.17  2.67 0.6794018  0.67565825 0.009912625
    ## averageprice5  0.400302 0.51 2.73  2.22 0.6334883  0.30997839 0.010733857
    ## averageprice6  0.370650 0.57 2.77  2.20 0.7766223  0.54271731 0.010910302
    ## averageprice7  0.340998 0.64 2.67  2.03 0.6783879  0.28750489 0.009203955
    ## averageprice8  0.370650 0.58 3.04  2.46 0.7854832  0.69762176 0.011201881
    ## averageprice9  0.385476 0.64 2.97  2.33 0.6721711  0.58891815 0.010998389
    ## averageprice10 0.370650 0.67 3.25  2.58 0.7515656  0.88834407 0.010495728
    ## averageprice11 0.326172 0.60 3.12  2.52 0.6348279  0.97607152 0.010168208
    ## averageprice12 0.355824 0.56 2.71  2.15 0.5523340  0.51250772 0.011461119
