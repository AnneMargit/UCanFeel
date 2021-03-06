UCanFeel Descriptives
================
Anne Margit
5/12/2021

``` r
load("data_ucanfeel.Rdata")
```

``` r
library(dplyr)
library(skimr)
library(tidyverse)
library(stringr)
library(papaja)
library(reshape2)   
library(ggplot2)
```

Aantal participanten per school

``` r
data_ucanfeel$school <- as.factor(data_ucanfeel$school)

levels(data_ucanfeel$school)
```

    ## [1] "CapellenCampus"                 "GreijdanusCollege"             
    ## [3] "MeanderCollege"                 "Mens&"                         
    ## [5] "Thomas├íKempisCollege"           "ThorbeckeScholengemeenschapMHA"
    ## [7] "Zone.college"

``` r
pp_school <- data_ucanfeel %>%
  group_by(school) %>%
  summarise(n_id = n_unique(id))
```

``` r
apa_table(pp_school, caption="Aantal participanten per school")
```

<caption>

(\#tab:unnamed-chunk-4)

</caption>

<div data-custom-style="Table Caption">

*Aantal participanten per school*

</div>

| school                         | n\_id |
| :----------------------------- | :---- |
| CapellenCampus                 | 29    |
| GreijdanusCollege              | 19    |
| MeanderCollege                 | 17    |
| Mens&                          | 5     |
| Thomas├íKempisCollege           | 26    |
| ThorbeckeScholengemeenschapMHA | 29    |
| Zone.college                   | 30    |
| NA                             | 4     |

Aantal participanten per schooljaar

``` r
pp_schooljaar <- data_ucanfeel %>%
  group_by(schooljaar) %>%
  summarise(n_id = n_unique(id))
```

``` r
apa_table(pp_schooljaar, caption="Aantal participanten per schooljaar")
```

<caption>

(\#tab:unnamed-chunk-6)

</caption>

<div data-custom-style="Table Caption">

*Aantal participanten per schooljaar*

</div>

| schooljaar | n\_id |
| :--------- | :---- |
| 2          | 23    |
| 3          | 85    |
| 4          | 25    |
| 5          | 25    |
| NA         | 1     |

En per school per schooljaar

``` r
pp_school_schooljaar <- data_ucanfeel %>%
  group_by(school, schooljaar) %>%
  summarise(n_id = n_unique(id))
```

``` r
apa_table(pp_school_schooljaar, caption="Aantal participanten per schooljaar per school")
```

<caption>

(\#tab:unnamed-chunk-8)

</caption>

<div data-custom-style="Table Caption">

*Aantal participanten per schooljaar per school*

</div>

| school                         | schooljaar | n\_id |
| :----------------------------- | :--------- | :---- |
| CapellenCampus                 | 3          | 11    |
| CapellenCampus                 | 4          | 8     |
| CapellenCampus                 | 5          | 10    |
| GreijdanusCollege              | 2          | 1     |
| GreijdanusCollege              | 3          | 13    |
| GreijdanusCollege              | 4          | 3     |
| GreijdanusCollege              | 5          | 2     |
| MeanderCollege                 | 3          | 7     |
| MeanderCollege                 | 4          | 7     |
| MeanderCollege                 | 5          | 3     |
| Mens&                          | 2          | 5     |
| Thomas├íKempisCollege           | 2          | 3     |
| Thomas├íKempisCollege           | 3          | 11    |
| Thomas├íKempisCollege           | 4          | 5     |
| Thomas├íKempisCollege           | 5          | 7     |
| ThorbeckeScholengemeenschapMHA | 3          | 25    |
| ThorbeckeScholengemeenschapMHA | 4          | 2     |
| ThorbeckeScholengemeenschapMHA | 5          | 2     |
| Zone.college                   | 2          | 13    |
| Zone.college                   | 3          | 17    |
| NA                             | 2          | 1     |
| NA                             | 3          | 1     |
| NA                             | 5          | 1     |
| NA                             | NA         | 1     |

Gemiddelden en SDs schoolfactoren (tevredenheid etc) per school

``` r
schoolfactoren <- data_ucanfeel %>%
  group_by(school) %>%
  summarise(across(TR:AE_Stress, .fns=list(Mean = mean, SD = sd), na.rm=TRUE,
                   .names="{col}_{fn}"))

schoolfactoren <- schoolfactoren %>%
  mutate(across(2:11, round, 2))
```

TR = Teacher relationships SC = School connectedness AS = Academic
support OD = Order and discipline ASF = Academic satisfaction

``` r
apa_table(schoolfactoren, caption="Schoolfactoren (M, SD) per school")
```

<caption>

(\#tab:unnamed-chunk-10)

</caption>

<div data-custom-style="Table Caption">

*Schoolfactoren (M, SD) per
school*

</div>

| school                         | TR\_Mean | TR\_SD | SC\_Mean | SC\_SD | AS\_Mean | AS\_SD | OD\_Mean | OD\_SD | ASF\_Mean | ASF\_SD | AE\_Stress\_Mean | AE\_Stress\_SD |
| :----------------------------- | :------- | :----- | :------- | :----- | :------- | :----- | :------- | :----- | :-------- | :------ | :--------------- | :------------- |
| CapellenCampus                 | 61.88    | 19.32  | 45.28    | 17.72  | 68.56    | 16.35  | 70.51    | 14.28  | 44.89     | 25.93   | 58.48            | 17.53          |
| GreijdanusCollege              | 63.56    | 18.01  | 45.47    | 20.32  | 68.54    | 9.50   | 71.80    | 14.49  | 53.09     | 23.97   | 52.95            | 16.99          |
| MeanderCollege                 | 68.06    | 13.97  | 48.12    | 23.11  | 75.00    | 12.77  | 71.98    | 20.57  | 52.00     | 25.27   | 46.22            | 27.11          |
| Mens&                          | 63.40    | 27.94  | 61.00    | 33.10  | 70.73    | 17.06  | 71.46    | 17.20  | 79.80     | 35.66   | 53.77            | 27.58          |
| Thomas├íKempisCollege           | 65.95    | 15.21  | 45.18    | 21.92  | 71.43    | 9.71   | 71.87    | 13.50  | 47.56     | 21.97   | 47.87            | 27.98          |
| ThorbeckeScholengemeenschapMHA | 61.89    | 20.46  | 52.74    | 21.46  | 72.31    | 15.11  | 74.68    | 15.67  | 45.50     | 31.99   | 48.70            | 27.29          |
| Zone.college                   | 64.37    | 18.29  | 45.02    | 22.60  | 68.71    | 15.51  | 67.51    | 17.06  | 43.55     | 29.09   | 46.29            | 28.79          |
| NA                             | 70.17    | 18.32  | 67.75    | 9.47   | 70.00    | 18.55  | 74.50    | 8.25   | 73.00     | 11.75   | 44.78            | 39.83          |

Gemiddelden en SDs schoolfactoren (tevredenheid etc) per schooljaar

``` r
schoolfactoren_jaar <- data_ucanfeel %>%
  group_by(schooljaar) %>%
  summarise(across(TR:AE_Stress, .fns=list(Mean = mean, SD = sd), na.rm=TRUE,
                   .names="{col}_{fn}"))

schoolfactoren_jaar <- schoolfactoren_jaar %>%
  mutate(across(2:11, round, 2))
```

TR = Teacher relationships SC = School connectedness AS = Academic
support OD = Order and discipline ASF = Academic
satisfaction

``` r
apa_table(schoolfactoren_jaar, caption="Schoolfactoren (M, SD) per schooljaar")
```

<caption>

(\#tab:unnamed-chunk-12)

</caption>

<div data-custom-style="Table Caption">

*Schoolfactoren (M, SD) per
schooljaar*

</div>

| schooljaar | TR\_Mean | TR\_SD | SC\_Mean | SC\_SD | AS\_Mean | AS\_SD | OD\_Mean | OD\_SD | ASF\_Mean | ASF\_SD | AE\_Stress\_Mean | AE\_Stress\_SD |
| :--------- | :------- | :----- | :------- | :----- | :------- | :----- | :------- | :----- | :-------- | :------ | :--------------- | :------------- |
| 2          | 69.79    | 21.53  | 53.05    | 25.07  | 72.02    | 18.40  | 73.26    | 16.50  | 60.64     | 33.97   | 44.82            | 28.35          |
| 3          | 61.82    | 17.56  | 44.15    | 21.30  | 69.52    | 13.76  | 70.26    | 15.89  | 43.83     | 26.21   | 45.80            | 25.65          |
| 4          | 66.44    | 17.77  | 53.82    | 20.13  | 70.67    | 12.34  | 72.56    | 16.11  | 48.92     | 23.21   | 61.81            | 21.63          |
| 5          | 63.33    | 15.83  | 49.25    | 19.18  | 71.44    | 10.93  | 71.43    | 13.95  | 52.25     | 26.93   | 58.69            | 18.47          |
| NA         | 84.11    | NA     | 61.00    | NA     | 93.33    | NA     | 80.43    | NA     | 79.00     | NA      | 1.50             | NA             |

Gemiddelden en SDs angst and depressie en life satisfaction per
schooljaar

``` r
angst_dep_lifesat <- data_ucanfeel %>%
  group_by(schooljaar) %>%
  summarise(across(lifesat:rcads_dep, .fns=list(Mean = mean, SD = sd), na.rm=TRUE,
                   .names="{col}_{fn}"))

angst_dep_lifesat <- angst_dep_lifesat %>%
  mutate(across(2:7, round, 2))
```

``` r
apa_table(angst_dep_lifesat, caption="Angst, depressie, life satisfaction per schooljaar")
```

<caption>

(\#tab:unnamed-chunk-14)

</caption>

<div data-custom-style="Table Caption">

*Angst, depressie, life satisfaction per
schooljaar*

</div>

| schooljaar | lifesat\_Mean | lifesat\_SD | rcads\_anx\_Mean | rcads\_anx\_SD | rcads\_dep\_Mean | rcads\_dep\_SD |
| :--------- | :------------ | :---------- | :--------------- | :------------- | :--------------- | :------------- |
| 2          | 62.46         | 25.80       | 8.39             | 5.54           | 9.22             | 4.94           |
| 3          | 66.37         | 19.34       | 7.92             | 5.88           | 7.57             | 5.88           |
| 4          | 60.41         | 21.51       | 10.08            | 6.18           | 8.56             | 5.37           |
| 5          | 58.76         | 21.82       | 8.17             | 5.98           | 9.96             | 4.21           |
| NA         | 91.86         | NA          | 3.00             | NA             | 3.00             | NA             |

Jongens vs meisjes (jongen = 0, meisje = 1, anders = 2)

``` r
angst_dep_geslacht <- data_ucanfeel %>%
  group_by(geslacht) %>%
  summarise(across(lifesat:rcads_dep, .fns=list(Mean = mean, SD = sd), na.rm=TRUE,
                   .names="{col}_{fn}"))

angst_dep_geslacht <- angst_dep_geslacht %>%
  mutate(across(2:7, round, 2))
```

``` r
apa_table(angst_dep_geslacht, caption="Angst, depressie, life satisfaction per geslacht")
```

<caption>

(\#tab:unnamed-chunk-16)

</caption>

<div data-custom-style="Table Caption">

*Angst, depressie, life satisfaction per
geslacht*

</div>

| geslacht | lifesat\_Mean | lifesat\_SD | rcads\_anx\_Mean | rcads\_anx\_SD | rcads\_dep\_Mean | rcads\_dep\_SD |
| :------- | :------------ | :---------- | :--------------- | :------------- | :--------------- | :------------- |
| 0        | 67.52         | 19.61       | 5.40             | 4.57           | 6.47             | 4.52           |
| 1        | 62.78         | 21.52       | 9.58             | 5.97           | 9.06             | 5.69           |
| 2        | 33.21         | 15.86       | 11.00            | 7.07           | 11.50            | 4.95           |

``` r
data_ucanfeel %>% skim("geslacht", "leeftijd", "schooljaar", "eenhuis", "niveau_nu",
                       "TR", "SC", "AS", "OD", "ASF", "AE_Stress",
                       "happy", "rcads_anx", "rcads_dep", "distress_precorona", "distress_nu",
                       "vrienden_klas", "vrienden_school", "pest_freq")
```

|                                                  |            |
| :----------------------------------------------- | :--------- |
| Name                                             | Piped data |
| Number of rows                                   | 159        |
| Number of columns                                | 198        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| factor                                           | 4          |
| numeric                                          | 15         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type:
factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                       |
| :------------- | ---------: | -------------: | :------ | --------: | :-------------------------------- |
| geslacht       |          0 |           1.00 | FALSE   |         3 | 1: 108, 0: 49, 2: 2               |
| schooljaar     |          1 |           0.99 | FALSE   |         4 | 3: 85, 4: 25, 5: 25, 2: 23        |
| eenhuis        |          0 |           1.00 | FALSE   |         2 | 1: 133, 2: 26                     |
| niveau\_nu     |          0 |           1.00 | FALSE   |         6 | hav: 65, vwo: 52, vmb: 23, vmb: 8 |

**Variable type:
numeric**

| skim\_variable      | n\_missing | complete\_rate |  mean |    sd |    p0 |   p25 |   p50 |   p75 |   p100 | hist  |
| :------------------ | ---------: | -------------: | ----: | ----: | ----: | ----: | ----: | ----: | -----: | :---- |
| leeftijd            |          0 |           1.00 | 14.87 |  1.13 | 12.00 | 14.00 | 15.00 | 15.00 |  19.00 | ÔľüÔľćÔľçÔľüÔľü |
| TR                  |         10 |           0.94 | 64.17 | 18.04 | 13.44 | 49.33 | 64.22 | 78.67 | 100.00 | ÔľüÔľůÔľçÔľçÔľâ |
| SC                  |         10 |           0.94 | 48.02 | 21.53 |  0.00 | 30.50 | 48.00 | 63.00 | 100.00 | ÔľéÔľçÔľçÔľćÔľé |
| AS                  |         10 |           0.94 | 70.55 | 13.89 | 18.00 | 62.17 | 70.50 | 78.33 |  98.50 | ÔľüÔľüÔľćÔľçÔľâ |
| OD                  |         10 |           0.94 | 71.35 | 15.57 | 25.29 | 60.43 | 73.57 | 82.14 | 100.00 | ÔľüÔľâÔľůÔľçÔľů |
| ASF                 |         10 |           0.94 | 48.76 | 27.54 |  0.00 | 30.00 | 49.00 | 70.00 | 100.00 | ÔľćÔľćÔľçÔľćÔľů |
| AE\_Stress          |         10 |           0.94 | 50.12 | 25.41 |  0.00 | 30.88 | 54.38 | 68.88 | 100.00 | ÔľâÔľâÔľçÔľçÔľé |
| happy               |          3 |           0.98 | 63.88 | 23.67 |  5.00 | 44.25 | 70.00 | 81.25 | 100.00 | ÔľéÔľůÔľůÔľçÔľć |
| rcads\_anx          |          3 |           0.98 |  8.34 |  5.88 |  0.00 |  4.00 |  7.00 | 11.00 |  26.00 | ÔľçÔľçÔľâÔľéÔľü |
| rcads\_dep          |          3 |           0.98 |  8.31 |  5.47 |  0.00 |  4.00 |  8.00 | 12.00 |  28.00 | ÔľçÔľçÔľâÔľéÔľü |
| distress\_precorona |          3 |           0.98 | 29.12 | 23.21 |  0.00 |  9.79 | 25.25 | 46.17 |  96.67 | ÔľçÔľůÔľâÔľéÔľü |
| distress\_nu        |          3 |           0.98 | 35.63 | 25.25 |  0.00 | 13.00 | 33.67 | 55.83 |  91.33 | ÔľçÔľćÔľćÔľůÔľé |
| vrienden\_klas      |         11 |           0.93 |  6.54 |  5.83 |  0.00 |  2.75 |  5.00 |  8.00 |  30.00 | ÔľçÔľâÔľüÔľüÔľü |
| vrienden\_school    |         11 |           0.93 |  9.29 |  8.11 |  0.00 |  3.75 |  7.00 | 13.00 |  30.00 | ÔľçÔľůÔľéÔľüÔľé |
| pest\_freq          |         11 |           0.93 |  0.08 |  0.41 |  0.00 |  0.00 |  0.00 |  0.00 |   3.00 | ÔľçÔľüÔľüÔľüÔľü |
