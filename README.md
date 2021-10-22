---
title: "PM566 Midterm Project"
author: "Leona Ma"
date: "10/21/2021"
output: 
    html_document:
      toc: yes 
      toc_float: yes
      keep_md : yes 
    github_document:
      html_preview: false
always_allow_html: true
---



Introduction (provide background on your dataset and formulated question), 
Methods (include how and where the data were acquired, how you cleaned and wrangled the data, what tools you used for data exploration), 
Preliminary Results (provide summary statistics in tabular form and publication-quality figures, take a look at the kable function from knitr to write nice tables in Rmarkdown), 
and a brief Conclusion about what you found in terms of the formulated question.

Introduction:
Methods:


```r
library(data.table)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(dtplyr)
library(plyr)
```

```
## ------------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## ------------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
## ✓ tibble  3.1.2     ✓ stringr 1.4.0
## ✓ tidyr   1.1.3     ✓ forcats 0.5.1
## ✓ readr   1.4.0
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x plyr::arrange()    masks dplyr::arrange()
## x dplyr::between()   masks data.table::between()
## x purrr::compact()   masks plyr::compact()
## x plyr::count()      masks dplyr::count()
## x plyr::failwith()   masks dplyr::failwith()
## x dplyr::filter()    masks stats::filter()
## x dplyr::first()     masks data.table::first()
## x plyr::id()         masks dplyr::id()
## x dplyr::lag()       masks stats::lag()
## x dplyr::last()      masks data.table::last()
## x plyr::mutate()     masks dplyr::mutate()
## x plyr::rename()     masks dplyr::rename()
## x plyr::summarise()  masks dplyr::summarise()
## x plyr::summarize()  masks dplyr::summarize()
## x purrr::transpose() masks data.table::transpose()
```

# Part 1 Data Cleaning and Wrangling

## Step1 reading and combining the data sets

```r
tobacco10 <- data.table::fread("tobacco2010.csv")
tobacco11 <- data.table::fread("tobacco2011.csv")
location <- data.table::fread("location.csv")
tobacco <- rbind(tobacco10, tobacco11)
tobacco11_L <- merge(tobacco11, location, by = "State")
```
tobacco11_L missed two obs, but it's fine, we don't want nationwide in map. 

## Step2 creating new varible "smokers and nonsmokers"

```r
tobacco[, smokers := `Smoke everyday`+`Smoke some days`]
tobacco[, nonsmokers := `Former smoker`+`Never smoked`]
```

## Step3 Spliting the data sets

```r
count(tobacco, 'State')
```

```
##                                       State freq
## 1                                   Alabama   17
## 2                                    Alaska   17
## 3                                   Arizona   17
## 4                                  Arkansas   17
## 5                                California   17
## 6                                  Colorado   17
## 7                               Connecticut   17
## 8                                  Delaware   17
## 9                      District of Columbia   16
## 10                                  Florida   17
## 11                                  Georgia   17
## 12                                     Guam    8
## 13                                   Hawaii   16
## 14                                    Idaho   17
## 15                                 Illinois   17
## 16                                  Indiana   17
## 17                                     Iowa   17
## 18                                   Kansas   17
## 19                                 Kentucky   17
## 20                                Louisiana   17
## 21                                    Maine   17
## 22                                 Maryland   17
## 23                            Massachusetts   17
## 24                                 Michigan   17
## 25                                Minnesota   17
## 26                              Mississippi   17
## 27                                 Missouri   17
## 28                                  Montana   17
## 29               Nationwide (States and DC)   17
## 30 Nationwide (States, DC, and Territories)   17
## 31                                 Nebraska   17
## 32                                   Nevada   17
## 33                            New Hampshire   17
## 34                               New Jersey   17
## 35                               New Mexico   17
## 36                                 New York   17
## 37                           North Carolina   17
## 38                             North Dakota   17
## 39                                     Ohio   17
## 40                                 Oklahoma   17
## 41                                   Oregon   17
## 42                             Pennsylvania   17
## 43                              Puerto Rico   16
## 44                             Rhode Island   17
## 45                           South Carolina   17
## 46                             South Dakota   17
## 47                                Tennessee   17
## 48                                    Texas   17
## 49                                     Utah   15
## 50                                  Vermont   17
## 51                           Virgin Islands   10
## 52                                 Virginia   17
## 53                               Washington   17
## 54                            West Virginia   17
## 55                                Wisconsin   17
## 56                                  Wyoming   17
```

```r
nation <- tobacco[State == "Nationwide (States and DC)"]
nation_t <- tobacco[State == "Nationwide (States, DC, and Territories)"]
area <- tobacco[!State == "Nationwide (States, DC, and Territories)" & !State == "Nationwide (States and DC)"]
```


# Part 2 Looking at the Data (EDA)
summary of overall trend in nation wide

```r
summary(tobacco$smokers)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    5.80   19.20   21.80   21.39   23.80   34.10
```

## Step1 Facet plot showing scatterplots with regression lines of different smoking status vs Year by State.

1. Smoke everyday

```r
ggplot(data=area, mapping=aes(x = Year, y = `Smoke everyday`)) +
  geom_point() + 
  facet_wrap(~State) +
  geom_smooth(method='lm', formula = y~x)+
  labs(title = "scatterplots of 'Smoke everyday' vs Year by State", x="Yeas", y="Percentage of people who smoke everyday")
```

![](README_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

2. Smoke some days

```r
ggplot(data=area, mapping=aes(x = Year, y = `Smoke some days`)) +
  geom_point() + 
  facet_wrap(~State) +
  geom_smooth(method='lm', formula = y~x)+
  labs(title = "scatterplots of 'Smoke some days' vs Year by State", x="Yeas", y="Percentage of people who smoke some days")
```

![](README_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Former smoker

```r
ggplot(data=area, mapping=aes(x = Year, y = `Former smoker`)) +
  geom_point() + 
  facet_wrap(~State) +
  geom_smooth(method='lm', formula = y~x)+
  labs(title = "scatterplots of 'Smoke some days' vs Year by State", x="Yeas", y="Percentage of people who are former smoker")
```

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

4. Never smoked

```r
ggplot(data=area, mapping=aes(x = Year, y = `Never smoked`)) +
  geom_point() + 
  facet_wrap(~State) +
  geom_smooth(method='lm', formula = y~x)+
  labs(title = "scatterplots of 'Smoke some days' vs Year by State", x="Yeas", y="Percentage of people who are Never smoked")
```

![](README_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

5. Smokers regardless of frequency

```r
ggplot(data=area, mapping=aes(x = Year, y = smokers)) +
  geom_point() + 
  facet_wrap(~State) +
  geom_smooth(method='lm', formula = y~x)+
  labs(title = "scatterplots of smokers vs Year by State", x="Yeas", y="Percentage of people who are smokers")
```

![](README_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

6. present nonsmokers regardless of history

```r
ggplot(data=area, mapping=aes(x = Year, y = nonsmokers)) +
  geom_point() + 
  facet_wrap(~State) +
  geom_smooth(method='lm', formula = y~x)+
  labs(title = "scatterplots of smokers vs Year by State", x="Yeas", y="Percentage of people who are smokers")
```

![](README_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
ggplot(data = nation, mapping =aes(x = Year, y=nonsmokers)) +
  geom_point() +
  geom_smooth(method='lm', formula = y~x)
```

![](README_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

