# 2016-07-05 MSDS 6306 Data Prep for Live Session 8
Claire Chu  
June 24, 2016  

<br>

#### Introduction


This tutorial is based on the EDA Exercises in the book, Doing Data Science.

It is based off of the tutorial completed on https://rpubs.com/tmcfl/simulated-click-analysis

There are five columns: Age, Gender (0=female, 1=male), Impressions, Clicks, and Signed_In status (0=not signed in, 1=signed in).

Assignment:

I. Setup the Environment
II. Download the Data
III. Investigate the Data
IV. Clean the Data
-create a new variable in R called "age group"
-place the "age group into the following catagories: < 18, 18-24, 25-34, 35-44, 45-54, 55-64, 65+
V. Analyze the Data
-Plot distributions of number impressions and click-through-rate (CTR = click/impression) for the age groups.
-define a new variable to segment users based on CTR behavior
-look at the total number of male impressions, male clicks, male CTR and males signed in
-create a table of CTRgroup vs AGEgroup counts
-make a graph of impressions v. age groups
-graph of impressions of males v. age

****************************
#### setup the environment


```r
library(downloader)
library(ggplot2)
setwd("/Users/macnificent/desktop/")
```


****************************

#### download the data

We will pull the data from the website and download it for manipulation in R


```r
fileLocation <- "http://stat.columbia.edu/~rachel/datasets/nyt1.csv"
data1 <- read.csv(url(fileLocation))
```


****************************


#### investigate the data

Rough analysis of all the datasets. There are 14905865 observations of 7 variables. Below are the details of dataset:



```r
head(data1)
```

```
##   Age Gender Impressions Clicks Signed_In
## 1  36      0           3      0         1
## 2  73      1           3      0         1
## 3  30      0           3      0         1
## 4  49      1           3      0         1
## 5  47      1          11      0         1
## 6  47      0          11      1         1
```

```r
str(data1)
```

```
## 'data.frame':	458441 obs. of  5 variables:
##  $ Age        : int  36 73 30 49 47 47 0 46 16 52 ...
##  $ Gender     : int  0 1 0 1 1 0 0 0 0 0 ...
##  $ Impressions: int  3 3 3 3 11 11 7 5 3 4 ...
##  $ Clicks     : int  0 0 0 0 0 1 1 0 0 0 ...
##  $ Signed_In  : int  1 1 1 1 1 1 0 1 1 1 ...
```

```r
summary(data1)
```

```
##       Age             Gender       Impressions         Clicks       
##  Min.   :  0.00   Min.   :0.000   Min.   : 0.000   Min.   :0.00000  
##  1st Qu.:  0.00   1st Qu.:0.000   1st Qu.: 3.000   1st Qu.:0.00000  
##  Median : 31.00   Median :0.000   Median : 5.000   Median :0.00000  
##  Mean   : 29.48   Mean   :0.367   Mean   : 5.007   Mean   :0.09259  
##  3rd Qu.: 48.00   3rd Qu.:1.000   3rd Qu.: 6.000   3rd Qu.:0.00000  
##  Max.   :108.00   Max.   :1.000   Max.   :20.000   Max.   :4.00000  
##    Signed_In     
##  Min.   :0.0000  
##  1st Qu.:0.0000  
##  Median :1.0000  
##  Mean   :0.7009  
##  3rd Qu.:1.0000  
##  Max.   :1.0000
```

```r
names(data1)
```

```
## [1] "Age"         "Gender"      "Impressions" "Clicks"      "Signed_In"
```


Let's take a look how the data is distributed through the use of data visualization.



```r
hist(data1$Age, main="", xlab="Age")
```

![](CChuDDS_Unit8DataPrep_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
hist(data1$Impressions, main="", xlab="# of Impressions")
```

![](CChuDDS_Unit8DataPrep_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
hist(data1$Clicks, main="", xlab="# of Clicks")
```

![](CChuDDS_Unit8DataPrep_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

****************************

#### clean the data

Create a new variable named "Age_group", that groups users into age categories into the following: < 18, 18-24, 25-34, 35-44, 45-54, 55-64, 65+


```r
data1$Age_Group <- cut(data1$Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf))
levels(data1$Age_Group) <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
head(data1)
```

```
##   Age Gender Impressions Clicks Signed_In Age_Group
## 1  36      0           3      0         1     35-44
## 2  73      1           3      0         1       65+
## 3  30      0           3      0         1     25-34
## 4  49      1           3      0         1     45-54
## 5  47      1          11      0         1     45-54
## 6  47      0          11      1         1     45-54
```

```r
summary(data1$Age_Group)
```

```
##    <18  18-24  25-34  35-44  45-54  55-64    65+ 
## 156358  35270  58174  70860  64288  44738  28753
```

Take a look at the changes:


```r
head(data1)
```

```
##   Age Gender Impressions Clicks Signed_In Age_Group
## 1  36      0           3      0         1     35-44
## 2  73      1           3      0         1       65+
## 3  30      0           3      0         1     25-34
## 4  49      1           3      0         1     45-54
## 5  47      1          11      0         1     45-54
## 6  47      0          11      1         1     45-54
```
<br>

****************************

#### Analyze the data

Generate a subset of the data that only contains lines with impressions greater than 0. Generate a column with the Click Through Rate (CTR)


```r
d1 <- subset(data1, Impressions>0)
d1$CTR <- d1$Clicks/d1$Impressions
head(d1)
```

```
##   Age Gender Impressions Clicks Signed_In Age_Group        CTR
## 1  36      0           3      0         1     35-44 0.00000000
## 2  73      1           3      0         1       65+ 0.00000000
## 3  30      0           3      0         1     25-34 0.00000000
## 4  49      1           3      0         1     45-54 0.00000000
## 5  47      1          11      0         1     45-54 0.00000000
## 6  47      0          11      1         1     45-54 0.09090909
```

<br>

make a graph of impressions v. age groups


```r
ggplot(subset(d1, Impressions>0), aes(x=Impressions, fill=Age_Group))+  geom_histogram(binwidth=1)
```

![](CChuDDS_Unit8DataPrep_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
ggplot(subset(d1, CTR>0), aes(x=CTR, fill=Age_Group))+labs(title="Click-through rate by age group (05/01/2012)")+geom_histogram(binwidth=.025)
```

![](CChuDDS_Unit8DataPrep_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

<br>

segment and categorize the clickthrough data 


```r
d1$CTR_sub <- cut(d1$CTR, c(-Inf, 0.2, 0.4, 0.6, 0.8, Inf))
levels(d1$CTR_sub) <- c("<0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", ">0.8")
```

<br>

look at the total number of male impressions, male clicks, male CTR and males signed in


```r
d2 <- subset(d1, Gender>0)
str(d2)
```

```
## 'data.frame':	167146 obs. of  8 variables:
##  $ Age        : int  73 49 47 40 31 59 29 19 48 48 ...
##  $ Gender     : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Impressions: int  3 3 11 3 5 4 2 4 9 4 ...
##  $ Clicks     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ Signed_In  : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Age_Group  : Factor w/ 7 levels "<18","18-24",..: 7 5 5 4 3 6 3 2 5 5 ...
##  $ CTR        : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CTR_sub    : Factor w/ 5 levels "<0.2","0.2-0.4",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```r
summary(d2)
```

```
##       Age             Gender   Impressions         Clicks       
##  Min.   :  7.00   Min.   :1   Min.   : 1.000   Min.   :0.00000  
##  1st Qu.: 28.00   1st Qu.:1   1st Qu.: 3.000   1st Qu.:0.00000  
##  Median : 40.00   Median :1   Median : 5.000   Median :0.00000  
##  Mean   : 40.83   Mean   :1   Mean   : 5.042   Mean   :0.07046  
##  3rd Qu.: 52.00   3rd Qu.:1   3rd Qu.: 6.000   3rd Qu.:0.00000  
##  Max.   :107.00   Max.   :1   Max.   :20.000   Max.   :3.00000  
##                                                                 
##    Signed_In Age_Group          CTR             CTR_sub      
##  Min.   :1   <18  :12279   Min.   :0.00000   <0.2   :163827  
##  1st Qu.:1   18-24:18697   1st Qu.:0.00000   0.2-0.4:  2786  
##  Median :1   25-34:30750   Median :0.00000   0.4-0.6:   427  
##  Mean   :1   35-44:37429   Mean   :0.01392   0.6-0.8:    21  
##  3rd Qu.:1   45-54:33788   3rd Qu.:0.00000   >0.8   :    85  
##  Max.   :1   55-64:23830   Max.   :1.00000                   
##              65+  :10373
```

```r
ggplot(subset(d2, Impressions>0), aes(x=Impressions, fill=Age_Group))+  geom_histogram(binwidth=1)
```

![](CChuDDS_Unit8DataPrep_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
<br>

create a table of CTRgroup vs AGEgroup counts


```r
d3 <- summary(d2$Age_Group : d2$CTR_sub)
```

make a graph of male impressions v. age groups


```r
ggplot(subset(d2, Impressions>0), aes(x=Impressions, fill=Age_Group))+  geom_histogram(binwidth=1)
```

![](CChuDDS_Unit8DataPrep_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
ggplot(subset(d2, CTR>0), aes(x=CTR, fill=Age_Group))+labs(title="Male Click-through rate by age group (05/01/2012)")+geom_histogram(binwidth=.025)
```

![](CChuDDS_Unit8DataPrep_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

distribution of male age


```r
ggplot(subset(d2, Impressions>0), aes(x=Age, fill=Impressions))+geom_histogram(binwidth=.5)
```

![](CChuDDS_Unit8DataPrep_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

Conclusion: We can see that the majority of the dataset included data where the age and clicks were 0. I suspect that this was not in fact the truth but were simply users who did not signin and were therefore considered anonymous by the system. Because of this, we were able to subset the data (d1) and remove these entries. Once we did this, we can see that the biggest age group of users were the <18 group. Subsetting further, we can look at the male only users and see the impressions v age. Looking at the histogram, we can confirm our findings since the largest distribution of users seems to be in the 18-45 age range.
