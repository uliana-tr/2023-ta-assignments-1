Assignment 3
================

# Pre-Processing

## Loading Documents and Basic Packages

``` r
#personal lveve daa from correction
person_level_data= read.csv('person_level_data.csv')
#data of art unitu changes
aus=read.csv("examiner_aus.csv")
#get id crosswalk table
ids=read.csv("examiner_ids.csv")


#load packages
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

## Pre-Processing Data for Aggregation by year

``` r
#cleaning data for ids.Remove Nas
ids <- ids %>% 
  filter(!is.na(old_pid))

#remove duplicatse in ids
ids <- ids %>% 
  distinct(
    old_pid,
    .keep_all = TRUE
)

#add the number of changes to art unit
aus_changes <- aus %>%
  group_by(old_pid) %>% 
  summarise(
    art_unit_changes = n(),
    art_unit_distinct_changes = n_distinct(examiner_art_unit),
  )

#merge the ids and aus changes together so we can then merge this with the person level data
aus_changes <-merge(
  aus_changes,
  ids,
  by='old_pid'
)

#rename patex_id to examiner_id for consistency
aus_changes <- aus_changes %>%
  rename('examiner_id'='patex_id')


person_level_data_complete <- merge(
  person_level_data,
  aus_changes,
  by='examiner_id',
  all.x= T
)

#remove the NAs from the art_unit changes in dataset
person_level_data_complete <- person_level_data_complete %>%
filter(!is.na(art_unit_changes))

person_level_data_complete <- person_level_data_complete %>%
filter(!is.na(start_year))
```

## Feature Engineering for Categorical Variable

``` r
#if the employee is above the average number of art unit changes then they are a 1 else they are a 0 

#first create annual changes
person_level_data_complete$annual_art_change <- with(person_level_data_complete, art_unit_changes/365)

mean_art_changes = mean(person_level_data_complete$annual_art_change)

person_level_data_complete$high_mobility <- ifelse(person_level_data_complete$annual_art_change > mean_art_changes ,"Yes","No")

person_level_data_complete$high_mobility<-as.factor(person_level_data_complete$high_mobility)


#remove the un-needed columns for testing dataset
drop <- c("old_pid","examiner_name","examiner_id","art_unit_changes","art_unit_changes_distinct","new_pid","annual_art_change")
person_level_data_complete = person_level_data_complete[,!(names(person_level_data_complete) %in% drop)]
```

# Analysis of data using Logistic Regression and Decision Trees

## Evaluation of Strength of Logistic and Tree Models

### Assume all Data Used for Training and Testing

#### Logistic regression

``` r
predictors<-c("art_unit","gender","start_year","tenure_days","tc","work_group","art_unit_distinct_changes")
logit=glm(high_mobility~art_unit+gender+tenure_days+tc+work_group+start_year,data=person_level_data_complete,family = "binomial")


#logit_complete=glm(high_mobility~art_unit+gender+start_year+latest_date+tenure_days+tc+work_group+art_unit_distinct_changes,data=person_level_data_complete,family = "binomial")

summary(logit)
```

    ## 
    ## Call:
    ## glm(formula = high_mobility ~ art_unit + gender + tenure_days + 
    ##     tc + work_group + start_year, family = "binomial", data = person_level_data_complete)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9134  -0.8612  -0.2276   0.8309   2.7119  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  1.021e+02  5.228e+01   1.952   0.0509 .  
    ## art_unit    -1.517e-02  1.541e-02  -0.984   0.3249    
    ## gendermale   7.948e-02  8.333e-02   0.954   0.3402    
    ## tenure_days  7.520e-04  5.844e-05  12.868  < 2e-16 ***
    ## tc           7.861e-03  1.629e-03   4.824  1.4e-06 ***
    ## work_group   8.001e-03  1.527e-02   0.524   0.6003    
    ## start_year  -5.334e-02  2.600e-02  -2.052   0.0402 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 5499.1  on 3966  degrees of freedom
    ## Residual deviance: 4286.7  on 3960  degrees of freedom
    ## AIC: 4300.7
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
library(rms)
```

    ## Loading required package: Hmisc

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     src, summarize

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

    ## Loading required package: SparseM

    ## 
    ## Attaching package: 'SparseM'

    ## The following object is masked from 'package:base':
    ## 
    ##     backsolve

``` r
logit_lrm=lrm(high_mobility~art_unit+gender+tenure_days+tc+work_group+start_year,data=person_level_data_complete,maxit=1000,tol=1e-11)


#logit_complete=glm(high_mobility~art_unit+gender+start_year+latest_date+tenure_days+tc+work_group+art_unit_distinct_changes,data=person_level_data_complete,family = "binomial")

logit_lrm
```

    ## Logistic Regression Model
    ##  
    ##  lrm(formula = high_mobility ~ art_unit + gender + tenure_days + 
    ##      tc + work_group + start_year, data = person_level_data_complete, 
    ##      tol = 1e-11, maxit = 1000)
    ##  
    ##                          Model Likelihood     Discrimination    Rank Discrim.    
    ##                                Ratio Test            Indexes          Indexes    
    ##  Obs           3967    LR chi2    1212.41     R2       0.351    C       0.795    
    ##   No           2002    d.f.             6    R2(6,3967)0.262    Dxy     0.590    
    ##   Yes          1965    Pr(> chi2) <0.0001    R2(6,2975)0.333    gamma   0.590    
    ##  max |deriv| 0.0001                           Brier    0.179    tau-a   0.295    
    ##  
    ##              Coef     S.E.    Wald Z Pr(>|Z|)
    ##  Intercept   102.0712 52.2810  1.95  0.0509  
    ##  art_unit     -0.0152  0.0154 -0.98  0.3249  
    ##  gender=male   0.0795  0.0833  0.95  0.3402  
    ##  tenure_days   0.0008  0.0001 12.87  <0.0001 
    ##  tc            0.0079  0.0016  4.82  <0.0001 
    ##  work_group    0.0080  0.0153  0.52  0.6003  
    ##  start_year   -0.0533  0.0260 -2.05  0.0402  
    ## 

#### Tree model

``` r
library(tree)
library(rpart)
library(rpart.plot)

mytree=rpart(
  high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group,
  data=person_level_data_complete,
  control=rpart.control(cp=0.01)
)
rpart.plot(mytree)
```

![](Assignment--3-Code_files/figure-gfm/tree%20model%20all%20data-1.png)<!-- -->

``` r
myoverfittedtree=rpart(
  high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group,
  data=person_level_data_complete,
  control=rpart.control(cp=0.0001)
)

opt_cp=myoverfittedtree$cptable[which.min(myoverfittedtree$cptable[,"xerror"]),"CP"]
plotcp(myoverfittedtree)
```

![](Assignment--3-Code_files/figure-gfm/optimal%20cp%20value%20for%20tree%20all%20data-1.png)<!-- -->

``` r
mytree_optimal=rpart(
  high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group,
  data=person_level_data_complete,
  control=rpart.control(cp=opt_cp)
)

rpart.plot(mytree_optimal)
```

![](Assignment--3-Code_files/figure-gfm/optimal%20tree%20all%20data-1.png)<!-- -->

``` r
remove(mytree,myoverfittedtree)
```

``` r
##Accuarcy evaluation with caret
#install.packages("caret")
require(caret) 
```

    ## Loading required package: caret

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:survival':
    ## 
    ##     cluster

``` r
##set up variables
#logistic regression glm
y_logit = predict(logit,person_level_data_complete)
#logistic regression rlm
y_logit_lrm = predict(logit_lrm,person_level_data_complete)
#prediction with tree
y_tree= predict(mytree_optimal,person_level_data_complete)
y_tree<-as.data.frame(y_tree)
y_tree$pred<- ifelse(y_tree$Yes > 0.5,"Yes","No")


###logsitic regression
person_level_data_complete$glm_prediction <- y_logit
person_level_data_complete$glm_prediction <- ifelse(person_level_data_complete$glm_prediction > 0.5,"Yes","No")
person_level_data_complete$glm_prediction <-as.factor(person_level_data_complete$glm_prediction)


cm<-confusionMatrix(
  data=person_level_data_complete$glm_prediction, #data is the prediction
  reference=person_level_data_complete$high_mobility) #reference is the 'true' value
accuracy_glm=cm$overall[1]
precision_glm=cm$byClass[5]
recall_glm=cm$byClass[6]
F1_glm=cm$byClass[7]
  


#logistic regression rlm
person_level_data_complete$rlm_prediction <- y_logit_lrm
person_level_data_complete$rlm_prediction <- ifelse(person_level_data_complete$rlm_prediction > 0.5,"Yes","No")
person_level_data_complete$rlm_prediction <-as.factor(person_level_data_complete$rlm_prediction)

cm<-confusionMatrix(
  data=person_level_data_complete$rlm_prediction, #data is the prediction
  reference=person_level_data_complete$high_mobility) #reference is the 'true' value
accuracy_rlm=cm$overall[1]
precision_rlm=cm$byClass[5]
recall_rlm=cm$byClass[6]
F1_rlm=cm$byClass[7]
  

#tree regression
person_level_data_complete$tree_prediction <- y_tree$pred
#person_level_data_complete$tree_acc <- ifelse(person_level_data_complete$tree_prediction > 0.5,"Yes","No")
person_level_data_complete$tree_prediction <- as.factor(person_level_data_complete$tree_prediction)

levels(person_level_data_complete$high_mobility)
```

    ## [1] "No"  "Yes"

``` r
levels(person_level_data_complete$tree_prediction)
```

    ## [1] "No"  "Yes"

``` r
cm<-confusionMatrix(
  data=person_level_data_complete$tree_prediction, #data is the prediction
  reference=person_level_data_complete$high_mobility) #reference is the 'true' value
accuracy_tree=cm$overall[1]
precision_tree=cm$byClass[5]
recall_tree=cm$byClass[6]
F1_tree=cm$byClass[7]



#drop=c('glm_prediction','rlm_prediction','tree_prediction')
#person_level_data_complete = person_level_data_complete[,!(names(person_level_data_complete) %in% drop)]

#remove redundant data
remove(y_tree,cm,y_logit_lrm,y_logit_glm,y_logit)
```

    ## Warning in remove(y_tree, cm, y_logit_lrm, y_logit_glm, y_logit): object
    ## 'y_logit_glm' not found

#### Accuracy of Models with all Data

``` r
c(accuracy_glm,precision_glm,recall_glm,F1_glm)
```

    ##  Accuracy Precision    Recall        F1 
    ## 0.7146458 0.6984489 0.7647353 0.7300906

``` r
c(accuracy_rlm,precision_rlm,recall_rlm,F1_rlm)
```

    ##  Accuracy Precision    Recall        F1 
    ## 0.7146458 0.6984489 0.7647353 0.7300906

``` r
c(accuracy_tree,precision_tree,recall_tree,F1_tree)
```

    ##  Accuracy Precision    Recall        F1 
    ## 0.7874968 0.8459701 0.7077922 0.7707370

### Assume Data Split into Training and Testing data sets

``` r
#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(person_level_data_complete), replace=TRUE, prob=c(0.7,0.3))
train  <- person_level_data_complete[sample, ]
test   <- person_level_data_complete[!sample, ]
```

#### Evaluation of Training and Test

``` r
summary(train)
```

    ##     art_unit       gender            start_year   latest_date       
    ##  Min.   :1600   Length:2762        Min.   :2000   Length:2762       
    ##  1st Qu.:1714   Class :character   1st Qu.:2000   Class :character  
    ##  Median :2111   Mode  :character   Median :2002   Mode  :character  
    ##  Mean   :1976                      Mean   :2003                     
    ##  3rd Qu.:2169                      3rd Qu.:2005                     
    ##  Max.   :2496                      Max.   :2015                     
    ##   tenure_days         tc         work_group   art_unit_distinct_changes
    ##  Min.   : 267   Min.   :1600   Min.   :1600   Min.   : 1.000           
    ##  1st Qu.:3884   1st Qu.:1700   1st Qu.:1710   1st Qu.: 1.000           
    ##  Median :5194   Median :2100   Median :2110   Median : 2.000           
    ##  Mean   :4750   Mean   :1932   Mean   :1972   Mean   : 2.567           
    ##  3rd Qu.:6218   3rd Qu.:2100   3rd Qu.:2160   3rd Qu.: 3.000           
    ##  Max.   :6518   Max.   :2400   Max.   :2490   Max.   :17.000           
    ##  high_mobility glm_prediction rlm_prediction tree_prediction
    ##  No :1395      No :1521       No :1521       No :1167       
    ##  Yes:1367      Yes:1241       Yes:1241       Yes:1595       
    ##                                                             
    ##                                                             
    ##                                                             
    ## 

``` r
summary(test)
```

    ##     art_unit       gender            start_year   latest_date       
    ##  Min.   :1609   Length:1205        Min.   :2000   Length:1205       
    ##  1st Qu.:1723   Class :character   1st Qu.:2000   Class :character  
    ##  Median :2115   Mode  :character   Median :2002   Mode  :character  
    ##  Mean   :1988                      Mean   :2003                     
    ##  3rd Qu.:2174                      3rd Qu.:2005                     
    ##  Max.   :2495                      Max.   :2016                     
    ##   tenure_days         tc         work_group   art_unit_distinct_changes
    ##  Min.   :  27   Min.   :1600   Min.   :1600   Min.   : 1.000           
    ##  1st Qu.:3782   1st Qu.:1700   1st Qu.:1720   1st Qu.: 1.000           
    ##  Median :5116   Median :2100   Median :2110   Median : 2.000           
    ##  Mean   :4718   Mean   :1943   Mean   :1984   Mean   : 2.621           
    ##  3rd Qu.:6207   3rd Qu.:2100   3rd Qu.:2170   3rd Qu.: 3.000           
    ##  Max.   :6350   Max.   :2400   Max.   :2490   Max.   :21.000           
    ##  high_mobility glm_prediction rlm_prediction tree_prediction
    ##  No :607       No :671        No :671        No :508        
    ##  Yes:598       Yes:534        Yes:534        Yes:697        
    ##                                                             
    ##                                                             
    ##                                                             
    ## 

#### Logistic regression

``` r
logit=glm(
  high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group,
  data=train,
  family = "binomial"
)

summary(logit)
```

    ## 
    ## Call:
    ## glm(formula = high_mobility ~ art_unit + gender + start_year + 
    ##     tenure_days + tc + work_group, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9196  -0.8549  -0.2097   0.8168   2.7688  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  5.241e+01  6.665e+01   0.786 0.431679    
    ## art_unit    -1.651e-02  1.873e-02  -0.882 0.378044    
    ## gendermale   5.990e-02  1.004e-01   0.597 0.550786    
    ## start_year  -2.872e-02  3.313e-02  -0.867 0.386118    
    ## tenure_days  8.378e-04  7.631e-05  10.980  < 2e-16 ***
    ## tc           7.641e-03  1.983e-03   3.853 0.000117 ***
    ## work_group   9.517e-03  1.855e-02   0.513 0.607945    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3828.7  on 2761  degrees of freedom
    ## Residual deviance: 2942.9  on 2755  degrees of freedom
    ## AIC: 2956.9
    ## 
    ## Number of Fisher Scoring iterations: 5

#### Tree model

``` r
myoverfittedtree=rpart(
  high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group,
  data=train,
  control=rpart.control(cp=0.0001)
)
opt_cp=myoverfittedtree$cptable[which.min(myoverfittedtree$cptable[,"xerror"]),"CP"]
plotcp(myoverfittedtree)
```

![](Assignment--3-Code_files/figure-gfm/optimal%20cp%20value%20for%20tree%20Train%20&%20Test-1.png)<!-- -->

``` r
remove(myoverfittedtree)
```

``` r
mytree_optimal=rpart(
  high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group,
  data=person_level_data_complete,
  control=rpart.control(cp=opt_cp)
)
rpart.plot(mytree_optimal)
```

![](Assignment--3-Code_files/figure-gfm/optimal%20tree%20Train%20&%20Test-1.png)<!-- -->

``` r
##Accuarcy evaluation with caret
#install.packages("caret")
require(caret) 

##set up variables within the same dataframe
#logistic regression glm
y_logit = predict(logit,test)
#prediction with tree
y_tree= predict(mytree_optimal,test)
y_tree<-as.data.frame(y_tree)
y_tree$pred<- ifelse(y_tree$Yes > 0.5,"Yes","No")


###logsitic regression
test$glm_prediction <- y_logit
test$glm_prediction <- ifelse(test$glm_prediction > 0.5,"Yes","No")
test$glm_prediction <-as.factor(test$glm_prediction)


cm<-confusionMatrix(
  data=test$glm_prediction, #data is the prediction
  reference=test$high_mobility) #reference is the 'true' value
accuracy_glm=cm$overall[1]
precision_glm=cm$byClass[5]
recall_glm=cm$byClass[6]
F1_glm=cm$byClass[7]
  

#tree regression
test$tree_prediction <- y_tree$pred
#person_level_data_complete$tree_acc <- ifelse(person_level_data_complete$tree_prediction > 0.5,"Yes","No")
test$tree_prediction <- as.factor(test$tree_prediction)

cm<-confusionMatrix(
  data=test$tree_prediction, #data is the prediction
  reference=test$high_mobility) #reference is the 'true' value
accuracy_tree=cm$overall[1]
precision_tree=cm$byClass[5]
recall_tree=cm$byClass[6]
F1_tree=cm$byClass[7]

#remove redundant data
remove(y_tree,cm,y_logit)
```

#### Accuracy of Training & Test Models

``` r
c(accuracy_glm,precision_glm,recall_glm,F1_glm)
```

    ##  Accuracy Precision    Recall        F1 
    ## 0.7053942 0.6863905 0.7644152 0.7233048

``` r
c(accuracy_tree,precision_tree,recall_tree,F1_tree)
```

    ##  Accuracy Precision    Recall        F1 
    ## 0.7858921 0.8483034 0.7001647 0.7671480

# ROC Curves & AUC with Training & Test Data

## ROC & Precision-Recall Curves

### Logistic Regression

``` r
#install.packages("precrec")
library(precrec)
##note for SScurves
#Scores are predictions
#labels are observed

high_mobility_labels <- as.vector(test$high_mobility)
high_mobility_labels <- ifelse(high_mobility_labels=="Yes",1,0)
#glm
#vectorize and set to binary
glm_prediction <- as.vector(test$glm_prediction)
glm_prediction <- ifelse(glm_prediction=="Yes",1,0)

sscurves_glm <- evalmod(scores = glm_prediction, labels = high_mobility_labels)

# Show ROC and Precision-Recall plots
plot(sscurves_glm)
```

![](Assignment--3-Code_files/figure-gfm/ROC%20curve-1.png)<!-- -->

``` r
#create auc for glm
aucs_glm <- auc(sscurves_glm)
```

``` r
# Show a Precision-Recall pl ot
plot(sscurves_glm, "PRC")
```

![](Assignment--3-Code_files/figure-gfm/glm%20precesion%20recall%20plots-1.png)<!-- -->

### Tree Model

``` r
#glm
#vectorize and set to binary
tree_prediction <- as.vector(test$tree_prediction)
tree_prediction <- ifelse(tree_prediction=="Yes",1,0)

sscurves_tree <- evalmod(scores = tree_prediction, labels = high_mobility_labels)

# Show ROC and Precision-Recall plots
plot(sscurves_tree)
```

![](Assignment--3-Code_files/figure-gfm/Tree%20ROC-1.png)<!-- -->

``` r
#create auc for tree
aucs_tree <- auc(sscurves_tree)
```

``` r
# Show a Precision-Recall plot
plot(sscurves_tree, "PRC")
```

![](Assignment--3-Code_files/figure-gfm/tree%20precesion%20recall%20plots-1.png)<!-- -->

## AUC for best curve

### Logistic Regression

``` r
# Get a data frame with AUC scores
aucs_glm
```

    ##   modnames dsids curvetypes      aucs
    ## 1       m1     1        ROC 0.7049501
    ## 2       m1     1        PRC 0.6772011

``` r
# Use knitr::kable to display the result in a table format
#knitr::kable(aucs)
```

### Tree Model

``` r
# Get a data frame with AUC scores
aucs_tree
```

    ##   modnames dsids curvetypes      aucs
    ## 1       m1     1        ROC 0.7865372
    ## 2       m1     1        PRC 0.7231303

``` r
# Use knitr::kable to display the result in a table format
#knitr::kable(aucs)
```

## Unused code Section

#### Lrm Model

``` r
# #rlm
# #vectorize and set to binary
# rlm_prediction <- as.vector(person_level_data_complete$rlm_prediction)
# rlm_prediction <- ifelse(rlm_prediction=="Yes",1,0)
# 
# sscurves_rlm <- evalmod(scores = rlm_prediction, labels = high_mobility_labels)
# 
# # Show ROC and Precision-Recall plots
# plot(sscurves_rlm)
# 
# #create auc for rlm
# aucs_rlm <- auc(sscurves_rlm)
```

``` r
# # Show a Precision-Recall plot
# plot(sscurves_rlm, "PRC")
```
