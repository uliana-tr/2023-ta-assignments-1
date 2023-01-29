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
logit=glm(high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group+art_unit_distinct_changes,data=person_level_data_complete,family = "binomial")


#logit_complete=glm(high_mobility~art_unit+gender+start_year+latest_date+tenure_days+tc+work_group+art_unit_distinct_changes,data=person_level_data_complete,family = "binomial")

summary(logit)
```

    ## 
    ## Call:
    ## glm(formula = high_mobility ~ art_unit + gender + start_year + 
    ##     tenure_days + tc + work_group + art_unit_distinct_changes, 
    ##     family = "binomial", data = person_level_data_complete)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.0670  -0.7479  -0.1007   0.7991   3.3822  
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                2.631e+02  5.738e+01   4.585 4.55e-06 ***
    ## art_unit                   9.793e-03  1.661e-02   0.590 0.555525    
    ## gendermale                 2.312e-03  8.947e-02   0.026 0.979390    
    ## start_year                -1.341e-01  2.855e-02  -4.698 2.62e-06 ***
    ## tenure_days                7.405e-04  6.109e-05  12.122  < 2e-16 ***
    ## tc                         6.919e-03  1.782e-03   3.884 0.000103 ***
    ## work_group                -1.651e-02  1.649e-02  -1.001 0.316654    
    ## art_unit_distinct_changes  6.764e-01  3.472e-02  19.480  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 5499.1  on 3966  degrees of freedom
    ## Residual deviance: 3771.5  on 3959  degrees of freedom
    ## AIC: 3787.5
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
logit_lrm=lrm(high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group+art_unit_distinct_changes,data=person_level_data_complete)


#logit_complete=glm(high_mobility~art_unit+gender+start_year+latest_date+tenure_days+tc+work_group+art_unit_distinct_changes,data=person_level_data_complete,family = "binomial")

logit_lrm
```

    ## Logistic Regression Model
    ##  
    ##  lrm(formula = high_mobility ~ art_unit + gender + start_year + 
    ##      tenure_days + tc + work_group + art_unit_distinct_changes, 
    ##      data = person_level_data_complete)
    ##  
    ##                         Model Likelihood     Discrimination    Rank Discrim.    
    ##                               Ratio Test            Indexes          Indexes    
    ##  Obs          3967    LR chi2    1727.55     R2       0.471    C       0.854    
    ##   No          2002    d.f.             7    R2(7,3967)0.352    Dxy     0.708    
    ##   Yes         1965    Pr(> chi2) <0.0001    R2(7,2975)0.439    gamma   0.708    
    ##  max |deriv| 6e-08                           Brier    0.154    tau-a   0.354    
    ##  
    ##                            Coef     S.E.    Wald Z Pr(>|Z|)
    ##  Intercept                 263.0525 57.3761  4.58  <0.0001 
    ##  art_unit                    0.0098  0.0166  0.59  0.5555  
    ##  gender=male                 0.0023  0.0895  0.03  0.9794  
    ##  start_year                 -0.1341  0.0285 -4.70  <0.0001 
    ##  tenure_days                 0.0007  0.0001 12.12  <0.0001 
    ##  tc                          0.0069  0.0018  3.88  0.0001  
    ##  work_group                 -0.0165  0.0165 -1.00  0.3167  
    ##  art_unit_distinct_changes   0.6764  0.0347 19.48  <0.0001 
    ## 

#### Tree model

``` r
library(tree)
library(rpart)
library(rpart.plot)

mytree=rpart(high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group+art_unit_distinct_changes,data=person_level_data_complete,control=rpart.control(cp=0.01))
rpart.plot(mytree)
```

![](Assignment--3-Code_files/figure-gfm/tree%20model%20all%20data-1.png)<!-- -->

``` r
myoverfittedtree=rpart(high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group+art_unit_distinct_changes,data=person_level_data_complete,control=rpart.control(cp=0.0001))
opt_cp=myoverfittedtree$cptable[which.min(myoverfittedtree$cptable[,"xerror"]),"CP"]
plotcp(myoverfittedtree)
```

![](Assignment--3-Code_files/figure-gfm/optimal%20cp%20value%20for%20tree%20all%20data-1.png)<!-- -->

``` r
mytree_optimal=rpart(high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group+art_unit_distinct_changes,data=person_level_data_complete,control=rpart.control(cp=opt_cp))
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
    ## 0.7537182 0.7134527 0.8556444 0.7781058

``` r
c(accuracy_rlm,precision_rlm,recall_rlm,F1_rlm)
```

    ##  Accuracy Precision    Recall        F1 
    ## 0.7537182 0.7134527 0.8556444 0.7781058

``` r
c(accuracy_tree,precision_tree,recall_tree,F1_tree)
```

    ##  Accuracy Precision    Recall        F1 
    ## 0.8338795 0.8749302 0.7827173 0.8262589

### Assume Data Split into Training and Testing data sets

``` r
#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(person_level_data_complete), replace=TRUE, prob=c(0.7,0.3))
train  <- person_level_data_complete[sample, ]
test   <- person_level_data_complete[!sample, ]
```

#### Logistic regression

``` r
logit=glm(
  high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group+art_unit_distinct_changes,
  data=train,
  family = "binomial"
)

summary(logit)
```

    ## 
    ## Call:
    ## glm(formula = high_mobility ~ art_unit + gender + start_year + 
    ##     tenure_days + tc + work_group + art_unit_distinct_changes, 
    ##     family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8521  -0.7477  -0.0958   0.7945   3.3890  
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                2.088e+02  7.254e+01   2.878  0.00401 ** 
    ## art_unit                   4.438e-03  2.004e-02   0.222  0.82469    
    ## gendermale                -2.726e-02  1.074e-01  -0.254  0.79972    
    ## start_year                -1.072e-01  3.608e-02  -2.971  0.00297 ** 
    ## tenure_days                8.362e-04  7.982e-05  10.476  < 2e-16 ***
    ## tc                         6.956e-03  2.154e-03   3.229  0.00124 ** 
    ## work_group                -1.122e-02  1.987e-02  -0.565  0.57227    
    ## art_unit_distinct_changes  6.500e-01  4.182e-02  15.542  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3828.7  on 2761  degrees of freedom
    ## Residual deviance: 2617.2  on 2754  degrees of freedom
    ## AIC: 2633.2
    ## 
    ## Number of Fisher Scoring iterations: 5

#### Tree model

``` r
myoverfittedtree=rpart(
  high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group+art_unit_distinct_changes,
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
  high_mobility~art_unit+gender+start_year+tenure_days+tc+work_group+art_unit_distinct_changes,
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
    ## 0.7560166 0.7194951 0.8451400 0.7772727

``` r
c(accuracy_tree,precision_tree,recall_tree,F1_tree)
```

    ##  Accuracy Precision    Recall        F1 
    ## 0.8381743 0.8601399 0.8105437 0.8346056

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
# Show a Precision-Recall plot
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
    ## 1       m1     1        ROC 0.7553459
    ## 2       m1     1        PRC 0.7414496

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
    ## 1       m1     1        ROC 0.8383822
    ## 2       m1     1        PRC 0.7922232

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
