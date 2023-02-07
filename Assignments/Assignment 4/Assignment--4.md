Assignment 4
================

# Data Cleaning and Pre-Processing

## Loading Data and Basic Packages

### Load packages

``` r
library(tidyverse)
library(dplyr)
library(stringr)
library(arrow)
library(lubridate)
library(zoo)
library(ggplot2)
```

### Load data
```{r loading}
# Personal level data from correction
person_level_data= read.csv('person_level_data.csv')

# Data of art unit changes
aus=read.csv("examiner_aus.csv")

# Get id crosswalk table
ids=read.csv("examiner_ids.csv")

# Get parquet data
App_data=read_parquet('app_data_sample.parquet')
```

## Clean Data

### Clean ids dataset
Remove NAs:
``` r
ids <- ids %>% 
  filter(!is.na(old_pid))
```  
Remove duplicates in ids:
``` r
ids <- ids %>% 
  distinct(
    old_pid,
    .keep_all = TRUE
)
```
### Clean parquet dataset
``` r
App_data <- App_data %>% 
  mutate(start_date = ymd(filing_date), Status_date = as_date(dmy_hms(appl_status_date))) %>% 
  filter(year(Status_date)<2018)
```

Merge the App ID and the person level data to get additional details we can bring in as needed:
``` r
App_data <- merge(App_data, person_level_data, by = 'examiner_id',all=T)
```
Clean the app dataset:
``` r
App_data <- App_data %>% 
  filter(!is.na(examiner_id))
```
Rename tc.x to tc:
``` r
names(App_data)[names(App_data) == "tc.x"] <- "tc"
```

## Pre-process

### Get the quarter number
``` r
Q_data=App_data
Q_data$Quarter_Year=as.character(as.yearqtr(Q_data$Status_date))
```
### Create new dataset that only includes data to be used in analysis
``` r
Q_data_leaving <- Q_data

Q_data <- Q_data %>% 
  group_by(Quarter_Year,tc,gender) %>% 
  summarise(
    num_exam = n()
    )

Q_data_leaving <- Q_data_leaving %>% 
  group_by(examiner_art_unit, examiner_id,gender) %>% 
  summarise(
    num_exams = n(),
    change_date=max(Quarter_Year),
    tc=min(tc)
    )

Q_data_leaving$t1_change_art= 1

Q_data_leaving_merge <- Q_data_leaving %>%
  group_by(tc,gender,change_date ) %>% 
  summarise(
    leaving = sum(t1_change_art)
    )

Q_data <- Q_data %>% left_join( Q_data_leaving_merge, 
        by=c('tc'='tc', 'gender'='gender','Quarter_Year'='change_date' ))

Q_data_plot <- Q_data %>% 
  filter(!grepl("2017",Quarter_Year))
```

### Use Quarterly Data to Get Number of Examiners per Technology Centers

``` r
library(ggplot2)
library(sjlabelled)

ggplot(Q_data , aes(x=as_factor(tc),y=num_exam,fill=gender)) +
  geom_col() +
  facet_wrap('Quarter_Year',scales='free_y')+
  xlab("Technology Center")
```

![](Assignment--4_files/figure-gfm/feature%20eng%20quarterly%20data-1.png)<!-- -->

``` r
  #expand_limits(y=c(0.0, 50.0))
  #scale_y_continuous(expand = expansion(c(0, 1)))
```

# Casual Inference

## Demonstrate Causality Validity

Here are are examining the number of examiners per quarter and year:

``` r
for (x in 2000:2017){
Q_data_temp <- Q_data %>% 
  filter(grepl(as.character(x),Quarter_Year))
  
print(ggplot(Q_data_temp , aes(x=as_factor(tc),y=num_exam,fill=gender)) +
  geom_col() +
  facet_wrap('Quarter_Year',scales='free_y')+
  xlab("Technology Center"))
}
```

![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-1.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-2.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-3.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-4.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-5.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-6.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-7.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-8.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-9.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-10.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-11.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-12.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-13.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-14.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-15.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-16.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-17.png)<!-- -->![](Assignment--4_files/figure-gfm/causal%20Inference%20investigation%20by%20year-18.png)<!-- -->

Here we look at the tc data based on who will leave at time t+1:

``` r
ggplot(data=Q_data_plot, aes(x=as_factor(Quarter_Year), y=leaving, shape=gender,group=interaction(as_factor(tc),gender))) +
  geom_line(aes(color=as_factor(tc)))+
  scale_color_manual(values=c("#FFA500", "#009900", "#3366FF","#FF0000","#000000","#000000"))+
  geom_point(aes(colour=gender),alpha=3/10)+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=4.5))+
  facet_wrap('tc',nrow=2)
```

![](Assignment--4_files/figure-gfm/causal%20Inference%20on%20t+1%20data-1.png)<!-- -->


### Demonstrate Paralel Trends (Plot line graphs) 
We can see identical trends across all Technology Centers for the number of examiners per quarter:

``` r
ggplot(data=Q_data_plot, aes(x=as_factor(Quarter_Year), y=num_exam, shape=gender,group=interaction(as_factor(tc),gender))) +
  geom_line(aes(color=as_factor(tc)))+
  scale_color_manual(values=c("#FFA500", "#009900", "#3366FF","#FF0000","#000000","#000000"))+
  geom_point(aes(colour=gender),alpha=3/10)+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=4.5),
        plot.title = element_text(color="black", size=14, face="bold.italic"))+
  ggtitle("Number of Examiners per TC over time broken out by gender")
```

![](Assignment--4_files/figure-gfm/causal%20Inference%20plots-1.png)<!-- -->

``` r
ggplot(data=Q_data_plot, aes(x=as_factor(Quarter_Year), y=num_exam, shape=gender,group=interaction(as_factor(tc),gender))) +
  geom_line(aes(color=as_factor(tc)))+
  scale_color_manual(values=c("#FFA500", "#009900", "#3366FF","#FF0000","#000000","#000000"))+
  geom_point(aes(colour=gender),alpha=3/10)+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=4.5),
        plot.title = element_text(color="black", size=14, face="bold.italic"))+
  ggtitle("Number of Examiners per TC over time broken out by gender with data cleaning for non-assigned examiners")+
  facet_wrap('tc',nrow=2)
```

![](Assignment--4_files/figure-gfm/causal%20Inference%20plots-2.png)<!-- -->

``` r
ggplot(data=Q_data_plot, aes(x=as_factor(Quarter_Year), y=leaving, shape=gender,group=interaction(as_factor(tc),gender))) +
  geom_line(aes(color=as_factor(tc)))+
  scale_color_manual(values=c("#FFA500", "#009900", "#3366FF","#FF0000","#000000","#000000"))+
  geom_point(aes(colour=gender),alpha=3/10)+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=4.5),
        plot.title = element_text(color="black", size=14, face="bold.italic"))+
  ggtitle("Number of Examiners that will be leaving at time t+1")+
  facet_wrap('tc',nrow=2)
```

![](Assignment--4_files/figure-gfm/causal%20Inference%20plots-3.png)<!-- -->

``` r
tc_in=c(1600,2100)

Q_data_plot2 <- Q_data_plot %>% 
  filter(tc %in% tc_in)
Q_data_plot2 <- Q_data_plot2 %>% 
  filter(!is.na(gender))


ggplot(data=Q_data_plot2, aes(x=as_factor(Quarter_Year), y=num_exam, shape=gender,group=interaction(as_factor(tc),gender))) +
  geom_line(aes(color=as_factor(tc)))+
  scale_color_manual(values=c("#FFA500", "#009900", "#3366FF","#FF0000","#000000","#000000"))+
  geom_point(alpha=3/10)+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=4.5),
        plot.title = element_text(color="black", size=14, face="bold.italic"))+
  ggtitle("Number of Examiners over time in tc 1600 & 2100")
```

![](Assignment--4_files/figure-gfm/causal%20Inference%20plots-4.png)<!-- -->

``` r
ggplot(data=Q_data_plot2, aes(x=as_factor(Quarter_Year), y=leaving, shape=gender,group=interaction(as_factor(tc),gender))) +
  geom_line(aes(color=as_factor(tc)))+
  scale_color_manual(values=c("#FFA500", "#009900", "#3366FF","#FF0000","#000000","#000000"))+
  geom_point(alpha=3/10)+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=4.5),
        plot.title = element_text(color="black", size=14, face="bold.italic"))+
  ggtitle("Number of Examiners in tc removed gender NAs")
```

![](Assignment--4_files/figure-gfm/causal%20Inference%20plots-5.png)<!-- -->



## Compare two tech centers
### Decide on two tech centers
Comparing two tech centers based on the number of examiners. We have
chosen to examain 1600 and 2100 as these two centers demonstrate pthe
closes paralell trends.


``` r
tc_in=c(1600,2100)

Q_data_plot2 <- Q_data_plot %>% 
  filter(tc %in% tc_in)
Q_data_plot2 <- Q_data_plot2 %>% 
  filter(!is.na(gender))


ggplot(data=Q_data_plot2, aes(x=as_factor(Quarter_Year), y=num_exam, shape=gender,group=interaction(as_factor(tc),gender))) +
  geom_line(aes(color=as_factor(tc)))+
  scale_color_manual(values=c("#FFA500", "#009900", "#3366FF","#FF0000","#000000","#000000"))+
  geom_point(alpha=3/10)+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,, size=4.5),
        plot.title = element_text(color="black", size=14, face="bold.italic"))+
        ggtitle("Number of Examiners leaving in time t+1 for tc 1600 & 2100")

```

![](Assignment--4_files/figure-gfm/causal%20Inference%20two%20centers-1.png)<!-- -->

### Analyze each chosen treatment center (summary stats)

Load additional packages:
``` r
library(psych)
library(doBy)
```
View descriptive statistics by tech center:
``` r
describeBy(
  Q_data_plot2,
  Q_data_plot2$tc) # grouping variable
```
    ## 
    ##  Descriptive statistics by group 
    ## group: 1600
    ##               vars   n    mean      sd median trimmed     mad  min  max range
    ## Quarter_Year*    1 133   34.25   19.27     34   34.25   25.20    1   67    66
    ## tc               2 133 1600.00    0.00   1600 1600.00    0.00 1600 1600     0
    ## gender*          3 133    1.50    0.50      2    1.50    0.00    1    2     1
    ## num_exam         4 133 2881.35 1881.86   2516 2810.72 2170.53    4 8333  8329
    ## leaving          5 110    5.56    6.60      3    4.11    2.97    1   34    33
    ##                skew kurtosis     se
    ## Quarter_Year*  0.00    -1.23   1.67
    ## tc              NaN      NaN   0.00
    ## gender*       -0.01    -2.01   0.04
    ## num_exam       0.37    -0.52 163.18
    ## leaving        2.17     4.85   0.63
    ## ------------------------------------------------------------ 
    ## group: 2100
    ##               vars   n    mean      sd median trimmed     mad  min  max range
    ## Quarter_Year*    1 129   33.25   18.69     33   33.25   23.72    1   65    64
    ## tc               2 129 2100.00    0.00   2100 2100.00    0.00 2100 2100     0
    ## gender*          3 129    1.50    0.50      2    1.50    0.00    1    2     1
    ## num_exam         4 129 2261.26 2159.71   1357 1936.73 1727.23    2 9340  9338
    ## leaving          5 117   11.42   19.17      5    7.00    4.45    1  131   130
    ##                skew kurtosis     se
    ## Quarter_Year*  0.00    -1.23   1.65
    ## tc              NaN      NaN   0.00
    ## gender*       -0.02    -2.02   0.04
    ## num_exam       1.33     1.52 190.15
    ## leaving        3.83    17.15   1.77


Another summary statistics by tech center and gender:

``` r
summaryBy(num_exam ~ tc + gender ,
          data = Q_data_plot2,
          FUN = summary)
```

    ## # A tibble: 4 × 8
    ##      tc gender num_exam.Min. `num_exam.1st Qu.` num_ex…¹ num_e…² num_e…³ num_e…⁴
    ##   <dbl> <chr>          <dbl>              <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ## 1  1600 female             6              1090.     2507   2824.   4012     8327
    ## 2  1600 male               4              1225      2567   2938.   4169     8333
    ## 3  2100 female             5               416      1003   1097.   1334.    4256
    ## 4  2100 male               2              1387      3290   3408.   4226     9340
    ## # … with abbreviated variable names ¹​num_exam.Median, ²​num_exam.Mean,
    ## #   ³​`num_exam.3rd Qu.`, ⁴​num_exam.Max.

View number of examiners per tech center and gender:
``` r
aggregate(cbind(num_exam) ~ tc + gender,
          data = Q_data_plot2,
          mean)
```

    ##     tc gender num_exam
    ## 1 1600 female 2824.136
    ## 2 2100 female 1097.078
    ## 3 1600   male 2937.701
    ## 4 2100   male 3407.523

### Find period of treatment / difference
By visually examining the data, we established two possible periods of treatment.

Treatment date - 2003 Q1:
``` r
ggplot(data=Q_data_plot2, aes(x=as_factor(Quarter_Year), y=num_exam, shape=gender,group=interaction(as_factor(tc),gender))) +
  geom_line(aes(color=as_factor(tc)))+
  scale_color_manual(values=c("#FFA500", "#009900", "#3366FF","#FF0000","#000000","#000000"))+
  geom_point(alpha=3/10)+
   geom_vline(xintercept = "2003 Q1", linetype="dotted", color = "blue", size=1.5)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=4.5),
        plot.title = element_text(color="black", size=14, face="bold.italic"))+
        ggtitle("Number of in tc with predicted difference point (tc 1600 & 2100)")

```

![](Assignment--4_files/figure-gfm/causal%20Inference%20treatment%20evaluation-1.png)<!-- -->

Treatment date - 2013 Q1:
``` r
ggplot(data=Q_data_plot2, aes(x=as_factor(Quarter_Year), y=leaving, shape=gender,group=interaction(as_factor(tc),gender))) +
  geom_line(aes(color=as_factor(tc)))+
  scale_color_manual(values=c("#FFA500", "#009900", "#3366FF","#FF0000","#000000","#000000"))+
  geom_point(alpha=3/10)+
   geom_vline(xintercept = "2013 Q1", linetype="dotted", color = "blue", size=1.5)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(color="black", size=14, face="bold.italic"))+
        ggtitle("Number of examiners leaving (tc 1600 & 2100)")

```


![](Assignment--4_files/figure-gfm/causal%20Inference%20treatment%20evaluation-2.png)<!-- -->



## Run Causal analysis

Load required package:
``` r
library(CausalImpact)
```

Treatment date - 2003 Q1:
``` r
Q_data_plot_num <-Q_data_plot2[ , !names(Q_data_plot2) %in% c("leaving")]


impactdata <- Q_data_plot_num
#impactdata$female <- ifelse(impactdata$gender == 'female', 1, 0)
impactdata$male <- ifelse(impactdata$gender == 'male', 1, 0)
#impactdata$tc_1600 <- ifelse(impactdata$tc == '1600', 1, 0)
#impactdata$tc_2100 <- ifelse(impactdata$tc == '2100', 1, 0)

impactdata <- impactdata %>% 
  group_by(Quarter_Year) %>% 
  summarise(
    num_exam = sum(num_exam)
    )


pre.period <- c(1, 15)
post.period <- c(30, 67)

causaldata <- impactdata[,2]

impact <- CausalImpact(causaldata, pre.period, post.period)

summary(impact)
```

    ## Posterior inference {CausalImpact}
    ## 
    ##                          Average          Cumulative      
    ## Actual                   14880            565451          
    ## Prediction (s.d.)        1374 (381)       52223 (14487)   
    ## 95% CI                   [604, 2119]      [22965, 80527]  
    ##                                                           
    ## Absolute effect (s.d.)   13506 (381)      513228 (14487)  
    ## 95% CI                   [12761, 14276]   [484924, 542486]
    ##                                                           
    ## Relative effect (s.d.)   1096% (493%)     1096% (493%)    
    ## 95% CI                   [602%, 2362%]    [602%, 2362%]   
    ## 
    ## Posterior tail-area probability p:   0.00102
    ## Posterior prob. of a causal effect:  99.89837%
    ## 
    ## For more details, type: summary(impact, "report")

``` r
plot(impact)
```

![](Assignment--4_files/figure-gfm/running%20causal%20analysis%202-1.png)<!-- -->

Treatment date - 2013 Q1:
``` r
Q_data_plot_t <-Q_data_plot2[ , !names(Q_data_plot2) %in% c("num_exam")] ## works as expected 



Q_data_plot_t <- Q_data_plot_t %>% 
  filter(!is.na(leaving))

Q_data_plot_t <- Q_data_plot_t %>% 
  filter(!grepl("female",gender))


Q_data_plot_t <- Q_data_plot_t %>% 
  filter(grepl(2100,tc))


Q_data_plot_t <- Q_data_plot_t %>% 
  group_by(Quarter_Year) %>% 
  summarise(
    leaving = sum(leaving)
    )



impactdata <- Q_data_plot_t[ , !names(Q_data_plot_t) %in% c("tc","gender","Quarter_Year")] ## works as expected 

#impactdata$female <- ifelse(impactdata$gender == 'female', 1, 0)
#impactdata$male <- ifelse(impactdata$gender == 'male', 1, 0)
#impactdata$tc_1600 <- ifelse(impactdata$tc == '1600', 1, 0)
#impactdata$tc_2100 <- ifelse(impactdata$tc == '2100', 1, 0)


pre.period <- c(1, 50)
post.period <- c(60, 64)

causaldata <- impactdata


impact <- CausalImpact(causaldata, pre.period, post.period)


summary(impact)
```

    ## Posterior inference {CausalImpact}
    ## 
    ##                          Average         Cumulative   
    ## Actual                   84              418          
    ## Prediction (s.d.)        7.3 (2.3)       36.7 (11.3)  
    ## 95% CI                   [3, 12]         [15, 59]     
    ##                                                       
    ## Absolute effect (s.d.)   76 (2.3)        381 (11.3)   
    ## 95% CI                   [72, 81]        [359, 403]   
    ##                                                       
    ## Relative effect (s.d.)   1208% (784%)    1208% (784%) 
    ## 95% CI                   [610%, 2709%]   [610%, 2709%]
    ## 
    ## Posterior tail-area probability p:   0.001
    ## Posterior prob. of a causal effect:  99.8996%
    ## 
    ## For more details, type: summary(impact, "report")

``` r
plot(impact)
```

![](Assignment--4_files/figure-gfm/running%20causal%20analysis%20time-1.png)<!-- -->

