#### What is the mean total number of steps taken per day?

The following chunk of code calculates the number of steps, the mean and the median per day using dplyr. It also plots a histogram for the number of steps per day.

``` r
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

``` r
library(rmarkdown)
library(knitr)
library(ggplot2)

actdata<-read.csv("./activity/activity.csv")
actdata$date<-as.Date(actdata$date)

mean_per_day<-actdata%>%
  na.omit()%>%
  group_by(date)%>%
  summarise(no_steps=sum(steps), mean_steps=mean(steps), 
            median_steps=median(steps))

histplot<-ggplot(data=mean_per_day, aes(x=no_steps))+
  geom_histogram(binwidth = 2000, colour="cadetblue4", fill="cadetblue2")+
  xlab("Number of steps per day")+
  ylab("Frequency")+
  scale_x_continuous(breaks=seq(0,25000, by=2500))+
  ggtitle("Histogram of number of steps per day")

ggsave("./figure/histplot.png", histplot)
```

    ## Saving 7 x 5 in image

``` r
plot(histplot)
```

![](PA1_template_files/figure-markdown_github/histplot.png)

And the following tibble shows mean and median per day.

``` r
mean_per_day2<-mean_per_day[,c("date", "mean_steps", "median_steps")]

knitr::kable(mean_per_day2, format="html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
date
</th>
<th style="text-align:right;">
mean\_steps
</th>
<th style="text-align:right;">
median\_steps
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2012-10-02
</td>
<td style="text-align:right;">
0.4375000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-03
</td>
<td style="text-align:right;">
39.4166667
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-04
</td>
<td style="text-align:right;">
42.0694444
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-05
</td>
<td style="text-align:right;">
46.1597222
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-06
</td>
<td style="text-align:right;">
53.5416667
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-07
</td>
<td style="text-align:right;">
38.2465278
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-09
</td>
<td style="text-align:right;">
44.4826389
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-10
</td>
<td style="text-align:right;">
34.3750000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-11
</td>
<td style="text-align:right;">
35.7777778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-12
</td>
<td style="text-align:right;">
60.3541667
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-13
</td>
<td style="text-align:right;">
43.1458333
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-14
</td>
<td style="text-align:right;">
52.4236111
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-15
</td>
<td style="text-align:right;">
35.2048611
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-16
</td>
<td style="text-align:right;">
52.3750000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-17
</td>
<td style="text-align:right;">
46.7083333
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-18
</td>
<td style="text-align:right;">
34.9166667
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-19
</td>
<td style="text-align:right;">
41.0729167
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-20
</td>
<td style="text-align:right;">
36.0937500
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-21
</td>
<td style="text-align:right;">
30.6284722
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-22
</td>
<td style="text-align:right;">
46.7361111
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-23
</td>
<td style="text-align:right;">
30.9652778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-24
</td>
<td style="text-align:right;">
29.0104167
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-25
</td>
<td style="text-align:right;">
8.6527778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-26
</td>
<td style="text-align:right;">
23.5347222
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-27
</td>
<td style="text-align:right;">
35.1354167
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-28
</td>
<td style="text-align:right;">
39.7847222
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-29
</td>
<td style="text-align:right;">
17.4236111
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-30
</td>
<td style="text-align:right;">
34.0937500
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-31
</td>
<td style="text-align:right;">
53.5208333
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-02
</td>
<td style="text-align:right;">
36.8055556
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-03
</td>
<td style="text-align:right;">
36.7048611
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-05
</td>
<td style="text-align:right;">
36.2465278
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-06
</td>
<td style="text-align:right;">
28.9375000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-07
</td>
<td style="text-align:right;">
44.7326389
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-08
</td>
<td style="text-align:right;">
11.1770833
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-11
</td>
<td style="text-align:right;">
43.7777778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-12
</td>
<td style="text-align:right;">
37.3784722
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-13
</td>
<td style="text-align:right;">
25.4722222
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-15
</td>
<td style="text-align:right;">
0.1423611
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-16
</td>
<td style="text-align:right;">
18.8923611
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-17
</td>
<td style="text-align:right;">
49.7881944
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-18
</td>
<td style="text-align:right;">
52.4652778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-19
</td>
<td style="text-align:right;">
30.6979167
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-20
</td>
<td style="text-align:right;">
15.5277778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-21
</td>
<td style="text-align:right;">
44.3993056
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-22
</td>
<td style="text-align:right;">
70.9270833
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-23
</td>
<td style="text-align:right;">
73.5902778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-24
</td>
<td style="text-align:right;">
50.2708333
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-25
</td>
<td style="text-align:right;">
41.0902778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-26
</td>
<td style="text-align:right;">
38.7569444
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-27
</td>
<td style="text-align:right;">
47.3819444
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-28
</td>
<td style="text-align:right;">
35.3576389
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-29
</td>
<td style="text-align:right;">
24.4687500
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>
#### What is the average daily activity pattern?

-   How many NAs we have on the data set?

    -   This was calculated by finding the number of NAs on the steps column:

``` r
sum(is.na(actdata$steps))
```

    ## [1] 2304

-   Fill in all the missing values.

    -   This was done by filling in using the median of that day (which is 0 for all of them).

``` r
for (i in 1:length(actdata$steps)){
  if (is.na(actdata$steps[i])){
    actdata$steps[i]<-0
  }
} 

new_mean_per_day<-actdata%>%
  group_by(date)%>%
  summarise(no_steps=sum(steps), mean_steps=mean(steps), 
            median_steps=median(steps))

histplot2<-ggplot(data=new_mean_per_day, aes(x=no_steps))+
  geom_histogram(binwidth = 2000, colour="lightpink4", fill="lightpink2")+
  xlab("Number of steps per day")+
  ylab("Frequency")+
  scale_x_continuous(breaks=seq(0,25000, by=2500))+
  ggtitle("New histogram of number of steps per day by filling in the NAs")

ggsave("./figure/histplot2.png", histplot2)
```

    ## Saving 7 x 5 in image

``` r
plot(histplot2)
```

![](PA1_template_files/figure-markdown_github/histplot2.png)

The plot looks about the same, but has a higher frequency of zeros, as expected.

``` r
new_mean_per_day2<-new_mean_per_day[,c("date", "mean_steps", "median_steps")]

knitr::kable(new_mean_per_day2, format="html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
date
</th>
<th style="text-align:right;">
mean\_steps
</th>
<th style="text-align:right;">
median\_steps
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2012-10-01
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-02
</td>
<td style="text-align:right;">
0.4375000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-03
</td>
<td style="text-align:right;">
39.4166667
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-04
</td>
<td style="text-align:right;">
42.0694444
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-05
</td>
<td style="text-align:right;">
46.1597222
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-06
</td>
<td style="text-align:right;">
53.5416667
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-07
</td>
<td style="text-align:right;">
38.2465278
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-08
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-09
</td>
<td style="text-align:right;">
44.4826389
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-10
</td>
<td style="text-align:right;">
34.3750000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-11
</td>
<td style="text-align:right;">
35.7777778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-12
</td>
<td style="text-align:right;">
60.3541667
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-13
</td>
<td style="text-align:right;">
43.1458333
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-14
</td>
<td style="text-align:right;">
52.4236111
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-15
</td>
<td style="text-align:right;">
35.2048611
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-16
</td>
<td style="text-align:right;">
52.3750000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-17
</td>
<td style="text-align:right;">
46.7083333
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-18
</td>
<td style="text-align:right;">
34.9166667
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-19
</td>
<td style="text-align:right;">
41.0729167
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-20
</td>
<td style="text-align:right;">
36.0937500
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-21
</td>
<td style="text-align:right;">
30.6284722
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-22
</td>
<td style="text-align:right;">
46.7361111
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-23
</td>
<td style="text-align:right;">
30.9652778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-24
</td>
<td style="text-align:right;">
29.0104167
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-25
</td>
<td style="text-align:right;">
8.6527778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-26
</td>
<td style="text-align:right;">
23.5347222
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-27
</td>
<td style="text-align:right;">
35.1354167
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-28
</td>
<td style="text-align:right;">
39.7847222
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-29
</td>
<td style="text-align:right;">
17.4236111
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-30
</td>
<td style="text-align:right;">
34.0937500
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-31
</td>
<td style="text-align:right;">
53.5208333
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-01
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-02
</td>
<td style="text-align:right;">
36.8055556
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-03
</td>
<td style="text-align:right;">
36.7048611
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-04
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-05
</td>
<td style="text-align:right;">
36.2465278
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-06
</td>
<td style="text-align:right;">
28.9375000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-07
</td>
<td style="text-align:right;">
44.7326389
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-08
</td>
<td style="text-align:right;">
11.1770833
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-09
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-10
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-11
</td>
<td style="text-align:right;">
43.7777778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-12
</td>
<td style="text-align:right;">
37.3784722
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-13
</td>
<td style="text-align:right;">
25.4722222
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-14
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-15
</td>
<td style="text-align:right;">
0.1423611
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-16
</td>
<td style="text-align:right;">
18.8923611
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-17
</td>
<td style="text-align:right;">
49.7881944
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-18
</td>
<td style="text-align:right;">
52.4652778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-19
</td>
<td style="text-align:right;">
30.6979167
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-20
</td>
<td style="text-align:right;">
15.5277778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-21
</td>
<td style="text-align:right;">
44.3993056
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-22
</td>
<td style="text-align:right;">
70.9270833
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-23
</td>
<td style="text-align:right;">
73.5902778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-24
</td>
<td style="text-align:right;">
50.2708333
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-25
</td>
<td style="text-align:right;">
41.0902778
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-26
</td>
<td style="text-align:right;">
38.7569444
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-27
</td>
<td style="text-align:right;">
47.3819444
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-28
</td>
<td style="text-align:right;">
35.3576389
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-29
</td>
<td style="text-align:right;">
24.4687500
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-30
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>
#### Are there differences in activity patterns between weekdays and weekends?

``` r
actdata2<-actdata%>%
  mutate(day=weekdays(date), 
         daytype=ifelse(day=="Saturday" | day=="Sunday", "Weekend", "Weekday"))%>%
  group_by(interval, daytype)%>%
  summarise(average_steps=mean(steps))

plot3<-ggplot(data=actdata2, aes(x=interval, y=average_steps))+
  geom_line()+
  facet_wrap(~daytype)+
  ylab("Average number of steps per interval of time")+
  xlab("5 min interval")

ggsave("./figure/plot3.png", plot3)
```

    ## Saving 7 x 5 in image

``` r
plot(plot3)
```

![](PA1_template_files/figure-markdown_github/plot3.png)

From the plot above we can see that there are, in fact, differences between the activity patterns of the weekend and weekdays. The average is lower on a weekend, meaning they rest more.
