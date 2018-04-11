---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(lattice)
d <- read.csv("activity.csv")
d$date <- as.Date(d$date)
b <-d[complete.cases(d),] #removing NA's
```
## Mean total number of steps taken per day

```r
da<-b[b$interval == 5,"date"] #Extracting the dates
nosteps<-sapply(b[b$date == da,"steps"], function(x){ sum(x)}) # calculating total number of steps per day
hist(nosteps,xlab = "No Of Steps",main = "Histogram of the No Of steps" ,col ="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
print("The total  number of steps")
```

```
## [1] "The total  number of steps"
```

```r
print(nosteps)
```

```
##   [1]   0   0   0   0   0   0   0  56   0 154   0   0   7 145   0 114   0
##  [18]   0   8 555   0   0   0   0   0  64   0   0   0  62  12  13  61   0
##  [35]  28   0   0   0   0   0  32   0   0   8   0  22   0  27   0   0   0
##  [52]   0   0  41   0   0   7   0   0   0   0   0  65 120   0   0   0 757
##  [69]   0   4   0   0  57   0   0   0   8   0   0   0  60   0   0   0 193
##  [86]   0   0   0   0   0   0   0   0  51   0  21   0   0   0  16  35 260
## [103]  12   0   0  18   0   0  62   0  96 156   0   0   0   0   0 274 410
## [120]   0   0   0   0   0   0   0   0  17  31   0   0   0 519  16   0   0
## [137]   0   0  86   0   0   0   0   0   0  59   0   0  28   0   0 272   0
## [154]   0 206   0 140   0   0 377   0   0   0   0   0   0   0   0   0   0
## [171] 785   0   0   0   0   0   0 118   0   0   0 483   0   0  12   0   0
## [188]   0   0   0   0   0  39   0   0   0   0 536   0   0   0   0  79   0
## [205] 347   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0  70
## [222]  43   0   0 152 321  13   0   0 652   0  19   0   0   0   0   0  58
## [239]   0   0  37   0   0   0   0   0   0   0 484   0   0   0 505  49   0
## [256]   0   0   0 122   0   0   0  49  33  65   0   0   0 135   0   0   0
## [273]  63   0 198  34   0   0  71   0   0   0   0   0   0   0  66   0
```

```r
print("The mean")
```

```
## [1] "The mean"
```

```r
print(sapply(b[b$date == da,"steps"], function(x){ mean(x)}))
```

```
##   [1]   0   0   0   0   0   0   0  56   0 154   0   0   7 145   0 114   0
##  [18]   0   8 555   0   0   0   0   0  64   0   0   0  62  12  13  61   0
##  [35]  28   0   0   0   0   0  32   0   0   8   0  22   0  27   0   0   0
##  [52]   0   0  41   0   0   7   0   0   0   0   0  65 120   0   0   0 757
##  [69]   0   4   0   0  57   0   0   0   8   0   0   0  60   0   0   0 193
##  [86]   0   0   0   0   0   0   0   0  51   0  21   0   0   0  16  35 260
## [103]  12   0   0  18   0   0  62   0  96 156   0   0   0   0   0 274 410
## [120]   0   0   0   0   0   0   0   0  17  31   0   0   0 519  16   0   0
## [137]   0   0  86   0   0   0   0   0   0  59   0   0  28   0   0 272   0
## [154]   0 206   0 140   0   0 377   0   0   0   0   0   0   0   0   0   0
## [171] 785   0   0   0   0   0   0 118   0   0   0 483   0   0  12   0   0
## [188]   0   0   0   0   0  39   0   0   0   0 536   0   0   0   0  79   0
## [205] 347   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0  70
## [222]  43   0   0 152 321  13   0   0 652   0  19   0   0   0   0   0  58
## [239]   0   0  37   0   0   0   0   0   0   0 484   0   0   0 505  49   0
## [256]   0   0   0 122   0   0   0  49  33  65   0   0   0 135   0   0   0
## [273]  63   0 198  34   0   0  71   0   0   0   0   0   0   0  66   0
```

```r
print("The median")
```

```
## [1] "The median"
```

```r
print(sapply(b[b$date == da,"steps"], function(x){ median(x)}))
```

```
##   [1]   0   0   0   0   0   0   0  56   0 154   0   0   7 145   0 114   0
##  [18]   0   8 555   0   0   0   0   0  64   0   0   0  62  12  13  61   0
##  [35]  28   0   0   0   0   0  32   0   0   8   0  22   0  27   0   0   0
##  [52]   0   0  41   0   0   7   0   0   0   0   0  65 120   0   0   0 757
##  [69]   0   4   0   0  57   0   0   0   8   0   0   0  60   0   0   0 193
##  [86]   0   0   0   0   0   0   0   0  51   0  21   0   0   0  16  35 260
## [103]  12   0   0  18   0   0  62   0  96 156   0   0   0   0   0 274 410
## [120]   0   0   0   0   0   0   0   0  17  31   0   0   0 519  16   0   0
## [137]   0   0  86   0   0   0   0   0   0  59   0   0  28   0   0 272   0
## [154]   0 206   0 140   0   0 377   0   0   0   0   0   0   0   0   0   0
## [171] 785   0   0   0   0   0   0 118   0   0   0 483   0   0  12   0   0
## [188]   0   0   0   0   0  39   0   0   0   0 536   0   0   0   0  79   0
## [205] 347   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0  70
## [222]  43   0   0 152 321  13   0   0 652   0  19   0   0   0   0   0  58
## [239]   0   0  37   0   0   0   0   0   0   0 484   0   0   0 505  49   0
## [256]   0   0   0 122   0   0   0  49  33  65   0   0   0 135   0   0   0
## [273]  63   0 198  34   0   0  71   0   0   0   0   0   0   0  66   0
```
## The average daily activity pattern

```r
i<- b[b$date == da[1],"interval"] #extracting the intervals
intsteps<-sapply(b[b$interval == i,"steps"], function(x){ mean(x)}) #Average No Of steps per interval
timeseries <- data.frame(intervals = i,avg_steps =as.ts(intsteps)) # data frame for the plot
 plot(timeseries,type ="l",xlab = "interval",ylab = "Average number of steps",main = "Time series plot") # the plot
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
 maxsteps<-sapply(b[b$interval == i,"steps"], function(x){ sum(x)}) # Calculating total number of steps per interval
 max1<- data.frame(interval = i ,total_steps = maxsteps)
 print("The interval with maximum number of steps is")
```

```
## [1] "The interval with maximum number of steps is"
```

```r
 max1[max1$total_steps == max(maxsteps),"interval"]
```

```
## [1] 615
```
## Imputing missing values

```r
com<-complete.cases(d)
print("The No Of NA's")
```

```
## [1] "The No Of NA's"
```

```r
length(com[com == FALSE])
```

```
## [1] 2304
```

```r
meansteps <-sapply(b[b$date == da,"steps"], function(x){ mean(x)}) # calculating mean steps
dc <- d[!complete.cases(d),]
mstps <- data.frame(vl = i ,mteps = meansteps)
for (k in i) {
  dc[dc$interval == k,"steps"] = mstps[mstps$vl == k,"mteps"] #imputing missing values with mean values
}
d[!complete.cases(d),"steps"] <- dc[,"steps"]# transfered to the original data frame
da<-d[d$interval == 5,"date"] #Extracting the dates
nosteps<-sapply(d[d$date == da,"steps"], function(x){ sum(x)}) # calculating total number of steps per day
hist(nosteps,xlab = "No Of Steps",main = "Histogram of the No Of steps" ,col ="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
print("The total  number of steps")
```

```
## [1] "The total  number of steps"
```

```r
print(nosteps)
```

```
##   [1]   0   0   0   0   0   0   0   0   0   0   0  53   0   0   0   0 145
##  [18]  20   0   0  36   0   0   0   0 170  10   6   0   0  23   0  31   0
##  [35]   0   0   0 484   0  63 211   7   0   0   0   0  63   0   0   0   0
##  [52]   0   0  38   0   0   0   0   0   9  43   0   0   0  17   0   0   0
##  [69]   0  19  22   0  38  16  20   0   0 266   0 479   0   0   0   0   0
##  [86]   0 149   0 168   0   0   0   0   0   0   0   0 188   0   0   0  96
## [103]   0   0   0   7   0   0   0   0   0 159  46   0   0   0   0   0   0
## [120]   0   0   0   0   0 505   0  60   0   0   0   0  22   0   0   0   0
## [137]   0   0  28  17  17   0   0   0  83   0 154   0   0  39  49   0   0
## [154]  49   0   0   0 509  66  71   0 120   0   0   0   0   0   0   0   0
## [171]   0   0 294  70  38   0  44   0  27   0   0   0  45  13  13   0   0
## [188]   0  34   0  96   0   0   0   0   0  19   0   0  20   0   0   0   0
## [205]  16   0  57   0   0   0   0   0   0   0   0   0   0   0   0   0   0
## [222]   0   0   0   0  55   0   0  30 725   0   0 756   7   0   0   0 500
## [239]   0  49   0   0   0  51   0   0  36 141  42   0   0   0 174 446   0
## [256]   0   0 360   0   0   0 319   0   0   0  64   0  29   0   0  21 198
## [273]  31   0  23   0   0   0   0   0   0   0   0   8   0   0  13   0
```

```r
print("The mean")
```

```
## [1] "The mean"
```

```r
print(sapply(d[d$date == da,"steps"], function(x){ mean(x)}))
```

```
##   [1]   0   0   0   0   0   0   0   0   0   0   0  53   0   0   0   0 145
##  [18]  20   0   0  36   0   0   0   0 170  10   6   0   0  23   0  31   0
##  [35]   0   0   0 484   0  63 211   7   0   0   0   0  63   0   0   0   0
##  [52]   0   0  38   0   0   0   0   0   9  43   0   0   0  17   0   0   0
##  [69]   0  19  22   0  38  16  20   0   0 266   0 479   0   0   0   0   0
##  [86]   0 149   0 168   0   0   0   0   0   0   0   0 188   0   0   0  96
## [103]   0   0   0   7   0   0   0   0   0 159  46   0   0   0   0   0   0
## [120]   0   0   0   0   0 505   0  60   0   0   0   0  22   0   0   0   0
## [137]   0   0  28  17  17   0   0   0  83   0 154   0   0  39  49   0   0
## [154]  49   0   0   0 509  66  71   0 120   0   0   0   0   0   0   0   0
## [171]   0   0 294  70  38   0  44   0  27   0   0   0  45  13  13   0   0
## [188]   0  34   0  96   0   0   0   0   0  19   0   0  20   0   0   0   0
## [205]  16   0  57   0   0   0   0   0   0   0   0   0   0   0   0   0   0
## [222]   0   0   0   0  55   0   0  30 725   0   0 756   7   0   0   0 500
## [239]   0  49   0   0   0  51   0   0  36 141  42   0   0   0 174 446   0
## [256]   0   0 360   0   0   0 319   0   0   0  64   0  29   0   0  21 198
## [273]  31   0  23   0   0   0   0   0   0   0   0   8   0   0  13   0
```

```r
print("The median")
```

```
## [1] "The median"
```

```r
print(sapply(d[d$date == da,"steps"], function(x){ median(x)}))
```

```
##   [1]   0   0   0   0   0   0   0   0   0   0   0  53   0   0   0   0 145
##  [18]  20   0   0  36   0   0   0   0 170  10   6   0   0  23   0  31   0
##  [35]   0   0   0 484   0  63 211   7   0   0   0   0  63   0   0   0   0
##  [52]   0   0  38   0   0   0   0   0   9  43   0   0   0  17   0   0   0
##  [69]   0  19  22   0  38  16  20   0   0 266   0 479   0   0   0   0   0
##  [86]   0 149   0 168   0   0   0   0   0   0   0   0 188   0   0   0  96
## [103]   0   0   0   7   0   0   0   0   0 159  46   0   0   0   0   0   0
## [120]   0   0   0   0   0 505   0  60   0   0   0   0  22   0   0   0   0
## [137]   0   0  28  17  17   0   0   0  83   0 154   0   0  39  49   0   0
## [154]  49   0   0   0 509  66  71   0 120   0   0   0   0   0   0   0   0
## [171]   0   0 294  70  38   0  44   0  27   0   0   0  45  13  13   0   0
## [188]   0  34   0  96   0   0   0   0   0  19   0   0  20   0   0   0   0
## [205]  16   0  57   0   0   0   0   0   0   0   0   0   0   0   0   0   0
## [222]   0   0   0   0  55   0   0  30 725   0   0 756   7   0   0   0 500
## [239]   0  49   0   0   0  51   0   0  36 141  42   0   0   0 174 446   0
## [256]   0   0 360   0   0   0 319   0   0   0  64   0  29   0   0  21 198
## [273]  31   0  23   0   0   0   0   0   0   0   0   8   0   0  13   0
```
## Activity patterns in the weekends and weekdays

```r
w<-weekdays(d$date)
w[w == "Saturday"] <-"weekend"
w[w == "Sunday"] <-"weekend"
w[!w == "weekend"] <-"weekday"
w<-factor(w)
dat<-data.frame(steps=d$steps,date =d$date,interval =d$interval,days =w)#NEW data frame with the weekends and weekdays classification
weekd<- dat[dat$days=="weekday",]
weeke<-dat[dat$days=="weekend",]
wkdsteps<-sapply(weekd[weekd$interval == i,"steps"], function(x){ mean(x)})#mean steps for weekday
wkesteps<-sapply(weeke[weeke$interval == i,"steps"], function(x){ mean(x)})#mean steps for weekend
wf <- c(rep("weekday",length(wkdsteps)),rep("weekend",length(wkesteps)))
wtimes<-data.frame(interval<-d$interval,steps <- as.ts(c(wkdsteps,wkesteps)),days <-factor(wf))
 xyplot(steps~interval|days,wtimes,type ="l",layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
