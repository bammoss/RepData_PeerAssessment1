---
title: "Assignement1"
author: "Brad Moss"
date: "November 15, 2015"
output: html_document
---

This next bit of code will upload the document and modify the format of one column.


```r
act <- read.csv("activity.csv")
act[,2]<-as.Date(act[,2],"%Y-%m-%d")
```

Now on to completing the first question.


```r
cat1 <- aggregate(act$steps,list(act$date),sum, na.rm=TRUE)
hist(cat1$x, main="Total Steps per Day", xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 
Now getting the mean and median

```r
mean(cat1$x)
```

```
## [1] 9354.23
```

```r
median(cat1$x)
```

```
## [1] 10395
```

What is the average daily pattern?

```r
dog1 <- aggregate(act$steps, list(act$interval), mean, na.rm=TRUE)
plot(dog1[,1],dog1[,2], type="l",xlab="Interval", ylab="Average Number of Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
d <- dog1[dog1[,2]==max(dog1[,2]),1]
```
The maximum number of steps is 835.
Now I am going to create a new set of data with the missing values
filled in.  I used the mean for each day.
First let's find the number of missing values.

```r
sum(is.na(act$steps))
```

```
## [1] 2304
```
Above you should see the number of missing values.
Now let's fill in the missing values

```r
dog2 <- aggregate(act$steps, list(act$date), mean, na.rm=TRUE)
dog2[is.na(dog2[,2]),2]<-c(0,0,0,0,0,0,0,0)
for(i in 1:nrow(act)){
	if(is.na(act[i,1])){
		act[i,1] <- dog2[act[i,2]==dog2[,1],2]	
	}
}
```
On line 2, it turned out the dog2 still had missing values, so I replaced them with zeros.  
Those missing values in dog2 corresponded to days that had all missing values.
Now for the graph.

```r
cat2 <- aggregate(act$steps,list(act$date),sum, na.rm=TRUE)
hist(cat2$x, main="Total Steps per Day", xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 
Let's look at the mean and median

```r
mean(cat2$x)
```

```
## [1] 9354.23
```

```r
median(cat2$x)
```

```
## [1] 10395
```
As you can see, the computations did not change.  It is because all the missing values coresponded to the 0's
in dog2.
I did not complete the last question because of problems I faced in using aggregate and other functions

```r
weekend<-function(date){
	if((weekdays(date)=="Saturday")|(weekdays(date)=="Sunday")){
		blah<-"Weekend"
	}else{
		blah<-"Weekday"
	}
	return(blah)
}
act$TimeOfWeek <- lapply(act$date, weekend)
```
It wouldn't let me aggregate grouping by TimeOfWeek and interval.
