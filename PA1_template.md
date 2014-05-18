# Reproducible Research: Peer Assessment 1


```r
## Loading and preprocessing the data


datos <- read.csv("activity.csv")


# sum of the activity per day
resum <- datos[0, 1:2]
ini <- 1
for (i in 1:61) {
    resum[i, 1] <- (sum(na.omit(datos[ini:(ini + 287), 1])))
    ini <- ini + 288
    resum[i, 2] <- datos[ini, 2]
}
```

## What is mean total number of steps taken per day?


```r



hist(resum[[1]])
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

```r

mean(resum[[1]])
```

```
## [1] 9354
```

```r
median(resum[[1]])
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r



resumday <- rep(0, 288)
ini <- 1
for (i in 1:61) {
    dp <- datos[ini:(ini + 287), 1]
    dp[is.na(dp)] <- 0
    resumday <- resumday + dp
    ini <- ini + 288
    
}
resumday <- resumday/61

plot(resumday, type = "l")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r

max(resumday)
```

```
## [1] 179.1
```

## Imputing missing values

```r



sum(is.na(datos[, 1]))
```

```
## [1] 2304
```

```r
# 2304 , there are not na in the rest of attributes

# we change Na for the eq value of resumday
newdatos <- datos
for (i in 1:61) {
    for (j in 1:288) {
        if (is.na(newdatos[((i - 1) * 61 + j), 1])) 
            newdatos[(i - 1) * 61 + j, 1] <- resumday[j]
    }
}


# new histograms
newresum <- newdatos[0, 1:2]
ini <- 1
for (i in 1:61) {
    newresum[i, 1] <- (sum(na.omit(newdatos[ini:(ini + 287), 1])))
    ini <- ini + 288
    newresum[i, 2] <- newdatos[ini, 2]
}


## What is mean total number of steps taken per day?

hist(newresum[[1]])
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r

mean(newresum[[1]])
```

```
## [1] 9580
```

```r

median(newresum[[1]])
```

```
## [1] 10395
```

```r

```



## Are there differences in activity patterns between weekdays and weekends?
