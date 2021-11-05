setwd("~/Library/Mobile Documents/com~apple~CloudDocs")

library(tidyverse)

## Thank you Veronica for assistance with some of the functions and with formulating the loops for part 2.

## Exercise 5 ##

## Question 01 ##

national <- read.csv("national.csv")

## Question 01 ##

## Function for the mean ##

M <- function(x) {
  y <- (sum(x))/length(x)
  return(y)
  
}

M(national$christianity_protestant)

## Function for the standard deviation ##


SD <- function(x) {
  y <- sqrt(sum((x - M(x))^2) / (length(x) - 1))
  return(y)
}

sd(national$christianity_protestant) 

SD(national$christianity_protestant)

## Function for the median ## 

MD <- function(lst) {
  a <- length(lst)
  b <- sort(lst)
  y <- ifelse(a%%2==1,b[(a+1)/2], M(b[n/2+0:1]))
  return(y)
}

MD(national$christianity_protestant)


## Question 02 ##

M_exclude <- function(x) {
  x1 <- sort(x)
  y1 <- x1[-c(1, length(x1))]
  y2 <- M(x1)
  return(y2)
}

## Question 03 ##

## Mean ##

M(national$christianity_protestant) 
M(national$christianity_romancatholic)
M(national$christianity_easternorthodox)
M(national$christianity_anglican)
M(national$christianity_other)
M(national$christianity_all)

## Standard Deviation ##

SD(national$christianity_protestant) 
SD(national$christianity_romancatholic)
SD(national$christianity_easternorthodox)
SD(national$christianity_anglican)
SD(national$christianity_other)
SD(national$christianity_all)

## Median ## 


MD(national$christianity_protestant) 
MD(national$christianity_romancatholic)
MD(national$christianity_easternorthodox)
MD(national$christianity_anglican)
MD(national$christianity_other)
MD(national$christianity_all)


## Mean that excludes values ##

M_exclude(national$christianity_protestant) 
M_exclude(national$christianity_romancatholic)
M_exclude(national$christianity_easternorthodox)
M_exclude(national$christianity_anglican)
M_exclude(national$christianity_other)
M_exclude(national$christianity_all)


## Loops ##

## Question 01 ##

LV <- rep(0, ncol(national))
for (i in 1:length(national)) {
  LV[i] <- ncol(national[i, ])
}

LV

## Question 02 ##

## Function ##

MP <- data.frame(state = unique(national$state, prots = NA))
                 
for (i in 1:length(MP$state)) {
  state1 <- MP[i,1]
  y <- national %>% filter(state == state1) %>%
    summarise(mean = mean(christianity_protestant, na.rm = TRUE)) %>%
    pull(mean)
  MP[i, 2] <- y
  
}

## tapply function ##

tapply(national$christianity_protestant, national$state, mean, na.rm = TRUE)

## Question 03 ##

sapply(national,class)
