setwd("~/Library/Mobile Documents/com~apple~CloudDocs")

## Thank you Dillon for assistance with question 06 for part 1 in base R, and to Veronica for assistance with several sections in part 1 and in Tidyverse for part 2. ##

library(tidyverse)
library(scales)

## Base R Tasks ##

## Question 01 ##

food <- read.csv("food_coded.csv")

## Question 02 ##

view(food)

## Question 03 ##

food1  <- food[1:95,]

view(food1)

## Question 04 ##

food2 <- food1[  , c("GPA", "calories_chicken", "drink", "fav_cuisine", "father_profession", "mother_profession")]
view(food2)  

## Question 05 ##

food$healthy_feeling2 <- rescale(food$healthy_feeling, to = c(1,100))

## Question 06 ##


food$GPAnew <- as.numeric(as.character(food$GPA))

food[74, 63] <- 3.79

GPAfilter <- subset(food, Gender == "1" & GPAnew > "3")

view(GPAfilter)


## Question 07 ##

calories <- food[  , c("calories_chicken", "tortilla_calories", "turkey_calories", "waffle_calories")]

view(calories)  

calories <- food[  , c("calories_chicken", "tortilla_calories", "turkey_calories", "waffle_calories")]

calories$chickenM <- mean(calories$calories_chicken)
calories$tortillaM <- mean(calories$tortilla_calories)
calories$turkeyM <- mean(calories$turkey_calories)
calories$waffleM <- mean(calories$waffle_calories)

calories$chickenSD <- sd(calories$calories_chicken)
calories$tortillaSD <- sd(calories$tortilla_calories)
calories$turkeySD <- sd(calories$turkey_calories)
calories$waffleSD <- sd(calories$waffle_calories)

head(calories)

  
## Question 08 ##

food$weight <- as.numeric((food$weight))

food$weight[4] <- 240

food$weight[68] <- 144

female <- subset(food, Gender == 2)
male <- subset(food, Gender == 1)

## Mean of GPA (femaleG) and Weight (W) in cuisine variable ## 

femaleG <- tapply(female$GPAnew, female$cuisine, mean, na.rm = T)
  
maleG <- tapply(male$GPAnew, male$cuisine, mean, na.rm = T)

femaleW <- tapply(female$GPAnew, female$weight, mean, na.rm = T)

maleW <- tapply(male$GPAnew, male$weight, mean, na.rm = T)
  
## Standard Deviation of GPA (femaleG) and Weight (W) in cuisine variable ## 

femaleGSD <- tapply(female$GPAnew, female$cuisine, sd, na.rm = T)

maleGSD <- tapply(male$GPAnew, male$cuisine, sd, na.rm = T)

femaleWSD <- tapply(female$GPAnew, female$weight, sd, na.rm = T)

maleWSD <- tapply(male$GPAnew, male$weight, sd, na.rm = T)

## Tidyverse Tasks ##

## Question 01 ##

facebook <- read.csv("facebook-fact-check.csv")

## Question 02 ##

facebook <- read.csv("facebook-fact-check.csv")

## Question 03 ## 

facebook %>% top_n(-500)
facebook1 <- top_n(facebook, -500)

## Question 04 ##

facebook %>% select(2,4,6,8,10,12)

## Question 05 ##

facebook2 <-
  facebook %>% mutate(post_type_coded = case_when(
    facebook$Post.Type == "link" ~ 1,
    facebook$Post.Type == "photo" ~ 2,
    facebook$Post.Type == "text" ~ 3,
    facebook$Post.Type == "video" ~ 4,
  ))
head(facebook2)

## Question 06 ##

facebook %>% arrange(desc(Page))

## Question 07 ##

facebook %>% 
  summarise(across(c(share_count, reaction_count, comment_count), list(sd = sd, mean = mean), na.rm = T))


## Question 08 ##

facebook %>%
  filter(Category == "mainstream") %>% summarise(across(c(share_count, reaction_count, comment_count), list(sd = sd, mean = mean), na.rm = T))
