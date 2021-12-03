setwd("~/Library/Mobile Documents/com~apple~CloudDocs")

## Exercise 7 ##

## Matthew Kim ##

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")   
library("dplyr")


## 1. Check out the base R built-in dataset, data("USArrests"). ##

data("USArrests")

pull1 <- pull(USArrests, Murder)
pull2 <- pull(USArrests, Assault)
pull3 <- pull(USArrests, UrbanPop)
pull4 <- pull(USArrests, Rape)

arrests <- data.frame(State = c("AL", "AK", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM","NY","NC","ND","OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT",  "VT","VA", "WA", "WI", "WV", "WY"),
                      Murder = c(13.2, 10.0, 8.1, 8.8, 9.0, 7.9, 3.3, 5.9, 15.4, 17.4, 5.3, 2.6, 10.4, 7.2, 2.2, 6.0, 9.7, 15.4, 2.1, 11.3, 4.4, 12.1, 2.7, 16.1, 9.0, 6.0, 4.3, 12.2, 2.1, 7.4, 11.4, 11.1, 13.0,  0.8,  7.3,  6.6,  4.9, 6.3,  3.4, 14.4, 3.8, 13.2, 12.7,  3.2,  2.2,  8.5,  4.0,  5.7,  2.6,  6.8),
                      Assault = c(pull2),
                      UrbanPop = c(pull3),
                      Rape = c(pull4)
)
print(arrests)


## 2. Create a scatterplot that looks at the correlation between murder and assault arrests. Label the x and y axes and title the graph. ##

regression <- lm(Assault ~ Murder, arrests)
summary(regression)

plot(arrests$Murder, arrests$Assault)

plot1 <- ggplot(arrests, aes(x = Murder, y = Assault)) +
geom_point() +
  geom_abline(intercept = 51.27, slope = 15.34, linetype  = "solid", color = "black", size = 1) +
  ggtitle("Correlation between murder and assult arrests") +
  xlim(0, 20) +
  ylim(0, 400) +
  xlab("Murder") +
  ylab("Assault") +
  theme(legend.position="bottom")
ggsave("scatterplot.pdf", width = 30, height = 15)

## 3. Create a boxplot of rape arrests. Label the plot. ##

ggplot(arrests) + 
  geom_boxplot(aes(y = Rape)) + 
  ylim(c(0, 40)) +
  labs(title = "Rape Arrests",
       y = "Percentage of Rapes")
ggsave("boxplot.pdf", width = 30, height = 15)

mean(arrests$Rape)

## 4. Create a barplot of the number of rape arrests per state. ##

plot3 <- ggplot(arrests, aes(x = State, y = Rape)) +
geom_bar(stat = "identity") +
ylim(c(0, 50))
ggsave("barplot.pdf", width = 30, height = 15)

mean(arrests$Rape)

## 5. Create a histogram for the percent of urban population. ##
 
plot4 <- ggplot(arrests, aes(x=UrbanPop)) +
geom_histogram(bins = 40) +
  xlim(0, 100) +
  ylim(0, 10) 
ggsave("histogram.pdf", width = 30, height = 15)




