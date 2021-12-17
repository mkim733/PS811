setwd("~/Library/Mobile Documents/com~apple~CloudDocs")

install.packages("ggplot2")
library(ggplot2)

install.packages('pander')


## Opening the CSV ##

anes <- read.csv("anestotal.csv")

## Subsetting for presidential elections after 1980 ##

anes1 <- subset(anes, VCF0004 >= 1980 & VCF0004 <= 2016 )

head(anes1$VCF0211)

## Subsetting voters based on party affiliation ##

dem <- subset(anes1, VCF0803 <= 3)

dem1 <- subset(dem, VCF0229 <= 100 & VCF0656 < 999)

reb <- subset(anes1, VCF0803 >= 5)

reb1 <- subset(reb, VCF0229 <= 100 & VCF0656 < 999)

test <- lm(VCF0656 ~ VCF0229, dem1)


## finding the average support for environmentalist in each year post 1980 ##

## Creating a loop that finds the mean level of support for environmentalists for each year and for each party in each year ##

## Democrats ##

## Vector for presidential years 1980, 1988, 1990, 1994, 1996, 2000, 2002, 2004, 2008 ## 

y <- sort(unique(dem1$VCF0004))

y

year <- y[c(1:16)]

year

## Vector for mode support levels for years 1980, 1988, 1990, 1994, 1996, 2000, 2002, 2004, 2008 ## 

o <- rep(NA, length(year))

names(o) <- year

o

## Creating the loop ## 

m <- mean(dem1$VCF0229[dem1$VCF0004 == 2000])

m

for(i  in 1:length(year)){
  m1 <- mean(dem1$VCF0229[dem1$VCF0004 == year[i]])
  o[i] <- m1
}
o

## Republicans 


y2 <- sort(unique(reb1$VCF0004))

y2

year2 <- y2[c(1:16)]

year2

## Vector for mode support levels for years 1980, 1988, 1990, 1994, 1996, 2000, 2002, 2004, 2008 ## 

o1 <- rep(NA, length(year2))

names(o1) <- year2

o1

## Creating the loop ## 

m2 <- mean(reb1$VCF0229[reb1$VCF0004 == 2000])

m2

for(i  in 1:length(year2)){
  m3 <- mean(reb1$VCF0229[reb1$VCF0004 == year2[i]])
  o1[i] <- m3
}
o1


## Creating a loop that finds the mean level of trust for government for each year and for each party in each year ##

## Democrats ##

## Vector for presidential years 1980, 1988, 1990, 1994, 1996, 2000, 2002, 2004, 2008 ## 

y3 <- sort(unique(dem1$VCF0004))

y3

year3 <- y3[c(1:16)]

year3

## Vector for mode support levels for years 1980, 1988, 1990, 1994, 1996, 2000, 2002, 2004, 2008 ## 

o4 <- rep(NA, length(year3))

names(o4) <- year3

o4

## Creating the loop ## 

m4 <- mean(dem1$VCF0656[dem1$VCF0004 == 2008])

m4

for(i  in 1:length(year3)){
  m4 <- mean(dem1$VCF0656[dem1$VCF0004 == year[i]])
  o4[i] <- m4
}
o4

## Republicans 

y4 <- sort(unique(reb1$VCF0004))

y4

year4 <- y[c(1:16)]

year4

## Vector for mode support levels for years 1980, 1988, 1990, 1994, 1996, 2000, 2002, 2004, 2008 ## 

o5 <- rep(NA, length(year4))

names(o5) <- year4

o5

## Creating the loop ## 

m5 <- mean(reb1$VCF0656[reb1$VCF0004 == 2000])

m5

for(i  in 1:length(year4)){
  m5 <- mean(reb1$VCF0656[reb1$VCF0004 == year4[i]])
  o5[i] <- m5
}
o5

## Regression model that tracks changes in attitudes with environmentalists with changes in trust towards the government 

demplot <- lm(VCF0656 ~ VCF0229, dem1)

summary(demplot)

plot(dem1$VCF0229, dem1$VCF0656)


rebplot <- lm(VCF0656 ~ VCF0229, reb1)

summary(rebplot)

plot(reb1$VCF0229, reb1$VCF0656)

## Plotting the scatterplots ##



## Plotting these regression outputs with ggplot2 ##




## Plotting attitudes towards environmentalists and trust for government support over time ##

## Environment ##

demenviro <- data.frame(Year = c(1980, 1988, 1990, 1992, 1994, 1996, 2000, 2002, 2004, 2008),
                        Environmentalist.Thermo = c(78.39667, 79.67101, 83.57057, 74.04752, 78.40647, 71.98020, 66.75231, 70.87742, 74.38498, 74.23333))




rebenviro <- data.frame(Year = c(1980, 1988, 1990, 1992, 1994, 1996, 2000, 2002, 2004, 2008),
                        Environmentalist.Thermo = c(74.39575, 75.73929, 76.28183, 66.72915, 67.35143, 60.93642, 61.43216, 58.91452, 62.83784, 64.63821))

devenviroplot <- ggplot(demenviro, aes(x = Year, y = Environmentalist.Thermo)) +
geom_path() +
  geom_point(size = 0.5) +
  ggtitle("Figure 1") +
  xlim(1979, 2009) +
  ylim(0, 90) +
  xlab("Year") +
  ylab("Thermometer Index") +
  theme(legend.position="bottom")
ggsave("demenviroplot.pdf", width = 5, height = 5)

rebenviroplot <- ggplot(rebenviro, aes(x = Year, y = Environmentalist.Thermo)) +
  geom_path() +
  geom_point(size = 0.5) +
  ggtitle("Figure 2") +
  xlim(1979, 2009) +
  ylim(0, 90) +
  xlab("Year") +
  ylab("Thermometer Index") +
  theme(legend.position="bottom")
ggsave("rebenviroplot.pdf", width = 5, height = 5)

## Government ##

demgov <- data.frame(Year = c(1980, 1988, 1990, 1992, 1994, 1996, 2000, 2002, 2004, 2008),
                     Gov.Trust.Average = c(30.51667, 31.99023, 28.24024, 27.55508, 28.19065, 35.33663, 35.72662, 40.60968, 31.69014, 25.01778))

rebgov <- data.frame(Year = c(1980, 1988, 1990, 1992, 1994, 1996, 2000, 2002, 2004, 2008),
                  Gov.Trust.Average = c(25.93320, 34.6238, 29.34504, 28.77363, 25.71964, 30.54335, 34.41709, 43.64993, 38.93919, 27.13609))

demgovplot <- ggplot(demgov, aes(x = Year, y = Gov.Trust.Average)) +
  geom_path() +
  geom_point(size = 0.5) +
  ggtitle("Figure 3") +
  xlim(1979, 2009) +
  ylim(0, 50) +
  xlab("Year") +
  ylab("Trust In Government Index") +
  theme(legend.position="bottom")
ggsave("demgovplot.pdf", width = 5, height = 5)

rebgovplot <- ggplot(rebgov, aes(x = Year, y = Gov.Trust.Average)) +
  geom_path() +
  geom_point(size = 0.5) +
  ggtitle("Figure 4") +
  xlim(1979, 2009) +
  ylim(0, 50) +
  xlab("Year") +
  ylab("Trust In Government Index") +
  theme(legend.position="bottom")
ggsave("rebgovplot.pdf", width = 5, height = 5)

## Regression models ##

demplot <- lm(VCF0656 ~ VCF0229, dem1)

summary(demplot)

table(dem1$VCF0229)

table(dem1$VCF0656)

demplot1 <- ggplot(dem1, aes(x = VCF0229, y = VCF0656, color = VCF0004)) +
  geom_point() +
  geom_abline(intercept = 32.27585, slope = -0.00622, linetype  = "solid", colour = "black", size = 1) +
  ggtitle("Figure 5") +
  xlim(0, 110) +
  ylim(0, 110) +
  xlab("Enviromentalist Thermometer") +
  ylab("Trust In Government Index") +
  theme(legend.position="bottom")
ggsave("demplot1.pdf", width = 10, height = 10)


rebplot <- lm(VCF0656 ~ VCF0229, reb1)

summary(rebplot)

rebplot1 <- ggplot(reb1, aes(x = VCF0229, y = VCF0656, color = VCF0004)) +
  geom_point() +
  geom_abline(intercept = 27.02142, slope = 0.05740, linetype  = "solid", colour = "black", size = 1) +
  ggtitle("Figure 6") +
  xlim(0, 110) +
  ylim(0, 110) +
  xlab("Enviromentalist Thermometer") +
  ylab("Trust In Government Index") +
  theme(legend.position="bottom")
ggsave("rebplot1.pdf", width = 10, height = 10)


