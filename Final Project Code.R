library(tidyverse) #load packages
library(ggthemes)
library(huxtable)

getwd() #this checks my working directory, which is: "/Users/sarahdriver"
experiment <- read.csv(file = "./Documents/ECON_650/Project/Data_for_final_project.csv") 
SCENP0 <- read.csv(file = "./Documents/ECON_650/Project/project_SCEN-P-0-table.csv") 

#General Data Exploration 
class (experiment) #is a data frame 
names (experiment) #lists variables
summary(experiment$count.cows)
summary(experiment$count.patches.with..pcolor...green.)
summary(experiment$X.step.)
sd(experiment$count.cows, na.rm=TRUE)
sd(experiment$count.patches.with..pcolor...green., na.rm=TRUE)
experiment %>% count(count.cows, na.rm=TRUE)
experiment %>% count(count.patches.with..pcolor...green., na.rm=TRUE)
experiment %>% count(feasted, na.rm=TRUE)

#For Data Exploration by Scenario
#Made some new filtered data
summaryofSc1 <- experiment %>% filter(Scenario_1 == 1) 
summaryofSc2 <- experiment %>% filter(Scenario_2 == 1) 
summaryofSc3 <- experiment %>% filter(Scenario_3 == 1) 

#Scenario 1
summary(summaryofSc1$count.cows, na.rm=TRUE)
sd(summaryofSc1$count.cows, na.rm=TRUE)
summary(summaryofSc1$count.patches.with..pcolor...green., na.rm=TRUE)
sd(summaryofSc1$count.patches.with..pcolor...green., na.rm=TRUE)

#Scenario 2
summary(summaryofSc2$count.cows, na.rm=TRUE)
sd(summaryofSc2$count.cows, na.rm=TRUE)
summary(summaryofSc2$count.patches.with..pcolor...green., na.rm=TRUE)
sd(summaryofSc2$count.patches.with..pcolor...green., na.rm=TRUE)

#Scenario 3
summary(summaryofSc3$count.cows, na.rm=TRUE)
sd(summaryofSc3$count.cows, na.rm=TRUE)
summary(summaryofSc3$count.patches.with..pcolor...green., na.rm=TRUE)
sd(summaryofSc3$count.patches.with..pcolor...green., na.rm=TRUE)

#Adding duplicate columns (some just in case I want to safely mess up)
experiment <- experiment %>% 
  mutate(Cow_Existence = count.cows)

experiment <- experiment %>% 
  mutate(Cow_Existence_Divided = count.cows)

experiment <- experiment %>% 
  mutate(Meadow_Existence = count.patches.with..pcolor...green.)

experiment <- experiment %>% 
  mutate(fairness_to_others_as_categories = fairness.to.others)

experiment <- experiment %>% 
  mutate(positive_reciprocity_as_categories = positive.reciprocity)

experiment <- experiment %>% 
  mutate(risk_aversion_as_categories = risk.aversion)

#This command finds the 0 step that has less than 120 cows--this implies that herders with friends ate some of the cows at the start
experiment <- experiment %>% 
  mutate(feasted = ifelse(X.step. == 0, ifelse(count.cows < 120, 1, 0), 0))

#This separates by scenarios 
experiment <- experiment %>% 
  mutate(Scenario_1 = ifelse(fairness.to.others == 0.4, ifelse(positive.reciprocity == 0.5, 1, 0), 0))
experiment <- experiment %>% 
  mutate(Scenario_1 = ifelse(risk.aversion == 0.8, 1, 0))

experiment <- experiment %>% 
  mutate(Scenario_2 = ifelse(fairness.to.others == 0.58, ifelse(positive.reciprocity == 0.68, 1, 0), 0))
experiment <- experiment %>% 
  mutate(Scenario_2 = ifelse(risk.aversion == 0.75, 1, 0))

experiment <- experiment %>% 
  mutate(Scenario_3 = ifelse(fairness.to.others == 0.8, ifelse(positive.reciprocity == 0.85, 1, 0), 0))
experiment <- experiment %>% 
  mutate(Scenario_3 = ifelse(risk.aversion == 0.7, 1, 0))

#Transforming into dummy variables
experiment <- experiment %>%
  dplyr::mutate(Cow_Existence = ifelse(Cow_Existence > 0, 1, 0)) #not going to use this probably 

experiment <- experiment %>%
  dplyr::mutate(Meadow_Existence = ifelse(Meadow_Existence > 0, 1, 0))

experiment <- experiment %>%
  dplyr::mutate(Cow_Existence_Divided = ifelse(count.cows > 4, 1, 0))

#Histogram of Cow Existence
#Max number of cows: 236
Image1 <- ggplot(data = experiment, 
                 mapping = aes(x = count.cows)) +
  geom_histogram(fill = "lightblue", color = "gray", bins = 23) +
  labs(title = "Cow Existence", subtitle = "Histogram") +
  xlab(label = "Cows") +
  ylab(label = "Count") +
  theme(
    panel.background = element_rect(fill = "white"), # it automatically went to gray so I had to change it to the correct color 
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor.x = element_line(colour = "lightgray", linetype = "solid", size = 0.2),
    panel.grid.minor.y = element_line(colour = "lightgray", linetype = "solid", size = 0.2))
Image1 #take a look

#Histogram of Meadow Existence
#Max number of green patches: 1089
Image2 <- ggplot(data = experiment, 
                 mapping = aes(x = count.patches.with..pcolor...green.)) +
  geom_histogram(fill = "lightgreen", color = "gray", bins = 108) +
  labs(title = "Meadow Existence", subtitle = "Histogram") +
  xlab(label = "Grass") +
  ylab(label = "Count") +
  theme(
    panel.background = element_rect(fill = "white"), # it automatically went to gray so I had to change it to the correct color 
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor.x = element_line(colour = "lightgray", linetype = "solid", size = 0.2),
    panel.grid.minor.y = element_line(colour = "lightgray", linetype = "solid", size = 0.2))
Image2 #take a look

#Bar Graph of Meadows
Image3 <- ggplot(data = experiment) +
  geom_bar(fill = "lightgreen", color = "gray", mapping = aes(x = Meadow_Existence)) +
  xlab(label = "Meadow Existence") +
  ylab(label = "Count") +
  labs(title = "Meadow Existence by Dummy Variable", subtitle = "Bar Graph") +
  theme(
    panel.background = element_rect(fill = "white"), 
    panel.grid = element_line(color = "lightgray"),
    legend.key = element_blank())
Image3 #take a look

#Bar Graph of Cows
Image3.1 <- ggplot(data = experiment) +
  geom_bar(fill = "lightblue", color = "gray", mapping = aes(x = Cow_Existence_Divided)) +
  xlab(label = "Count Existence") +
  ylab(label = "Count") +
  labs(title = "Cow Existence by Dummy Variable (Cows > 4 = 1)", subtitle = "Bar Graph") +
  theme(
    panel.background = element_rect(fill = "white"), 
    panel.grid = element_line(color = "lightgray"),
    legend.key = element_blank())
Image3.1 #take a look--it is just a blob since there are no 0s

#Box Plots of Scenarios
Image4 <-ggplot(data = experiment,
                mapping = aes(y = count.patches.with..pcolor...green.)) +
  geom_boxplot(color = "darkgreen") +
  facet_wrap(~fairness.to.others) +
  ylab(label = "Greenness of Meadows") +
  labs(title = "Meadow Health and Varying Levels of Fairness-to-Others", subtitle = "Box Plots") +
  theme(
    panel.background = element_rect(fill = "white"), # it automatically went to gray so I had to change it to the correct color 
    panel.grid.major = element_line(color = "lightgray"),
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1), # this allows me to edit the strip at the top of all the tiny graphs 
    panel.grid.minor.x = element_line(colour = "lightgray", linetype = "solid", size = 0.2),
    panel.grid.minor.y = element_line(colour = "lightgray", linetype = "solid", size = 0.2))
Image4 #take a look

Image4.1 <-ggplot(data = experiment,
                mapping = aes(y = count.patches.with..pcolor...green.)) +
  geom_boxplot(color = "darkgreen") +
  facet_wrap(~risk.aversion) +
  ylab(label = "Greenness of Meadows") +
  labs(title = "Meadow Health and Varying Levels of Risk Aversion", subtitle = "Box Plots") +
  theme(
    panel.background = element_rect(fill = "white"), # it automatically went to gray so I had to change it to the correct color 
    panel.grid.major = element_line(color = "lightgray"),
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1), # this allows me to edit the strip at the top of all the tiny graphs 
    panel.grid.minor.x = element_line(colour = "lightgray", linetype = "solid", size = 0.2),
    panel.grid.minor.y = element_line(colour = "lightgray", linetype = "solid", size = 0.2))
Image4.1 #take a look

Image4.2 <-ggplot(data = experiment,
                  mapping = aes(y = count.patches.with..pcolor...green.)) +
  geom_boxplot(c) +
  facet_wrap(~positive.reciprocity) +
  ylab(label = "Greenness of Meadows") +
  labs(title = "Meadow Health and Varying Levels of Positive Reciprocity", subtitle = "Box Plots") +
  theme(
    panel.background = element_rect(fill = "white"), # it automatically went to gray so I had to change it to the correct color 
    panel.grid.major = element_line(color = "lightgray"),
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1), # this allows me to edit the strip at the top of all the tiny graphs 
    panel.grid.minor.x = element_line(colour = "lightgray", linetype = "solid", size = 0.2),
    panel.grid.minor.y = element_line(colour = "lightgray", linetype = "solid", size = 0.2))
Image4.2 #take a look

Image4.3 <-ggplot(data = experiment,
                  mapping = aes(y = count.cows)) +
  geom_boxplot(color = "darkblue") +
  facet_wrap(~risk.aversion) +
  ylab(label = "Cow Count") +
  labs(title = "Cows and Varying Levels of Risk Aversion", subtitle = "Box Plots") +
  theme(
    panel.background = element_rect(fill = "white"), # it automatically went to gray so I had to change it to the correct color 
    panel.grid.major = element_line(color = "lightgray"),
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1), # this allows me to edit the strip at the top of all the tiny graphs 
    panel.grid.minor.x = element_line(colour = "lightgray", linetype = "solid", size = 0.2),
    panel.grid.minor.y = element_line(colour = "lightgray", linetype = "solid", size = 0.2))
Image4.3 #take a look

Image4.4 <-ggplot(data = experiment,
                  mapping = aes(y = count.cows)) +
  geom_boxplot(color = "darkblue") +
  facet_wrap(~positive.reciprocity) +
  ylab(label = "Cow Count") +
  labs(title = "Cows and Varying Levels of Positive Reciprocity", subtitle = "Box Plots") +
  theme(
    panel.background = element_rect(fill = "white"), # it automatically went to gray so I had to change it to the correct color 
    panel.grid.major = element_line(color = "lightgray"),
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1), # this allows me to edit the strip at the top of all the tiny graphs 
    panel.grid.minor.x = element_line(colour = "lightgray", linetype = "solid", size = 0.2),
    panel.grid.minor.y = element_line(colour = "lightgray", linetype = "solid", size = 0.2))
Image4.4 #take a look

Image4.5 <-ggplot(data = experiment,
                  mapping = aes(y = count.cows)) +
  geom_boxplot(color = "darkblue") +
  facet_wrap(~fairness.to.others) +
  ylab(label = "Cow Count") +
  labs(title = "Cows and Varying Levels of Fairness-to-Others", subtitle = "Box Plots") +
  theme(
    panel.background = element_rect(fill = "white"), # it automatically went to gray so I had to change it to the correct color 
    panel.grid.major = element_line(color = "lightgray"),
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1), # this allows me to edit the strip at the top of all the tiny graphs 
    panel.grid.minor.x = element_line(colour = "lightgray", linetype = "solid", size = 0.2),
    panel.grid.minor.y = element_line(colour = "lightgray", linetype = "solid", size = 0.2))
Image4.5 #take a look

#Scatter Plot of Meadows and Cows
Image5 <- ggplot(data = experiment,
                 mapping = aes(x = count.patches.with..pcolor...green.,
                               y = count.cows, 
                               linetype="solid")) +
  geom_point(size = .2, alpha = .8) +
  labs(title = "Cow and Meadow Relationships with Varying Levels of Fairness-to-Others", subtitle = "Scatter Plots") + # setting this as subtitle keeps the font tiny, which seems correct 
  xlab(label = "Greenness of Meadows") + 
  ylab(label = "Cows") +
  facet_wrap(~fairness.to.others) + #separates everything by fairness.to.others
  theme(
    panel.background = element_rect(fill = "white"), 
    panel.grid = element_line(color = "lightgray"),
    legend.key = element_blank(), #this removed the gray surrounding the bullet points in the legend, is a part of theme()
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1), # this allows me to edit the strip at the top of all the tiny graphs 
    panel.border = element_rect(colour = "black", fill=NA, size=0.5))
Image5 #take a look

Image6 <- ggplot(data = experiment,
                 mapping = aes(x = count.patches.with..pcolor...green.,
                               y = count.cows, 
                               linetype="solid")) +
  geom_point(size = .2, alpha = .8) +
  labs(title = "Cow and Meadow Relationships with Varying Levels of Risk Aversion", subtitle = "Scatter Plots") + # setting this as subtitle keeps the font tiny, which seems correct 
  xlab(label = "Greenness of Meadows") + 
  ylab(label = "Cows") +
  facet_wrap(~risk.aversion) + #separates everything by risk.aversion
  theme(
    panel.background = element_rect(fill = "white"), 
    panel.grid = element_line(color = "lightgray"),
    legend.key = element_blank(), #this removed the gray surrounding the bullet points in the legend, is a part of theme()
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1), # this allows me to edit the strip at the top of all the tiny graphs 
    panel.border = element_rect(colour = "black", fill=NA, size=0.5))
Image6 #take a look

Image7 <- ggplot(data = experiment,
                 mapping = aes(x = count.patches.with..pcolor...green.,
                               y = count.cows, 
                               linetype="solid")) +
  geom_point(size = .2, alpha = .8) +
  labs(title = "Cow and Meadow Relationships with Varying Levels of Positive Reciprocity", subtitle = "Scatter Plots") + # setting this as subtitle keeps the font tiny, which seems correct 
  xlab(label = "Greenness of Meadows") + 
  ylab(label = "Cows") +
  facet_wrap(~positive.reciprocity) + #separates everything by risk.aversion
  theme(
    panel.background = element_rect(fill = "white"), 
    panel.grid = element_line(color = "lightgray"),
    legend.key = element_blank(), #this removed the gray surrounding the bullet points in the legend, is a part of theme()
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1), # this allows me to edit the strip at the top of all the tiny graphs 
    panel.border = element_rect(colour = "black", fill=NA, size=0.5))
Image7 #take a look

Image8 <- ggplot(data = experiment,
                 mapping = aes(x = count.patches.with..pcolor...green.,
                               y = count.cows, 
                               linetype="solid")) +
  geom_point(size = .2, alpha = .8) +
  labs(title = "Cows, Grass, and Feasting Relationships", subtitle = "Scatter Plots") + # setting this as subtitle keeps the font tiny, which seems correct 
  xlab(label = "Greenness of Meadows") + 
  ylab(label = "Cows") +
  facet_wrap(~feasted) + #separates everything 
  theme(
    panel.background = element_rect(fill = "white"), 
    panel.grid = element_line(color = "lightgray"),
    legend.key = element_blank(), #this removed the gray surrounding the bullet points in the legend, is a part of theme()
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1), # this allows me to edit the strip at the top of all the tiny graphs 
    panel.border = element_rect(colour = "black", fill=NA, size=0.5))
Image8 #take a look

Image9 <- ggplot(data = experiment,
                 mapping = aes(x = count.patches.with..pcolor...green.,
                               y = count.cows, 
                               linetype="solid")) +
  geom_point(size = .2, alpha = .8) +
  labs(title = "Scenario 1 Outcomes", subtitle = "Scatter Plots") + # setting this as subtitle keeps the font tiny, which seems correct 
  xlab(label = "Greenness of Meadows") + 
  ylab(label = "Cows") +
  facet_wrap(~Scenario_1) + #separates everything 
  theme(
    panel.background = element_rect(fill = "white"), 
    panel.grid = element_line(color = "lightgray"),
    legend.key = element_blank(), #this removed the gray surrounding the bullet points in the legend, is a part of theme()
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1), # this allows me to edit the strip at the top of all the tiny graphs 
    panel.border = element_rect(colour = "black", fill=NA, size=0.5))
Image9 #take a look

Image10 <- ggplot(data = experiment,
                 mapping = aes(x = count.patches.with..pcolor...green.,
                               y = count.cows, 
                               linetype="solid")) +
  geom_point(size = .2, alpha = .8) +
  labs(title = "Scenario 2 Outcomes", subtitle = "Scatter Plots") + # setting this as subtitle keeps the font tiny, which seems correct 
  xlab(label = "Greenness of Meadows") + 
  ylab(label = "Cows") +
  facet_wrap(~Scenario_2) + #separates everything 
  theme(
    panel.background = element_rect(fill = "white"), 
    panel.grid = element_line(color = "lightgray"),
    legend.key = element_blank(), #this removed the gray surrounding the bullet points in the legend, is a part of theme()
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1), # this allows me to edit the strip at the top of all the tiny graphs 
    panel.border = element_rect(colour = "black", fill=NA, size=0.5))
Image10 #take a look

Image10 <- ggplot(data = experiment,
                 mapping = aes(x = count.patches.with..pcolor...green.,
                               y = count.cows, 
                               linetype="solid")) +
  geom_point(size = .2, alpha = .8) +
  labs(title = "Scenario 3 Outcomes", subtitle = "Scatter Plots") + # setting this as subtitle keeps the font tiny, which seems correct 
  xlab(label = "Greenness of Meadows") + 
  ylab(label = "Cows") +
  facet_wrap(~Scenario_3) + #separates everything 
  theme(
    panel.background = element_rect(fill = "white"), 
    panel.grid = element_line(color = "lightgray"),
    legend.key = element_blank(), #this removed the gray surrounding the bullet points in the legend, is a part of theme()
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1), # this allows me to edit the strip at the top of all the tiny graphs 
    panel.border = element_rect(colour = "black", fill=NA, size=0.5))
Image10 #take a look

#Regression
regression1 <- lm(formula = count.patches.with..pcolor...green. ~ positive.reciprocity, data = experiment)
summary(regression1)

regression2 <- lm(formula = count.patches.with..pcolor...green. ~ positive.reciprocity + fairness.to.others, data = experiment)
summary(regression2)

regression3 <- lm(formula = count.patches.with..pcolor...green. ~ positive.reciprocity + fairness.to.others + risk.aversion, data = experiment)
summary(regression3)

regression4 <- lm(formula = count.patches.with..pcolor...green. ~ positive.reciprocity + fairness.to.others + risk.aversion + count.cows, data = experiment)
summary(regression4)

regression6 <- lm(formula = count.cows ~ fairness.to.others, data = experiment)
summary(regression6)

regression7 <- lm(formula = count.cows ~ fairness.to.others + positive.reciprocity, data = experiment)
summary(regression7)

regression8 <- lm(formula = count.cows ~ fairness.to.others + positive.reciprocity + risk.aversion, data = experiment)
summary(regression8)


#Tables of regressions
huxreg(regression1, regression2, regression3, regression4)

huxreg(regression6, regression7, regression8, regression9)

#Logistic regression 
logregression1 <- glm(formula = Meadow_Existence ~ positive.reciprocity, data = experiment, family = binomial)
summary(logregression1)

logregression2 <- glm(formula = Meadow_Existence ~ positive.reciprocity + fairness.to.others, data = experiment, family = binomial)
summary(logregression2)

logregression3 <- glm(formula = Meadow_Existence ~ positive.reciprocity + fairness.to.others + risk.aversion, data = experiment, family = binomial)
summary(logregression3)

logregression4 <- glm(formula = Meadow_Existence ~ positive.reciprocity + fairness.to.others + risk.aversion + count.cows, data = experiment, family = binomial)
summary(logregression4)

logregression6 <- glm(formula = Cow_Existence_Divided ~ fairness.to.others, data = experiment, family = binomial)
summary(logregression6)

logregression7 <- glm(formula = Cow_Existence_Divided ~ fairness.to.others + positive.reciprocity, data = experiment, family = binomial)
summary(logregression7)

logregression8 <- lm(formula = Cow_Existence_Divided ~ fairness.to.others + positive.reciprocity + risk.aversion, data = experiment, family = binomial)
summary(logregression8)

#Tables of logistic regressions
huxreg(logregression1, logregression2, logregression3, logregression4)
huxreg(logregression6, logregression7, logregression8, logregression9)

#Scenario Regressions
lregression10 <- lm(formula = Meadow_Existence ~ Scenario_1, data = experiment)
summary(lregression10)

lregression11 <- lm(formula = Meadow_Existence ~ Scenario_1 + Scenario_2, data = experiment)
summary(lregression11)

lregression12 <- lm(formula = Meadow_Existence ~ Scenario_1 + Scenario_2 + Scenario_3, data = experiment)
summary(lregression12)

lregression13 <- lm(formula = Meadow_Existence ~ Scenario_1 + Scenario_2 + Scenario_3 + count.cows, data = experiment)
summary(lregression13)

lregression14 <- lm(formula = Cow_Existence_Divided ~ Scenario_1, data = experiment)
summary(lregression14)

lregression15 <- lm(formula = Cow_Existence_Divided ~ Scenario_1 + Scenario_2, data = experiment)
summary(lregression15)

lregression16 <- lm(formula = Cow_Existence_Divided ~ Scenario_1 + Scenario_2 + Scenario_3, data = experiment)
summary(lregression16)

#Tables of scenario regressions
huxreg(lregression10, lregression11, lregression12, lregression13)
huxreg(lregression14, lregression15, lregression16, lregression17)

