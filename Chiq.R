dat=read.csv("C:\\Users\\lalit\\Downloads\\602_hypothesis.csv")
colnames(dat)
dat
library(dplyr)

#health

#Comparing health Expenditure across different Continents

filtered_data = dat %>% 
  filter(Continent %in% c("Asia", "Europe"))
contingency_table = table(filtered_data$Continent, filtered_data$Health_Expenditure_Category)
chi_squared_result = chisq.test(contingency_table)
print(contingency_table)
print(chi_squared_result)

#Significance: Since the p-value is significantly lower than 0.05, you reject the null hypothesis. 
#This means that there is a statistically significant association between the continent (Asia vs. Europe)
#and the health expenditure categories in your dataset.

#H0:Health Expenditure Category is independent of Continent (Asia or Europe)

#Ha:Health Expenditure Category is not independent of Continent (Asia or Europe)


# north america and europe
filtered_data = dat %>% 
  filter(Continent %in% c("North America", "Europe"))
  
contingency_table = table(filtered_data$Continent, filtered_data$Health_Expenditure_Category)
chi_squared_result = chisq.test(contingency_table)
print(contingency_table)
print(chi_squared_result)

#africa vs asia
filtered_data = dat %>% 
  filter(Continent %in% c("Africa", "Asia"))

contingency_table = table(filtered_data$Continent, filtered_data$Health_Expenditure_Category)
chi_squared_result = chisq.test(contingency_table)
print(contingency_table)
print(chi_squared_result)

#life-------------------------------------------------------------------------

filtered_data = dat %>% 
  filter(Continent %in% c("North America", "Europe"))

contingency_table = table(filtered_data$Continent, filtered_data$life_Category)
chi_squared_result = chisq.test(contingency_table)
print(contingency_table)
print(chi_squared_result)
#--------------------------------------------------------------------------------
#gdp

#asia vs europe
filtered_data <- dat %>% 
  filter(Continent %in% c("Asia", "Europe"))
contingency_table <- table(filtered_data$Continent, filtered_data$GDP_Category)
chi_squared_result <- chisq.test(contingency_table)
print(contingency_table)
print(chi_squared_result)

#north america vs europe
filtered_data <- dat %>% 
  filter(Continent %in% c("North America", "Europe"))
contingency_table <- table(filtered_data$Continent, filtered_data$GDP_Category)
chi_squared_result <- chisq.test(contingency_table)
print(contingency_table)
print(chi_squared_result)


#population

filtered_data <- dat %>% 
  filter(Continent %in% c("North America", "Europe"))
contingency_table <- table(filtered_data$Continent, filtered_data$Population_Category)
chi_squared_result <- chisq.test(contingency_table)
print(contingency_table)
print(chi_squared_result)


#best model for chiq 
filtered_data <- dat %>% 
  filter(Continent %in% c("Asia", "Europe"))
contingency_table <- table(filtered_data$Continent, filtered_data$Population_Category)
chi_squared_result <- chisq.test(contingency_table)
print(contingency_table)
print(chi_squared_result)

#p value is > 0.05 so we accept the null hypothesis i.e the historical population of both 
# asian and uuropean countries are the same

hist(dat$Period.life.expectancy.at.birth...Sex..all...Age..0, 
     col = "lightgreen", 
     border = "white")

boxplot(dat$Period.life.expectancy.at.birth...Sex..all...Age..0 ~ dat$Continent, 
       col = "orange")


plot(density(dat$Period.life.expectancy.at.birth...Sex..all...Age..0), 
    col = "darkblue")

ggplot(dat, aes(x = `GDP.per.capita`, y = `Period.life.expectancy.at.birth...Sex..all...Age..0`)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") 


library(dplyr)

anova_gdp = aov(dat$Period.life.expectancy.at.birth...Sex..all...Age..0~dat$Continent + dat$GDP.per.capita)
summary(anova_gdp)

# the p value for both continent and gdp is less than 0.05 hence there is 
#this indicates that life expectancy significantly differs between continents (reject null)
#null: There is no significant difference in life expectancy between continents.
#alternate: there is significant difference in life exp among the others.

anova_health = aov(dat$Period.life.expectancy.at.birth...Sex..all...Age..0~dat$Continent + dat$Health.Expenditure)
summary(anova_health)

#----------------------------------------------------------------------------------------


#####################################################################################
anova_LE = aov(dat$Period.life.expectancy.at.birth...Sex..all...Age..0~dat$Continent)
summary(anova_LE)
anova_HE = aov(dat$Health.Expenditure~dat$Continent)
summary(anova_HE)
anova_GDP = aov(dat$GDP.per.capita~dat$Continent)
summary(anova_GDP)
anova_P = aov(dat$Population..historical.~dat$Continent)
summary(anova_P)
#--------------------------------
anova_result <- aov(Period.life.expectancy.at.birth...Sex..all...Age..0 ~ Continent, data = dat)
summary(anova_result)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
plot(tukey_result)


#######################################################################################

anova_health = aov(dat$Period.life.expectancy.at.birth...Sex..all...Age..0~dat$Health.Expenditure)
summary(anova_health)

anova_health = aov(dat$Period.life.expectancy.at.birth...Sex..all...Age..0~dat$Continent + dat$Health.Expenditure +dat$GDP.per.capita)
summary(anova_health)

#-------------------------------------------------------------------------------------------

anova_population = aov(dat$Period.life.expectancy.at.birth...Sex..all...Age..0~dat$Continent+dat$Population..historical.)
summary(anova_population)
#this suggests that the historical population does not have a statistically
#significant impact on life expectancy at birth, as p is > than 0.05 aceept null

anova_all = aov(dat$Period.life.expectancy.at.birth...Sex..all...Age..0~dat$Population..historical. + dat$Health.Expenditure +dat$GDP.per.capita+dat$Continent)
summary(anova_all)


#-----------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

top_20_richest <- data %>%
  arrange(desc(GDP.per.capita)) %>%
  head(20)

ggplot(top_20_richest, aes(x = reorder(Entity, -GDP.per.capita))) +
  geom_bar(aes(y = Period.life.expectancy.at.birth...Sex..all...Age..0), stat = "identity", fill = "orange", width = 0.4) +
  geom_bar(aes(y = GDP.per.capita / 1000), stat = "identity", fill = "skyblue", width = 0.4, position = position_dodge(width = 0.4)) +
  scale_y_continuous(
    name = "Life Expectancy (Years)",
    sec.axis = sec_axis(~ . * 1000, name = "GDP per Capita (USD)")
  ) +
  geom_text(aes(y = Period.life.expectancy.at.birth...Sex..all...Age..0, label = round(Period.life.expectancy.at.birth...Sex..all...Age..0, 1)), vjust = -0.5, size = 3) +
  geom_text(aes(y = GDP.per.capita / 1000, label = round(GDP.per.capita, 1)), vjust = 1.5, size = 3, color = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Life Expectancy and GDP per Capita by Country (Top 20 Richest)", x = "Country")
#------------------------------------------------------------
top_20_richest <- data %>%
  arrange(desc(Period.life.expectancy.at.birth...Sex..all...Age..0)) %>%
  head(20)

ggplot(top_20_richest, aes(x = reorder(Entity, -GDP.per.capita))) +
  geom_bar(aes(y = Period.life.expectancy.at.birth...Sex..all...Age..0+10), stat = "identity", fill = "orange", width = 0.4) +
  geom_bar(aes(y = GDP.per.capita / 1000), stat = "identity", fill = "skyblue", width = 0.4, position = position_dodge(width = 0.4)) +
  scale_y_continuous(
    name = "Life Expectancy (Years)",
    sec.axis = sec_axis(~ . * 1000, name = "GDP per Capita (USD)")
  ) +
  geom_text(aes(y = Period.life.expectancy.at.birth...Sex..all...Age..0, label = round(Period.life.expectancy.at.birth...Sex..all...Age..0, 1)), vjust = -0.5, size = 3) +
  geom_text(aes(y = GDP.per.capita / 1000, label = round(GDP.per.capita, 1)), vjust = 1.5, size = 3, color = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Life Expectancy and GDP per Capita by Country (LE)", x = "Country")

##------------------------------------------------------------------------
library(ggplot2)
library(plotly)
library(dplyr)

top_20_richest <- data %>%
  arrange(desc(GDP.per.capita)) %>%
  head(20)

p <- ggplot(top_20_richest, aes(x = reorder(Entity, -GDP.per.capita))) +
  geom_bar(aes(y = Period.life.expectancy.at.birth...Sex..all...Age..0), stat = "identity", fill = "orange", width = 0.4) +
  geom_bar(aes(y = GDP.per.capita / 1000), stat = "identity", fill = "skyblue", width = 0.4, position = position_dodge(width = 0.4)) +
  scale_y_continuous(
    name = "Life Expectancy (Years)",
    sec.axis = sec_axis(~ . * 1000, name = "GDP per Capita (USD)")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Country")

fig <- ggplotly(p)

fig <- fig %>%
  layout(
    title = list(text = "Life Expectancy and GDP per Capita by Country (Top 20 Richest)"),
    yaxis2 = list(
      title = "GDP per Capita (USD)",
      overlaying = "y",
      side = "right"
    )
  )

fig


#```````````````````````````````````````

library(ggplot2)
library(plotly)
library(dplyr)

top_20_richest <- data %>%
  arrange(desc(Period.life.expectancy.at.birth...Sex..all...Age..0)) %>%
  head(20)

p <- ggplot(top_20_richest, aes(x = reorder(Entity, -GDP.per.capita))) +
  geom_bar(aes(y = Period.life.expectancy.at.birth...Sex..all...Age..0), stat = "identity", fill = "orange", width = 0.4) +
  geom_bar(aes(y = GDP.per.capita / 1000), stat = "identity", fill = "skyblue", width = 0.4, position = position_dodge(width = 0.4)) +
  scale_y_continuous(
    name = "Life Expectancy (Years)",
    sec.axis = sec_axis(~ . * 1000, name = "GDP per Capita (USD)")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Country")

fig <- ggplotly(p)

fig <- fig %>%
  layout(
    title = list(text = "Life Expectancy and GDP per Capita by Country (LE)"),
    yaxis2 = list(
      title = "GDP per Capita (USD)",
      overlaying = "y",
      side = "right"
    )
  )
fig




anova_LE = aov(dat$Period.life.expectancy.at.birth...Sex..all...Age..0~dat$Continent)
summary(anova_LE)
anova_HE = aov(dat$Health.Expenditure~dat$Continent)
summary(anova_HE)
anova_GDP = aov(dat$GDP.per.capita~dat$Continent)
summary(anova_GDP)
anova_P = aov(dat$Population..historical.~dat$Continent)
summary(anova_P)
#--------------------------------


anova_result <- aov(Period.life.expectancy.at.birth...Sex..all...Age..0 ~ Continent)
summary(anova_result)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
plot(tukey_result)

# Null Hypothesis (H0):
#   There is no significant difference in the mean life expectancy between the 
#   different continents. In other words, the mean life expectancy is the same 
#   across all continents.
# 
# 
# Alternate Hypothesis (H1):
#   At least one continent's mean life expectancy is significantly different from the others.

#Since P-Value is < 0.05 we reject the Null Hypothesis (H0),i.e. there are significant 
#differences in life expectancy at birth across the different Continents.
