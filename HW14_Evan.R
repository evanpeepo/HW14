#Researchers monitored Temperature (in Celsius) and relative humidity (%) across two sites (Auburn and Merced, CA)  
#to look at abiotic conditions tied to bee fitness. 
#How does relative humidity (%) in response to temperature (C) compare between two different research sites?

library(tidyverse)
library(lmtest)

rel_humidity <- read.csv("VAS_oCAM_TempRelHum_subset.csv")

#Filter and clean up
rel_humidity_data <- rel_humidity %>% 
  select(-X, -X.1, -X.2) %>% 
  rename(
    temp_C = Temperature..C.,
    relative_humidity = Relative.Humidity....
  )

#Graph data
ggplot(rel_humidity_data, aes(x = temp_C, y = relative_humidity, shape = Site, color = Site)) +
  geom_point() +
  geom_smooth(method = 'lm')

ggplot(rel_humidity_data, aes(temp_C, relative_humidity)) +
  geom_point() +
  geom_smooth(method = 'lm')

#1B. Fit two models

lm_1 <- lm(relative_humidity ~ temp_C * Site, data = rel_humidity_data)
lm_2 <- lm(relative_humidity ~ temp_C + Site, data = rel_humidity_data)

#1C. Negative log likelihood

-logLik(lm_1) #smaller
-logLik(lm_2)

#1D. Likelihood ratio test
lrtest(lm_1, lm_2)
#Test is significant ( p = .018), meaning the more complex model with the interaction term
#is preferred. This is the same result I got when I compared models using ANOVA. 
#Ecologically, this would mean that site has a significant impact on the relationship 
#between temperature and relative humidity.

#2A. 
lm_3 <- lm(relative_humidity ~ temp_C, data = rel_humidity_data)
lm_4 <- lm(relative_humidity ~ Site, data = rel_humidity_data)
lm_5 <- lm(relative_humidity ~ 1, data = rel_humidity_data)

AIC_table <- AIC(lm_1, lm_2, lm_3, lm_4, lm_5)
AIC_table

#2B.

#The most complex model minimizes AIC. The second model is ~3.5 greater, which is 
#moderate to little support for this model, and the last three models have much greater AIC, so 
#these should be discarded. Ecologically, this would mean that site has a significant impact on the relationship 
#between temperature and relative humidity, so the interaction should be included in the model.
#The model without the interaction has deltaAIC less than 4, so it could potentially be used, but
#it is not the best fit model and could miss important information (different slopes for site). 



