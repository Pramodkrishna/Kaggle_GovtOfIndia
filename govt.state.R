"""
Only PLots

"""

'Loading required library'

library(ggplot2) 
library(tidyr)
library(dplyr)
library(treemap)
library(car)

"Loading and choosing the required variables "
state_data  <- s.data[,c(1,2,4,64:79)]

"Removing Na and "-" values and replacing them "

state_data[is.na(state_data)] <- 0
state_data[-2] <- lapply(state_data[-2], function(x) as.numeric(gsub("-", 0, x)))

"Aggregate the complete data set for easy analysis"

state_data <- aggregate(state_data[-2],by = list(state_data$State),
                                   na.rm = T,FUN = sum)

'Replace the name'
names(state_data)[names(state_data) == 'Group.1'] <- 'State'

'Visualization'
"Tree Map"

treemap(state_data, index = c('State'), vSize = 'Persons', vColor="count",
          type="index", palette="Blues",
          title="Persons per state", 
          fontsize.title=14,
          fontsize.labels=8,
          fontface.labels=3,
          aspRatio = 3/4,
          border.lwds=5)




'State vs Population'
'Scatter Plot'
ggplot(state_data, aes(x=state_data$State, y=state_data$Persons)) + 
  geom_point(aes(col=state_data$State, size=state_data$Persons)) + 
  geom_smooth(method="loess", se=F) + 
    ylim(c(0, 100000000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot" 
       )

'State vs Bus_Service'


ggplot(state_data, aes(state_data$State, state_data$Bus.services))+
  geom_point(aes(col=state_data$State, size=state_data$Bus.services)) + 
  geom_smooth(method="loess", se=F) + 
  ylim(c(0, 30000)) + 
  labs(subtitle="Area Vs Bus_Service", 
       y="Bus_service", 
       x="Area", 
       title="Scatterplot" 
  )

'Correlation between Busservice and Paved roads'
Paved_bus <- cor(x = state_data$Bus.services,y = state_data$Paved.approach.road)
paved_test <- cor.test(x = state_data$Bus.services,y = state_data$Paved.approach.road,
                       method = 'kendall')

'Correlation shows 0.77 which is not a weak one '
plot(y = state_data$Bus.services,
     x = state_data$Paved.approach.road)+
abline(lm(state_data$Bus.services~state_data$Paved.approach.road), col="red")





















