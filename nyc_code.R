
library(ggplot2)
library(dplyr)
library(plotly)
library(readr)


nyc <- read_csv("nyc.csv")
pairs(nyc)


ggplot(nyc, aes(x = Food, y = Price)) + geom_point()

lm(Price ~ Food, data = nyc)

nyc %>%  group_by(East) %>%  summarize(mean_price = mean(Price))  #!!!!!for Redfin project??

mean(nyc$Price[nyc$East ==0])
mean(nyc$Price[nyc$East ==1])
t.test(Price ~ East, data = nyc)
  
lm(Price ~ Food + Service, data = nyc)  #regression analysis between price food and service

p <- plot_ly(data = nyc, z = ~Price, x = ~Food, y = ~Service, opacity = 0.6) %>% add_markers() 


lm(Price ~ Food + Service + East, data = nyc)


p <- plot_ly(data = nyc, z = ~Price, x = ~Food, y = ~Service, opacity = 0.6) %>% add_markers(color = ~factor(East)) 


summary(lm(Price ~ Food + Service + Decor + East, data = nyc))
