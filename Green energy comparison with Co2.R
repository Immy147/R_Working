library(ggplot2)
library(plot3D)
library(plotly)
library(RColorBrewer)
install.packages("devtools")
library(devtools)
install.packages("ggpubr")
library(ggpubr)


rm(list=ls())
setwd("C:\\Users\\Decent\\Documents\\R")

data<-read.csv("C:\\Users\\Decent\\Documents\\R\\Book2.csv")
data(Book2.csv)


df <- read.table("C:\\Users\\Decent\\Documents\\R\\Book2.csv", 
                 header = FALSE,
                 sep = ",")

df <- read.csv("C:\\Users\\Decent\\Documents\\R\\Book2.csv",
               header = TRUE)
head(df)
str(df)
install.packages(devtools)
library(devtools)
library(ggplot2)

AN1 <- ggplot(data = df, aes(x = Renewable energy consumption, y = Carbon emissins)) +
  geom_point(show.legend = F, alpha=0.8) +
  scale_colour_viridis_d() +
  scale_size(c(1,11)) +
  scale_x_log10() +
  labs(x="Renewable enrgy consumption", y="Carbon emission")
AN1

plot(df)
head(df)
plot(Renewable energy consumption~Carbon emissins, data=df)
hist(df$v2)
hist(df$)
View(df)



p1 <- ggplot() + geom_line( data = df, aes(Renewable.energy.consumption, Carbon.emissins))

p1


plot(df$Renewable.energy.consumption)
lines(df$Carbon.emissins, col="red")


boxplot(df$Renewable.energy.consumption ~ df$Carbon.emissins )


plot_ly(df, x = ~df$Renewable.energy.consumption, y =~df$Carbon.emissins, 
        type = 'scatter',
        mode = 'markers',
        hoverinfo= 'text')


ggplot(df, aes(x= df$Renewable.energy.consumption, y=df$Carbon.emissins))+geom_point()+geom_line()+
  geom_smooth(method = "lm")+
  labs(title = "Renawble energy comparison", x="renawble energy", y= "co2 emission")

ggplot(df, aes(df$Renewable.energy.consumption, df$Carbon.emissins, col=df$Carbon.emissins))+
  geom_point()


ggplot(df, aes(x = df$Renewable.energy.consumption)) + 
  geom_bar(position = "dodge") +
  labs(x = "renawble energy", 
       y = "years", 
       fill = "Storm Category")


shapiro.test(df$Renewable.energy.consumption)
shapiro.test(df$Carbon.emissins)



library(plotly)
t1 <- rnorm(df$Renewable.energy.consumption)
t2 <- rnorm(df$Carbon.emissins)

fig <- plot_ly(df, x = df$Years)
fig <- fig %>% add_trace(y = ~t1,labs(y=""), name = 'Renewable Energy consumption',mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~t2,labs(y="") , name = 'Carbon Emissions', mode = 'lines+markers')

fig
help("plotly")


a1 <- rnorm(df$Carbon.emissins)
a2 <- rnorm(df$GDP.per.capita..constant.2010.US..)

fig <- plot_ly(df, x = df$Years)
fig <- fig %>% add_trace(y = ~t1,labs(y=""), name = 'Carbon Emissions',mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~t2,labs(y="") , name = 'GDP per capita', mode = 'lines+markers')

fig
View(df)



b1 <- rnorm(df$Carbon.emissins)
b2 <- rnorm(df$Trade....of.GDP.)

fig <- plot_ly(df, x = df$Years)
fig <- fig %>% add_trace(y = ~b1,labs(y=""), name = 'Carbon Emissions',mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~b2,labs(y="") , name = 'trade openness', mode = 'lines+markers')

fig