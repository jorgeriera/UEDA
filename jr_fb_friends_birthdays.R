library(readxl)
fbd<-read.csv('jr_friend_bday.csv')

#Format data and create date/ month variables
fbd$Start.date<-as.Date(fbd$Start.date,"%m/%d/%y")
fbd$day<-format(fbd$Start.date,"%d")
fbd$month<-format(fbd$Start.date,"%m")


#How many of my friends were born on my birthday?
length(subset(fbd,month==11&day==23))

# How many birthdays are in each month?
library(dtplyr)
fbd.month_total<-fbd %>% group_by(month) %>% 
  summarise(n=n())
fbd.month_total

# Which month contains the most number of birthdays?
subset(fbd.month_total,n==max(fbd.month_total$n))$month

# Which day of the year has the most number of birthdays?
fbd.day_total<-fbd %>% group_by(day) %>% 
  summarise(n=n())

subset(fbd.day_total,n==max(fbd.day_total$n))$day


#Bar charts
ggplot(aes(x=month),data=fbd)+geom_bar(color='black',fill='blue')+
  xlab('Month')+ylab('Number of Birthdays')+ggtitle('Birthdays by Month')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(aes(x=day),data=fbd)+geom_bar(color='black',fill='blue')+
  xlab('Day')+ylab('Number of Birthdays')+ggtitle('Birthdays by Day')+
  theme(plot.title = element_text(hjust = 0.5))



