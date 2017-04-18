#Read in data
library(readxl)
cde<-read_excel("indicator CDIAC carbon_dioxide_cumulative_emissions.xlsx",sheet='Data')
names(cde)

#Transform data into three columns: country, year, emissions
library(tidyr)
tidycde<-gather(cde,'year','emissions',2:239,na.rm = TRUE, convert = TRUE)

#Change name of first column to country
colnames(tidycde)[1]<- "country"

#Preview data
head(tidycde,5)

#Plot of number of entries per year
library(ggplot2)
ggplot(aes(x=year),data=tidycde)+geom_freqpoly(binwidth=1)+ 
  scale_x_continuous(limits = c(min(tidycde$year),max(tidycde$year)))

#Group emissions by year
library(dtplyr)
emission_by_year <- tidycde %>%
  group_by(year) %>%
  summarize(sum_emission = sum(emissions)) %>%
  arrange(sum_emission)

#Preview new df
head(emission_by_year,1)

#Plot of total emissions by year
ggplot(aes(x=year,y=sum_emission),data=emission_by_year)+geom_point(shape=13,size=.08)+ 
  scale_x_continuous(limits = c(min(tidycde$year)-1,max(tidycde$year)),
                     breaks=seq(min(tidycde$year)-1,max(tidycde$year),25))


topgdp<-tidycde[which(tidycde$country=='United States'|
                        tidycde$country=='China'|
                        tidycde$country=='Japan'|
                        tidycde$country=='Germany'|
                        tidycde$country=='United Kingdom'|
                        tidycde$country=='France'|
                        tidycde$country=='India'|
                        tidycde$country=='Italy'|
                        tidycde$country=='Brazil'|
                        tidycde$country=='Canada'),]

#Plot of total emssions for countries with top 10 GPD as of 2017
ggplot(aes(x=year, y=emissions, color=country),data=topgdp)+geom_point()+ 
  scale_x_continuous(limits = c(min(tidycde$year)-1,max(tidycde$year)),
                     breaks=seq(min(tidycde$year)-1,max(tidycde$year),25))



