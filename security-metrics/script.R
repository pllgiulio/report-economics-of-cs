mydata <- read.csv("360data.csv", header = F, stringsAsFactors = F) 
names(mydata) <- c('timestamp', 'ip', 'port')
mydata$timestamp <- as.Date(mydata$timestamp)
mydata$port <- factor(mydata$port)
mydata <- mydata[complete.cases(mydata),]

library(ggplot2)
library(dplyr)
library(IPtoCountry)

## Port chart

mydata$month <- format(as.Date(mydata$timestamp), "%Y-%m")
ggplot(mydata, aes(x = month,  fill = port)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Date", y = "Port distribution") +
  scale_y_continuous(labels = scales::percent_format())


## Infection rate chart
mydata$country <- IP_country(mydata$ip)
x <- mydata %>% group_by(mydata$timestamp, country) %>% summarize(count=n())

#ip database can be found here: https://download.ip2location.com/lite/ 
ips <- read.csv("ip.csv", header = F, stringsAsFactors = F)
names(ips) <- c('start', 'end', 'code', 'name')
ips$sum <- ips$end - ips$start
ipc <- aggregate(ips$sum, by=list(name = ips$name), FUN=sum)
names(ipc) <- c('country', 'total')

wrapper <- merge(x, ipc, by = "country")

wrapper$per <- wrapper$total * 0.35
names(wrapper) <- c('country', 'date', 'count_iot', 'total_ip', 'total_iot')
wrapper <- wrapper[order(wrapper$date, wrapper$country),]

wrapperm <- wrapper
wrapperm$date <- format(as.Date(wrapperm$date), "%Y-%m")
wrapperm <- aggregate(wrapperm$count_iot, by=list(country = wrapperm$country, month = wrapperm$date), FUN=mean)
names(wrapperm) <- c('country', 'month', 'avg')

wrapperm <- merge(wrapperm, ipc, by = "country")
wrapperm$total <- wrapperm$total * 0.35
wrapperm$rate <- wrapperm$avg / wrapperm$total

wrappercut <- wrapperm[(wrapperm$avg > 200),]
botmean <- aggregate(wrappercut$rate, by=list(country = wrappercut$country), FUN=mean)
names(botmean) <- c('country', 'mean')
botmean$country <- as.character(botmean$country)
botmean <- botmean[order(-botmean$mean),]
botmean$country[5] <- "Taiwan"
botmean$country <- factor(botmean$country)

top10 <- botmean[1:10,]
top10 <- merge(wrapperm, top10, by = 'country')

top10$date <- as.Date(paste(top10$month, '-01', sep=''))
top10[(top10$date > '2016-12-01'), "date"] <- '2017-01-24'

ggplot(top10, aes(x = date, y = rate, color = country)) + geom_line(alpha = 0.5)+labs(x = "Date", y = "Percentage of infected device") + scale_y_continuous(labels = scales::percent_format())


