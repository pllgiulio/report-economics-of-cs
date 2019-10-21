
library(ggplot2)
library(dplyr)
#library(IPtoCountry)
library(rgeolocate)

#ip database can be found here: https://download.ip2location.com/lite/ 
ips <- read.csv("ip.csv", header = F, stringsAsFactors = F)
names(ips) <- c('start', 'end', 'code', 'name')
ips$sum <- ips$end - ips$start
ip_count <- aggregate(ips$sum, by=list(name = ips$name), FUN=sum)
names(ip_count) <- c('country', 'total')
ip_count <- ip_count[(ip_count$total > 1000000),]
ip_count$total <- ip_count$total * 0.35


mydata <- read.csv("360data.csv", header = F, stringsAsFactors = F) 
names(mydata) <- c('timestamp', 'ip', 'port')
mydata$timestamp <- as.Date(mydata$timestamp)
mydata$port <- factor(mydata$port)
mydata <- mydata[complete.cases(mydata),]


# delete duplicates
tmp <- mydata %>% distinct(timestamp, ip, port)

# map IP to countries
file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
tmp$country <- unlist(maxmind(tmp$ip, file, "country_name"))
tmp$continent <- unlist(maxmind(tmp$ip, file, "continent_name"))

europe <- tmp[(tmp$continent== "Europe"),]


# map IP to countries
#tmp$country <- IP_country(tmp$ip)

# count distint IP per day per country
ip_per_day <- europe %>% group_by(timestamp, country) %>% summarize(count=n())
ip_per_day <- merge(ip_per_day, ip_count, by = "country")
ip_per_day$avg <- ip_per_day$count/ip_per_day$total
ip_per_month <- ip_per_day
ip_per_month$timestamp <- format(as.Date(ip_per_month$timestamp), "%Y-%m")

country_months <- aggregate(ip_per_month$avg, by=list(month = ip_per_month$timestamp, country = ip_per_month$country), FUN=mean)
names(country_months) <- c("month", "country", "avg")


plot(density(log(country_months$avg)), main="Density Plot: Montly averages", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(log(country_months$avg)), 2)))  # density plot for 'avg'
polygon(density(log(country_months$avg)), col="red")
shapiro.test(log(country_months$avg))


## GCI
gci <- read.csv2("GCI.csv", header = T, stringsAsFactors = F)
gci$gci <- as.numeric(gci$gci)
daily_ip_gci <- merge(ip_per_day, gci, by = "country")
month_ip_gci <- merge(country_months, gci, by = "country")

gci_europe <- month_ip_gci %>% distinct(country, gci)

plot(density(gci_europe$gci), main="Density Plot: GCI", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(gci_europe$gci), 2)))  # density plot for 'avg'
polygon(density(gci_europe$gci), col="red")
shapiro.test(gci_europe$gci)

ggplot(month_ip_gci, aes(x = gci, y = avg)) + geom_point(size = 2, alpha = 0.5, color="blue") + labs(y = "Montly infection rate", x = "GCI")
tmp <- month_ip_gci[(month_ip_gci$avg < 0.0004),]
ggplot(tmp, aes(x = gci, y = avg)) + geom_point(size = 2, alpha = 0.5, color="blue") + labs(y = "Montly infection rate", x = "GCI")
ggplot(month_ip_gci, aes(x = gci, y = log(avg))) + geom_point(size = 2, alpha = 0.5, color="blue") + labs(y = "Montly infection rate (log)", x = "GCI")



log_avg.lm = lm(log(month_ip_gci$avg) ~ month_ip_gci$gci, data = month_ip_gci)
log_avg.stdres = rstandard(log_avg.lm)
qqnorm(log_avg.stdres)
qqline(log_avg.stdres, col="blue")
shapiro.test(log_avg.stdres)
summary(log_avg.lm)

p <- ggplot(month_ip_gci, aes(x = gci, y = log(avg))) + geom_point(size = 2, alpha = 0.5, color="blue") + labs(y = "Montly infection rate (log)", x = "GCI")

reg <- function(x) -5.8096 + -7.7840 * x 

p + stat_function(fun = reg)


