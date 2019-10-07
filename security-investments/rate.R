mydata <- read.csv("360data.csv", header = F, stringsAsFactors = F) 
names(mydata) <- c('timestamp', 'ip', 'port')
mydata$timestamp <- as.Date(mydata$timestamp)
mydata$port <- factor(mydata$port)
mydata <- mydata[complete.cases(mydata),]

october <- mydata[(mydata$timestamp >= "2016-10-1" & mydata$timestamp <= "2016-10-31"),]

remove(mydata)

library(ggplot2)
library(dplyr)
library(cymruservices)
library(IPtoCountry)


#ip.info <-bulk_origin(mydata$ip)

# count number of IP per ASN https://iptoasn.com/

asn_info <- read.csv("ip2asn.tsv", header = F, stringsAsFactors = F, sep = "\t")
names(asn_info) <- c('start', 'end', 'asn.id', 'cc', 'name')
asn_info$start <- IP_integer(asn_info$start)
asn_info$end <- IP_integer(asn_info$end)
asn_info$count <- asn_info$end - asn_info$start
asn_count_ip <- aggregate(asn_info$count, by=list(asn.id = asn_info$asn.id), FUN=sum)
names(asn_count_ip) <- c('asn', 'total')

# consider only 35% of ips to be IoT devices
asn_count_ip$total <- asn_count_ip$total * 0.35

uips <- data.frame("ip" <- unique(october$ip))
names(uips) <- "ip"
max <- floor(length(uips$ip)/1000)
for(i in 1:max){
  istart <- (i-1)* 1000 +1
  iend <- (i)* 1000
  tmp <- bulk_origin(uips[i:(i+999), "ip"])
  uips[istart:iend, "asn"] <- tmp[1:1000, "as"]
  uips[istart:iend, "cc"] <- tmp[1:1000, "cc"]
  uips[istart:iend, "as.name"] <- tmp[1:1000, "as_name"]
}

# delete duplicates
tmp <- october %>% distinct(timestamp, ip)

# join ip with as
ip.asn <- merge(tmp, uips, by = "ip")

dist <- ip.asn %>% count(timestamp, asn, as.name)

dist_freq <- merge(dist, asn_count_ip, by = "asn")
dist_freq$rate <- dist_freq$n / dist_freq$total

dist_cut <- dist_freq
botmean <- aggregate(dist_cut$rate, by=list(asn = dist_cut$asn), FUN=mean)
botmean <- botmean[order(-botmean$x),]

worst3 <- botmean[1:3,]
worst3 <- merge(dist_cut, worst3, by = 'asn')

ggplot(worst3, aes(x = timestamp, y = rate, color = as.name)) + geom_line(alpha = 0.8, size = 1)+labs(x = "Date", y = "Percentage of infected device", color = "Worst 3 ISPs") + scale_y_continuous(labels = scales::percent_format())

botmean <- botmean[order(botmean$x),]

best3 <- botmean[1:3,]
best3 <- merge(dist_cut, best3, by = 'asn')

# rename to best fit into the legend
best3$as.name <- c("ATT-INTERNET4 - AT&T, US", "COGENT-174, US", "TWC-20001-PACWEST, US")

ggplot(best3, aes(x = timestamp, y = rate, color = as.name)) + geom_line(alpha = 0.8, size = 1)+labs(x = "Date", y = "Percentage of infected device", color = "Best 3 ISPs") + scale_y_continuous(labels = scales::percent_format())
