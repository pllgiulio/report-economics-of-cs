library(ggplot2)

cost <- data.frame("duration" = c("10min", "1h", "6h", "1day", "1week", "2week"), "probability" = c(0.1, 0.21, 0.35, 0.14, 0.09, 0.07), "impact" = c(2438, 14630, 87780, 351120, 2457840, 4915680))

cost$no.sec <- cost$probability * 0.24

cost$sec <- cost$probability * 0.03

# cost distribution with no.sec 
ggplot(cost, aes(x = impact, y = no.sec)) + geom_col(fill = "#C23B23") + geom_text(label = paste(cost$impact, "$", sep=""), size = 3, aes(y = cost$no.sec - 0.005, x = cost$impact - 50000), angle = 90) + scale_fill_discrete(breaks=cost$duration) + labs(x = "Impact ($)", y = "Probability without security measures") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ scale_y_continuous(labels = scales::percent_format()) + geom_smooth(formula = "y ~ log(x)", aes(color = "#C23B23", alpha = 0.5)) + geom_point(color = "#C23B23")


# cost distribution with sec 
ggplot(cost, aes(x = impact, y = sec)) + geom_col(fill = "#769ECB") + geom_text(label = paste(cost$impact, "$", sep=""), size = 3, aes(y = cost$sec - 0.001, x = cost$impact - 50000), angle = 90) + scale_fill_discrete(breaks=cost$duration) + labs(x = "Impact ($)", y = "Probability with security measures") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ scale_y_continuous(labels = scales::percent_format()) + geom_smooth(formula = "y ~ log(x)", color = "#769ECB", aes(alpha = 0.5)) + geom_point(color = "#769ECB")




# comparison
comparison <- data.frame("values" = c(cost$no.sec, cost$sec), "security measure" = c("no", "no", "no", "no", "no", "no", "yes", "yes", "yes", "yes", "yes", "yes"), "impact" = c(cost$impact, cost$impact))

ggplot(cost, aes(impact)) + geom_smooth(formula = "y ~ log(x)", aes(y = sec, color = "#769ECB"))+
  geom_smooth(formula = "y ~ log(x)", aes(y = no.sec, color = "#C23B23"))  + labs(x = "Impact ($)", y = "Probability") + scale_y_continuous(labels = scales::percent_format()) + scale_colour_manual(name = "Security measure", values=c("#769ECB", "#C23B23"), labels = c("Yes", "No"))

