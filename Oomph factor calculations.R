host_final<- final[c(1,4,5,6,8,9,10,11,26,35,38,59,77)]
host_final <- host_final[host_final$host_response_time == c("within an hour") & host_final$host_is_superhost == 't',]
average_rating <- host_final %>% group_by(host_id) %>% summarise(avg.rating=mean(review_scores_rating))
host_final <- host_final[!duplicated(host_final$host_id), ]
host_final<- inner_join(host_final, average_rating, by = "host_id")

host_final$host_since <- as.Date(host_final$host_since, "%Y-%m-%d")
time_in_days <- Sys.Date() - host_final$host_since


host_final$experience_rating<-scale(as.integer(time_in_days))

# host_final$response_time_rating<-ifelse(host_final$host_response_time == "within an hour",1,
#        ifelse(host_final$host_response_time == "within a few hours",.8,
#               ifelse(host_final$host_response_time == "within a day",.5,0)))

# time_in_days - mean(na.omit(as.integer(time_in_days)))

host_final$host_response_rate<-gsub(pattern = "[%]",x = host_final$host_response_rate, replacement = "",perl = T)
host_final$response_rating<-(as.numeric(host_final$host_response_rate)/100)

host_final$host_acceptance_rate<-gsub(pattern = "[%]",x = host_final$host_acceptance_rate, replacement = "",perl = T)
host_final$acceptance_rating<-(as.numeric(host_final$host_acceptance_rate)/100)

# superhost_rating<-ifelse(host_final$host_is_superhost == 't',.5,0)


host_final$security_deposit<-gsub(pattern = "[$,]",x = host_final$security_deposit, replacement = "",perl = T)
host_final$price<-gsub(pattern = "[$,]",x = host_final$price, replacement = "",perl = T)
host_final$risk<- as.numeric(host_final$security_deposit)/as.numeric(host_final$price)
host_final$risk[is.na(as.numeric(host_final$security_deposit)) & !is.na(as.numeric(host_final$price))]<- 0

host_final$risk_score <- (100 - 2*host_final$risk)/100 
host_final$risk_score[host_final$risk_score < 0] <- 0
host_final<- host_final[!(is.na(host_final$price) | is.na(host_final$avg.rating)),]
host_final$risk <- round(host_final$risk , 2)
host_final$risk_score <- round(host_final$risk_score , 2)
host_final$host_rating <- (host_final$experience_rating + 1.5* host_final$response_rating + 3*host_final$risk_score + 4*host_final$acceptance_rating)
host_final$price <- as.integer(host_final$price)
host_final$security_deposit <- as.integer(host_final$security_deposit)
host_final$host_rating <-  round(host_final$host_rating , 2)
host_final$avg.rating <-  round(host_final$avg.rating , 2)
host_final$experience_rating <-  round(host_final$experience_rating , 2)
names(host_final)[14] <- "avg_rating"
antwerp <- host_final[host_final$place == "Antwerp",]
brussels <- host_final[host_final$place == "Brussels",]
ghent <- host_final[host_final$place == "Ghent",]


anterwerp$