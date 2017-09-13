rm(list=ls())
library(corrplot)
library(dummies)
library(caret)
##setting the directory
setwd("C:/Users/Rohit Shiva/Desktop/INTERNSHIP")
##reading the data
data = read.table("kddcup.txt",header = T, sep=",")
View(data)
head(data)
tail(data)
sum(is.na(data))
data <- na.omit(data)
sum(is.na(data))
str(data)
dim(data)
colnames(data) = c("duration", "protocol_type", "service", "flag", "src_bytes", "dst_bytes", "land", 
                   "wrong_fragment", "urgent", "hot", "num_failed_logins", "logged_in", 
                   "num_compromised", "root_shell", "su_attempted", "num_root", "num_file_creations", 
                   "num_shells", "num_access_files", "num_outbound_cmds", "is_hot_login","is_guest_login", 
                   "count", "srv_count", "serror_rate", "srv_serror_rate", "rerror_rate","srv_rerror_rate", "same_srv_rate", "diff_srv_rate",                         "srv_diff_host_rate", "dst_host_count","dst_host_srv_count","dst_host_same_srv_rate", "dst_host_diff_srv_rate",                                    "dst_host_same_src_port_rate", "dst_host_srv_diff_host_rate", "dst_host_serror_rate","dst_host_srv_serror_rate", 
                   "dst_host_rerror_rate", "dst_host_srv_rerror_rate", "result")
View(data)
str(data)
summary(data)
data##table(data$duration)
data <- subset(data,select=-c(duration,protocol_type,service,result))
str(data)