rm(list=ls())
###loading the libraries
library(corrplot)
library(dummies)
library(caret)
install.packages("utils")
library(utils)

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
data
##table(data$duration)
data <- subset(data,select=-c(duration,protocol_type,service,result))
str(data)


###pre-processing the data
data$duration = as.numeric(as.character(data$duration))
data$protocol_type = factor(data$protocol_type)
data$service = factor(data$service)
data$flag = factor(data$flag)
data$src_bytes = as.numeric(as.character(data$src_bytes))
data$dst_bytes = as.numeric(as.character(data$dst_bytes))
data$land = factor(data$land)
data$wrong_fragment = as.numeric(as.character(data$wrong_fragment))
data$urgent = as.numeric(as.character(data$urgent))
data$hot = as.numeric(as.character(data$hot))
data$num_failed_logins = as.numeric(as.character(data$num_failed_logins))
data$logged_in = factor(data$logged_in)
data$num_compromised = as.numeric(as.character(data$num_compromised))
data$root_shell = factor(data$root_shell)
data$su_attempted = factor(data$su_attempted)
data$num_root = as.numeric(as.character(data$num_root))
data$num_file_creations = as.numeric(as.character(data$num_file_creations))
data$num_shells = as.numeric(as.character(data$num_shells))
data$num_access_files = as.numeric(as.character(data$num_access_files))
# data$num_outbound_cmds = as.numeric(as.character(data$num_outbound_cmds))
# data$is_hot_login = factor(data$is_hot_login)
data$is_guest_login = factor(data$is_guest_login)
data$count = as.numeric(as.character(data$count))
data$srv_count = as.numeric(as.character(data$srv_count))
data$serror_rate = as.numeric(as.character(data$serror_rate))
data$srv_serror_rate = as.numeric(as.character(data$srv_serror_rate))
data$rerror_rate = as.numeric(as.character(data$rerror_rate))
data$srv_rerror_rate = as.numeric(as.character(data$srv_rerror_rate))
data$same_srv_rate = as.numeric(as.character(data$same_srv_rate))
data$diff_srv_rate = as.numeric(as.character(data$diff_srv_rate))
data$srv_diff_host_rate = as.numeric(as.character(data$srv_diff_host_rate))
data$dst_host_count = as.numeric(as.character(data$dst_host_count))
data$dst_host_srv_count = as.numeric(as.character(data$dst_host_srv_count))
data$dst_host_same_srv_rate = as.numeric(as.character(data$dst_host_same_srv_rate))
data$dst_host_diff_srv_rate = as.numeric(as.character(data$dst_host_diff_srv_rate))
data$dst_host_same_src_port_rate = as.numeric(as.character(data$dst_host_same_src_port_rate))
data$dst_host_srv_diff_host_rate = as.numeric(as.character(data$dst_host_srv_diff_host_rate))
data$dst_host_serror_rate = as.numeric(as.character(data$dst_host_serror_rate))
data$dst_host_srv_serror_rate = as.numeric(as.character(data$dst_host_srv_serror_rate))
data$dst_host_rerror_rate = as.numeric(as.character(data$dst_host_rerror_rate))
data$dst_host_srv_rerror_rate = as.numeric(as.character(data$dst_host_srv_rerror_rate))



data$result = as.character(data$result)
data$result[data$result == "ipsweep."] = "probe"
data$result[data$result == "portsweep."] = "probe"
data$result[data$result == "nmap."] = "probe"
data$result[data$result == "satan."] = "probe"
data$result[data$result == "buffer_overflow."] = "u2r"
data$result[data$result == "loadmodule."] = "u2r"
data$result[data$result == "perl."] = "u2r"
data$result[data$result == "rootkit."] = "u2r"
data$result[data$result == "back."] = "dos"
data$result[data$result == "land."] = "dos"
data$result[data$result == "neptune."] = "dos"
data$result[data$result == "pod."] = "dos"
data$result[data$result == "smurf."] = "dos"
data$result[data$result == "teardrop."] = "dos"
data$result[data$result == "ftp_write."] = "r2l"
data$result[data$result == "guess_passwd."] = "r2l"
data$result[data$result == "imap."] = "r2l"
data$result[data$result == "multihop."] = "r2l"
data$result[data$result == "phf."] = "r2l"
data$result[data$result == "spy."] = "r2l"
data$result[data$result == "warezclient."] = "r2l"
data$result[data$result == "warezmaster."] = "r2l"
data$result[data$result == "normal."] = "normal"
data$result = as.factor(data$result)

