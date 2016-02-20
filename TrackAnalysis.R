#Running Analysis
library(lubridate)
library(ggplot2)

#convert string to dataframe
string <- c("xy_100_ab", "xy_101_ab","xy_102_ab","xy_103_ab") 
out <- data.frame( do.call( rbind, strsplit( string, '_' ) ) ) 
names(out) <- paste('column',1:3,sep="") 
out 


df1 <- read.csv("~/Desktop/R Projects/Track Track App/runnerstuff/RunnerStats.csv", stringsAsFactors = FALSE)

#Convert times and get difference in times
df1$x2014dc <- strptime(df1$X2014, format = "%H:%M:%S")
df1$x2015dc <- strptime(df1$X2015, format = "%H:%M:%S")
df1$diff <- df1$x2015dc - df1$x2014d
df1 <- df1[-67,]

#Order by time diff & event
df <- df1[order(df1$diff, df1$Event),]

#Convert
df$diffn <- as.numeric(df$diff)
df$diffi <- as.integer(df$diff)

#Remove NAs
df <- na.omit(df)

#Cluster
fit <- kmeans(df$diffn, centers = 2)
plot(df$diffn, col = fit$cluster)

df$cluster <- factor(fit$cluster)
center <- as.data.frame(fit$centers)

#Runner
ggplot(data = df, aes(x = Runner, y = diffn, color = cluster, size = -diffn)) + geom_point() + xlab("Runner") + ylab("Improved Time in Seconds") + ggtitle("Improved time by Runner")
#+ geom_text(aes(label=Runner))

#Coach
ggplot(data = df, aes(x = Coach, y = diffn, color = factor(Event))) + geom_point() + xlab("Coach") + ylab("Improved Time in Seconds") + ggtitle("Improved Time by Coach") + geom_text(aes(label=Runner))

ggplot(data = df, aes(x = Coach, y = diffn, color = factor(Event), size = 10)) + geom_point() + xlab("Coach") + ylab("Improved Time in Seconds") + ggtitle("Improved Time by Coach")

#Event
ggplot(data = df, aes(x = as.factor(Event), y = diffn, color = cluster)) + geom_point() + xlab("Event") + ylab("Improved Time in Seconds") + ggtitle("Improved Time by Event") + geom_text(aes(label=Runner))





#Import CCS Data
ccs <- read.csv("xc_boys_champ_2014.csv", stringsAsFactors = FALSE)

#convert to times
ccs$Time_time <- strptime(ccs$Time, format = "%M:%S")
ccs$Pace_time <- strptime(ccs$Pace, format = "%M:%S")

ccs$Timen <- as.numeric(ccs$Time_time)
ccs$Pacen <- as.numeric(ccs$Pace_time)
ccs$Year <- as.factor(ccs$Year)


#Replace empty cells with certain values:
#Team.Place

#

# 
# #Get avg pace and time per school
# ccsapp <- ccs[,c(5,7,18)]
# ccsagg <- ddply(ccsapp, .(School), summarize, mean_pace = mean(Pacen, na.rm = TRUE), sd_pace = sd(Pacen, na.rm = TRUE))
# plot((ccsagg)
# 
#  #Lm
# ccslm <- lm(Timen ~ School + Year + Team.or.Individual, data = ccs)
# summary(ccslm)
# 
# #Dtree
# ccstree <- tree(Timen ~ School + Year + Team.or.Individual, data = ccs)
# 
#RandomForest
ccsdf_rf <- ccs[,-c(15,16)]
ccsdf_rf <- na.omit(ccsdf_rf)
ccsrf <- randomForest(Time ~ School + Team.or.Individual + Year, data = ccsdf_rf, ntree = 20, mtry=5)



#Runner
ggplot(data = ccs, aes(x = School, y = Time, color = School)) + geom_point() + xlab("Racer") + ylab("Position") + ggtitle("CCS 2014") + geom_text(aes(label=Last.Name))
 #Removed ,aes(...size = -Team.Place)

ggplot(data = ccs, aes(x = School, y = mean, color = School)) + geom_point() + xlab("Racer") + ylab("Position") + ggtitle("CCS 2014") + geom_text(aes(label=Last.Name))



