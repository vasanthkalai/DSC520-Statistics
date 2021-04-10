survey<-read.csv("E:/Repos/StatisticsR/DSC520-Statistics/week4/acs-14-1yr-s0201.csv")
head(survey)

library(readxl)

housing<-read_excel("E:/Repos/StatisticsR/DSC520-Statistics/week4/week-6-housing.xlsx")
head(housing)

# Use the apply function on a variable in your dataset

apply(survey,2,length)
apply(housing,2,length)

# Use the aggregate function on a variable in your dataset

aggregate(survey$Geography,list(unique.values=survey$Geography),length)


# Use the plyr function on a variable in your dataset - more specifically, 
# I want to see you split some data, perform a modification to the data, and then bring it back together

d <- data.frame(year = rep(2000:2002, each = 3),count = round(runif(9, 0, 20)))
print(d)

library(plyr)
ddply(d, "year", function(x) {
  mean.count <- mean(x$count)
  sd.count <- sd(x$count)
  cv <- sd.count/mean.count
  data.frame(cv.count = cv)
})
ddply(d, "year", summarise, mean.count = mean(count))
ddply(d, "year", transform, total.count = sum(count))
ddply(d, "year", mutate, mu = mean(count), sigma = sd(count),cv = sigma/mu)

housing.dat <- subset(housing, 'sale year' > 2000)
x <- ddply(housing.dat, c("'sale year'", "ctyname"), summarize, homeruns = sum('Sale Price'))
head(x)

# Check distributions of the data
install.packages("fitdistrplus")
library(fitdistrplus)
normal_dist <- fitdist(housing$`Sale Price`, "norm")
plot(normal_dist)

# Identify if there are any outliers
summary(housing$`Sale Price`)
hist(housing$`Sale Price`,xlab = "Price",main = "Histogram of Price",breaks = sqrt(nrow(dat))) 

# Create at least 2 new variables
country<-rep("USA",12865)
serial_no<-c(1:12865)

new_df<-data.frame(serial_no,housing,country)
head(new_df)
