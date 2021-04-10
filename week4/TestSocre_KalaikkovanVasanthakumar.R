data<-read.csv("E:/Repos/StatisticsR/DSC520-Statistics/week4/scores.csv")

# What are the observational units in this study?
#Answer - comparing students performance in teh two section using course grades and total points earned in the course
# are the observational units.

# Identify the variables mentioned in the narrative paragraph and determine which are categorical and quantitative?
data
names(data)

# Answer - As per the narration, Section (Sports or Regular) is categorical variable
# Score is quantitative variable

# Create one variable to hold a subset of your data set that contains only the Regular Section and one variable for the Sports Section.
sports_section=subset(data,Section=="Sports")
head(sports_section)

regular_section=subset(data,Section=="Regular")
head(regular_section)

#Use the Plot function to plot each Sections scores and the number of students achieving that score. 
#Use additional Plot Arguments to label the graph and give each axis an appropriate label. 

Sports_Score=sports_section[,2]
Regular_Score=regular_section[,2]
par(mfrow=c(2,1))
plot(Sports_Score, xlab="number of students", ylab="Score", main="Sports")
plot(Regular_Score, xlab="number of students", ylab="Score", main="Regular")

#Once you have produced your Plots answer the following questions:
#Comparing and contrasting the point distributions between the two section, looking at both tendency and consistency: 
#Can you say that one section tended to score more points than the other? Justify and explain your answer.

# Answer - Yes, with the help of the plot, we can say that sports students are tends to score more than the regualr students.

#Did every student in one section score more points than every student in the other section? 
#If not, explain what a statistical tendency means in this context.

# Answer - It is quite evident from the plot that the sports section students have score more than the regular students.

#What could be one additional variable that was not mentioned in the narrative that could be influencing the point 
#distributions between the two sections?

#Answer - That one additional variable that was not mentioned in the narrative is the "count". If the count is included
# that could be influencing the distribution between the two sections.