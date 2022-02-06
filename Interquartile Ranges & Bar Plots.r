#PreWork

#Copying comments from the Assignment Instructions
#### Multilateral Development Institution Data
#read in the data
foo <- read.csv("https://tinyurl.com/yb4phxx8")
#column names
names(foo)
#dimensions of the data set
dim(foo)
#quick look at the data structure
head(foo)
#take note of the columns representing calendar dates
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)


for(i in date.columns) # loops through the "date.columns"
{
  which_values_are_missing <- which(as.character(foo[,i]) == "") #Find missing values
  foo[which_values_are_missing, i] <- NA #Replace them by NAs
  foo[, i] <- as.Date(as.character(foo[, i])) #Turn values into dates
}
foo[3,12]
foo[4,12]
foo[3,12] - foo[4,12]
which.have.NAs <- which(is.na(foo$Rating))
foov2 <- foo[-which.have.NAs, ]
which.have.NAs <- which(is.na(foo$CirculationDate))
foov2 <- foo[-which.have.NAs, ]

#If you want to remove NAs for a given column ...
which.date <- which(foov2$CirculationDate >= "2009-01-01")
foov2 <- foov2[which.date,]

#1(a)

#Subtracting the Approval Dates from the Original Completion Dates
subt.complete_approve <- foov2$OriginalCompletionDate - foov2$ApprovalDate
which.have.NAs <- which(is.na(subt.complete_approve))
#Removing the NAs within the Subtracted variables subt.complete_approve and creating a new variable which would have the subterence between the original project completion date and the approval date excluding the NA values
subt.complete_approve.without_NA <- subt.complete_approve[-which.have.NAs]
#calulating the mean of the time subterence i.e. average project duration at approval
mean(subt.complete_approve.without_NA)


#1(b)

#Calculate actual duration by subtracting the Original Completion Date from the Revised Completion Date
subt.original_revised <- foov2$RevisedCompletionDate - foov2$OriginalCompletionDate
#Assigning a variable to the Circulation Date Column
circulation_dates <- foov2$CirculationDate

#Removing the NAs within the Circulation Date column
which.have.NAs <- which(is.na(subt.original_revised))
subt.original_revised.no_NA <- subt.original_revised[-which.have.NAs]
circulation_dates <- circulation_dates[-which.have.NAs]

#Indexing rows that have circulation dates of each year from 2009 to 2018
which.dates.2009 <- which(circulation_dates >= "2009-01-01" & circulation_dates <= "2009-12-31")
which.dates.2010 <- which(circulation_dates >= "2010-01-01" & circulation_dates <= "2010-12-31")
which.dates.2011 <- which(circulation_dates >= "2011-01-01" & circulation_dates <= "2011-12-31")
which.dates.2012 <- which(circulation_dates >= "2012-01-01" & circulation_dates <= "2012-12-31")
which.dates.2013 <- which(circulation_dates >= "2013-01-01" & circulation_dates <= "2013-12-31")
which.dates.2014 <- which(circulation_dates >= "2014-01-01" & circulation_dates <= "2014-12-31")
which.dates.2015 <- which(circulation_dates >= "2015-01-01" & circulation_dates <= "2015-12-31")
which.dates.2016 <- which(circulation_dates >= "2016-01-01" & circulation_dates <= "2016-12-31")
which.dates.2017 <- which(circulation_dates >= "2017-01-01" & circulation_dates <= "2017-12-31")
which.dates.2018 <- which(circulation_dates >= "2018-01-01" & circulation_dates <= "2018-12-31")

#Extracting information from the rows that have circulation dates of each year from 2009 to 2018
subt.original_revised.2009 <- subt.original_revised.no_NA[which.dates.2009]
subt.original_revised.2010 <- subt.original_revised.no_NA[which.dates.2010]
subt.original_revised.2011 <- subt.original_revised.no_NA[which.dates.2011]
subt.original_revised.2012 <- subt.original_revised.no_NA[which.dates.2012]
subt.original_revised.2013 <- subt.original_revised.no_NA[which.dates.2013]
subt.original_revised.2014 <- subt.original_revised.no_NA[which.dates.2014]
subt.original_revised.2015 <- subt.original_revised.no_NA[which.dates.2015]
subt.original_revised.2016 <- subt.original_revised.no_NA[which.dates.2016]
subt.original_revised.2017 <- subt.original_revised.no_NA[which.dates.2017]
subt.original_revised.2018 <- subt.original_revised.no_NA[which.dates.2018]


#Medians of the project delays of each year from 2009 to 2018
median.2009 <- median(subt.original_revised.2009)
median.2010 <- median(subt.original_revised.2010)
median.2011 <- median(subt.original_revised.2011)
median.2012 <- median(subt.original_revised.2012)
median.2013 <- median(subt.original_revised.2013)
median.2014 <- median(subt.original_revised.2014)
median.2015 <- median(subt.original_revised.2015)
median.2016 <- median(subt.original_revised.2016)
median.2017 <- median(subt.original_revised.2017)
median.2018 <- median(subt.original_revised.2018)

#Means of the project delays of each year from 2009 to 2018
mean.2009 <- mean(subt.original_revised.2009)
mean.2010 <- mean(subt.original_revised.2010)
mean.2011 <- mean(subt.original_revised.2011)
mean.2012 <- mean(subt.original_revised.2012)
mean.2013 <- mean(subt.original_revised.2013)
mean.2014 <- mean(subt.original_revised.2014)
mean.2015 <- mean(subt.original_revised.2015)
mean.2016 <- mean(subt.original_revised.2016)
mean.2017 <- mean(subt.original_revised.2017)
mean.2018 <- mean(subt.original_revised.2018)

#75th Quantile of the project delays of each year from 2009 to 2018
quantile.75.2009 <- quantile(subt.original_revised.2009, 0.75)
quantile.75.2010 <- quantile(subt.original_revised.2010, 0.75)
quantile.75.2011 <- quantile(subt.original_revised.2011, 0.75)
quantile.75.2012 <- quantile(subt.original_revised.2012, 0.75)
quantile.75.2013 <- quantile(subt.original_revised.2013, 0.75)
quantile.75.2014 <- quantile(subt.original_revised.2014, 0.75)
quantile.75.2015 <- quantile(subt.original_revised.2015, 0.75)
quantile.75.2016 <- quantile(subt.original_revised.2016, 0.75)
quantile.75.2017 <- quantile(subt.original_revised.2017, 0.75)
quantile.75.2018 <- quantile(subt.original_revised.2018, 0.75)

#25th Quantile of the project delays of each year from 2009 to 2018
quantile.25.2009 <- quantile(subt.original_revised.2009, 0.25)
quantile.25.2010 <- quantile(subt.original_revised.2010, 0.25)
quantile.25.2011 <- quantile(subt.original_revised.2011, 0.25)
quantile.25.2012 <- quantile(subt.original_revised.2012, 0.25)
quantile.25.2013 <- quantile(subt.original_revised.2013, 0.25)
quantile.25.2014 <- quantile(subt.original_revised.2014, 0.25)
quantile.25.2015 <- quantile(subt.original_revised.2015, 0.25)
quantile.25.2016 <- quantile(subt.original_revised.2016, 0.25)
quantile.25.2017 <- quantile(subt.original_revised.2017, 0.25)
quantile.25.2018 <- quantile(subt.original_revised.2018, 0.25)

#Determining the inter quantile range for 2009 to 2018)
quantiles.25 <- c(quantile.25.2009, quantile.25.2010, quantile.25.2011, quantile.25.2012, quantile.25.2013, 
                  quantile.25.2014, quantile.25.2015, quantile.25.2016, quantile.25.2017, quantile.25.2018)
quantiles.75 <- c(quantile.75.2009, quantile.75.2010, quantile.75.2011, quantile.75.2012, quantile.75.2013, 
                  quantile.75.2014, quantile.75.2015, quantile.75.2016, quantile.75.2017, quantile.75.2018)

Inter.quantile.range <- quantiles.75 - quantiles.25

#Date conversion function to plot the x axis within the scatterplot
dates <- c(as.Date("2009-01-01"), as.Date("2010-01-01"), as.Date("2011-01-01"), as.Date("2012-01-01"), 
           as.Date("2013-01-01"), as.Date("2014-01-01"), as.Date("2015-01-01"), as.Date("2016-01-01"), 
           as.Date("2017-01-01"), as.Date("2018-01-01"))

#scatterplot of the project delays with the means represented as green and the medians represented as the yellow
plot(dates, means, col="green", ylim=c(0, 1000), pch=19, ylab="Project Delays in Number of Days", xlab="Circulation Dates")
points(dates, medians, col="yellow", pch=19)
abline(lm(means ~ dates), col="green")
abline(lm(medians ~ dates), col="yellow")

#Q1(c)

#finding the planned duration using a for loop in month, assuming 1 month = 30 days
planned_duration <- c()
for (i in 1:length(foov2$Number)) {
  planned_duration <- c(planned_duration, round((foov2$OriginalCompletionDate[i] - foov2$ApprovalDate[i]) / 30, digits = 1))
}

#removing the NAs from the planned duration column
which.is.na <- which(is.na(planned_duration))
planned_duration.without_na <- planned_duration[-which.is.na]


#finding the actual duration using a for loop in month, assuming 1 month = 30 days
actual_duration <- c()
for (i in 1:length(foov2$ApprovalDate)) {
  actual_duration <- c(actual_duration, round((foov2$RevisedCompletionDate[i] - foov2$ApprovalDate[i]) / 30, digits = 1))
}


#finding the mean 
mean(actual_duration)
mean(planned_duration.without_na)
#median duration
median(actual_duration)
median(planned_duration.without_na)
#interquartile range
quantile(actual_duration, na.rm = TRUE)
quantile(planned_duration.without_na, na.rm = TRUE)
#Histogram
compare <- data.frame(planned_duration, actual_duration)
qplot(compare$planned_duration, geom='histogram', col=I("white"), binwidth = 5) + geom_histogram(aes(actual_duration), data=compare, col=I('red'), binwidth = 5, alpha = 0) +labs(title="Histogram for Duration", x="Duration (months)", y="Count")

#Q2

#selecting the projects completed after 2010
which.projects.after_2010 <- which(foov2$RevisedCompletionDate >= "2010-01-01")
foov2_after_2010 <- foov2[which.projects.after_2010,]

#number of projects rated 0
which.projects.rated_0 <- which(foov2_after_2010$Rating == 0)

#number of projects rated 1
which.projects.rated_1 <- which(foov2_after_2010$Rating == 1)

#number of projects rated 2
which.projects.rated_2 <- which(foov2_after_2010$Rating == 2)

#number of projects rated 3
which.projects.rated_3 <- which(foov2_after_2010$Rating == 3)

#number of projects rated NA
which.projects.rated_NA <- which(is.na(foov2_after_2010$Rating))

#determining the total with NA and total without NA to be inputted in the table later
total_with_NA = length(which.projects.rated_0) + length(which.projects.rated_1) + length(which.projects.rated_2) + length(which.projects.rated_3) + length(which.projects.rated_NA)
total_wo_NA = length(which.projects.rated_0) + length(which.projects.rated_1) + length(which.projects.rated_2) + length(which.projects.rated_3)

#creating a table of the number of projects completed between 2010 and now converted to their relative percentages
tablev2 <- data.frame(
  Ratings = c("0", "1", "2", "3", "NA"),
  Number_of_Projects = c(length(which.projects.rated_0), 
                         length(which.projects.rated_1), 
                         length(which.projects.rated_2), 
                         length(which.projects.rated_3), 
                         length(which.projects.rated_NA)),
  Percentage_with_NA_included = c(length(which.projects.rated_0) / total_with_NA * 100,
                                  length(which.projects.rated_1) / total_with_NA * 100,
                                  length(which.projects.rated_2) / total_with_NA * 100,
                                  length(which.projects.rated_3) / total_with_NA * 100,
                                  length(which.projects.rated_NA) / total_with_NA * 100),
  Percentage_with_NA_excluded = c(length(which.projects.rated_0) / total_wo_NA * 100,
                                  length(which.projects.rated_1) / total_wo_NA * 100,
                                  length(which.projects.rated_2) / total_wo_NA * 100,
                                  length(which.projects.rated_3) / total_wo_NA * 100,
                                  "-"),
  stringsAsFactors = FALSE
)

tablev2


#Q3

#selecting the PATA projects completed after 2010
which.projects.after_2010.PATA <- which(foov2$RevisedCompletionDate >= "2010-01-01" & foov2$Type == "PATA")
foov2_after_2010_PATA <- foov2[which.projects.after_2010.PATA,]

#number of projects rated 0
which.projects.rated_0 <- which(foov2_after_2010_PATA$Rating == 0)

#number of projects rated 1
which.projects.rated_1 <- which(foov2_after_2010_PATA$Rating == 1)

#number of projects rated 2
which.projects.rated_2 <- which(foov2_after_2010_PATA$Rating == 2)

#number of projects rated 3
which.projects.rated_3 <- which(foov2_after_2010_PATA$Rating == 3)

#number of projects rated NA
which.projects.rated_NA <- which(is.na(foov2_after_2010_PATA$Rating))

#determining the total with NA and total without NA to be inputted in the table later
total_with_NA = length(which.projects.rated_0) + length(which.projects.rated_1) + length(which.projects.rated_2) + length(which.projects.rated_3) + length(which.projects.rated_NA)
total_wo_NA = length(which.projects.rated_0) + length(which.projects.rated_1) + length(which.projects.rated_2) + length(which.projects.rated_3)

#creating a table of the number of PATA projects completed between 2010 and now converted to their relative percentages
tablev3 <- data.frame(
  Ratings = c("0", "1", "2", "3", "NA"), 
  Number_of_Projects = c(length(which.projects.rated_0), 
                         length(which.projects.rated_1), 
                         length(which.projects.rated_2), 
                         length(which.projects.rated_3), 
                         length(which.projects.rated_NA)),
  
  Percentage_with_NA_included = c(length(which.projects.rated_0) / total_with_NA * 100,
                                  length(which.projects.rated_1) / total_with_NA * 100,
                                  length(which.projects.rated_2) / total_with_NA * 100,
                                  length(which.projects.rated_3) / total_with_NA * 100,
                                  length(which.projects.rated_NA) / total_with_NA * 100),
  
  Percentage_with_NA_excluded = c(length(which.projects.rated_0) / total_wo_NA * 100,
                                  length(which.projects.rated_1) / total_wo_NA * 100,
                                  length(which.projects.rated_2) / total_wo_NA * 100,
                                  length(which.projects.rated_3) / total_wo_NA * 100,
                                  "-"),
  
  stringsAsFactors = FALSE
)
tablev3

#Q4

#Sorting the top 10% and bottom 10% of projects by their Revised Amount
foov2_sorted <- foov2[order(foov2$RevisedAmount),]
which.have.NAs <- which(is.na(foov2_sorted$Rating))
foov2_sorted <- foov2_sorted[-which.have.NAs, ]
top_10 <- head(foov2_sorted, nrow(foov2) * 0.1)
bottom_10 <- tail(foov2_sorted, nrow(foov2) * 0.1)
top_10
bottom_10

#mean rating of the top 10% projects
mean(top_10$Rating)

#median rating of the top 10% projects
median(top_10$Rating)

#mean rating of the bottom 10% projects
mean(bottom_10$Rating)

#median rating of the bottom 10% projects
median(bottom_10$Rating)

summary(top_10$Dept)
summary(bottom_10$Dept)

summary(top_10$Division)
summary(bottom_10$Division)

summary(top_10$Country)
summary(bottom_10$Country)

summary(top_10$Cluster)
summary(bottom_10$Cluster)

#individual barplots segregating the top 10% and bottom 10% projects in terms of their Division, Country, Departments, and Clusters respectively

barplot(prop.table(table(top_10$Division)), main="Top 10% Projects in terms of their Division")
barplot(prop.table(table(bottom_10$Division)), main="Bottom 10% Projects in terms of their Division")

barplot(prop.table(table(top_10$Country)), main="Top 10% Projects in terms of their Country")
barplot(prop.table(table(bottom_10$Country)), main="Bottom 10% Projects in terms of their Country")

barplot(prop.table(table(top_10$Dept)), main="Top 10% Projects in terms of their Department")
barplot(prop.table(table(bottom_10$Dept)), main="Bottom 10% Projects in terms of their Department")

barplot(prop.table(table(top_10$Cluster)), main="Top 10% Projects in terms of their Cluster")
barplot(prop.table(table(bottom_10$Cluster)), main="Bottom 10% Projects in terms of their Cluster")
