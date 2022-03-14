# Use the read.csv() function to read the data into R. Call the loaded data college.Make sure that you have the directory set to the correct location for the data.
college = read.csv("/Users/chiru/Downloads/College.csv", stringsAsFactors = TRUE)

# Look at the data using the fix() function. You should notice that the first column is just the name of each university. We don't really want R to treat this as data. However, it may be handy to have these names for later. Try the following commands:

rownames(college) = college[, 1]
fix(college)
head(college)

rownames(college) = college[, 1]
head(college)

college = college[, -1]
head(college)

# Use the summary() function to produce a numerical summary of the variables in the data set.
summary(college)

# Use the pairs() function to produce a scatterplot matrix of the first ten columns or variables of the data. Recall that you can reference the first ten columns of a matrix A using A[, 1:10].
pairs(college[, 1:10])

# Use the plot() function to produce side-by-side boxplots of Outstate versus Private.

plot(college$Private, college$Outstate, xlab = "Private", ylab = "Out-of-state tuition (dollars)")

# Create a new qualitative variable, called Elite, by binning the Top10perc variable. We are going to divide universities into two groups based on whether or not the proportion of students coming from the top 10% of their high school classes exceeds 50%.
Elite = rep("No", nrow(college))
Elite[college$Top10per > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate, xlab = "Elite", ylab = "Out-of-state tuition (dollars)")

# Use the hist() function to produce some histograms with differing numbers of bins for a few of the quantitative variables. You may find the command par(mfrow = c(2, 2)) useful: it will divide the print window into four regions so that four plots can be made simultaneously. Modifying the arguments to this function will divide the screen in other ways.

par(mfrow = c(2, 2))
hist(college$Apps, xlab = "Number of applicants", main = "Histogram for all colleges")
hist(college$Apps[college$Private == "Yes"], xlab = "Number of applicants", main = "Histogram for private schools")
hist(college$Apps[college$Private == "No"], xlab = "Number of applicants", main = "Histogram for public schools")
hist(college$Apps[college$Elite == "Yes"], xlab = "Number of applicants", main = "Histogram for elite schools")

par(mfrow = c(2, 2))
hist(college$Expend, xlab = "Instructional expenditure per student (dollars)", main = "Histogram for all colleges")
hist(college$Expend[college$Private == "Yes"], xlab = "Instructional expenditure per student (dollars)", main = "Histogram for private schools")
hist(college$Expend[college$Private == "No"], xlab = "Instructional expenditure per student (dollars)", main = "Histogram for public schools")
hist(college$Expend[college$Elite == "Yes"], xlab = "Instructional expenditure per student (dollars)", main = "Histogram for elite schools")

par(mfrow = c(2, 2))
hist(college$S.F.Ratio, xlab = "Student-Faculty Ratio", main = "Histogram for all colleges")
hist(college$S.F.Ratio[college$Private == "Yes"], xlab = "Student-Faculty Ratio", main = "Histogram for private schools")
hist(college$S.F.Ratio[college$Private == "No"], xlab = "Student-Faculty Ratio", main = "Histogram for public schools")
hist(college$S.F.Ratio[college$Elite == "Yes"], xlab = "Student-Faculty Ratio", main = "Histogram for elite schools")

# Continue exploring the data, and provide a brief summary of what you discover.

NonTuitionCosts = college$Room.Board + college$Books + college$Personal
college = data.frame(college, NonTuitionCosts)
par(mfrow = c(1, 2))
plot(college$Private, college$NonTuitionCosts, xlab = "Private", ylab = "Total non-tuition costs per year (dollars)")
plot(college$Elite, college$NonTuitionCosts, xlab = "Elite", ylab = "Total non-tuition costs per year (dollars)")

AcceptPerc = college$Accept / college$Apps * 100
college = data.frame(college, AcceptPerc)
par(mfrow = c(1, 2))
plot(college$Private, college$AcceptPerc, xlab = "Private", ylab = "Acceptance Rate")
plot(college$Elite, college$AcceptPerc, xlab = "Elite", ylab = "Acceptance Rate")

summary(college$AcceptPerc[college$Private == "Yes"])

summary(college$AcceptPerc[college$Private == "No"])

summary(college$AcceptPerc[college$Elite == "Yes"])

summary(college$AcceptPerc[college$Elite == "No"])

par(mfrow = c(2, 2))
hist(college$perc.alumni, xlab = "Percent of alumni who donate", main = "Histogram for all colleges")
hist(college$perc.alumni[college$Private == "Yes"], xlab = "Percent of alumni who donate", main = "Histogram for private schools")
hist(college$perc.alumni[college$Private == "No"], xlab = "Percent of alumni who donate", main = "Histogram for public schools")
hist(college$perc.alumni[college$Elite == "Yes"], xlab = "Percent of alumni who donate", main = "Histogram for elite schools")

par(mfrow = c(2, 2))
plot(college$PhD, college$Grad.Rate, xlab = "Number of faculty with PhDs", ylab = "Graduation Rate")
plot(college$Terminal, college$Grad.Rate, xlab = "Number of faculty with terminal degrees", ylab = "Graduation Rate")
plot(college$S.F.Ratio, college$Grad.Rate, xlab = "Student-faculty ratio", ylab = "Graduation Rate")
plot(college$Expend, college$Grad.Rate, xlab = "Instructional expenditure per student (dollars)", ylab = "Graduation Rate")

