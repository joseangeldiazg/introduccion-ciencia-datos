library(Hmisc)
describe(iris)
head(ToothGrowth)
str(ToothGrowth)
table(ToothGrowth$dose)
# Ejemplo de variable en formato erroneo
boxplot(len ~ dose, data=ToothGrowth)
library(ggplot2)
ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_boxplot()

# Conversion a factor
ToothGrowth$dose<-as.factor(ToothGrowth$dose)

# Even though `dose` is a numeric variable, `boxplot` will convert it to a factor
boxplot(len ~ dose, data=ToothGrowth)
library(ggplot2)
# A basic box plot
ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_boxplot()

# # A basic box with the conditions colored
# ggplot(ToothGrowth, aes(x=dose, y=len, color=dose)+ geom_boxplot()
#
# # The above adds a redundant legend. With the legend removed:
# ggplot(ToothGrowth, aes(x=dose, y=len, color=dose))
# + geom_boxplot()

# With flipped axes
ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) + geom_boxplot() +
guides(fill=FALSE) + coord_flip()
# Two variables: 1 continuous, 1 ordinal => factor conversion (example)


head(mtcars)
# Add the regression line with confidence interval
ggplot(mtcars, aes(x=wt, y=mpg)) +
geom_point()+
geom_smooth(method=lm)

# Loess method
ggplot(mtcars, aes(x=wt, y=mpg)) +
geom_point()+
geom_smooth()

######################
# Correlation
######################

xdata <- c(2,4.4,3,3,2,2.2,2,4)
ydata <- c(1,4.4,1,3,2,2.2,2,7)
cov(xdata,ydata)

cov(xdata,ydata)/(sd(xdata)*sd(ydata))

cor(xdata,ydata)


#######################
# Bar plots examples
########################
# Univariate

mtcars[1:5,]
cyl.freq <- table(mtcars$cyl)
cyl.freq
barplot(cyl.freq)
qplot(factor(mtcars$cyl),geom="bar")

# Bivariate


# preliminary
install.packages("car")   # if you have not yet installed car
library(car)

attach(Salaries)
rankcount = table(rank) #get counts & save in vector rankcount
rankcount           # print results

rank
rank2 = table(rank,sex)
rank2
barplot(rank2, ylab = "Count", names.arg = c("Female","Male"),
main = "Faculty by Rank and Sex",
col = c("skyblue","skyblue4","burlywood"),
sub = "c. Stacked plot")
legend("topleft", c("Prof","Assoc","Asst"),
text.col = c("burlywood","skyblue4","skyblue"))

barplot(rank2, ylab = "Count", names.arg = c("Female","Male"),
main = "Faculty by Rank and Sex",
col = c("skyblue","skyblue4","burlywood"),
sub = "d. Grouped plot", beside = T)
legend("topleft", c("Prof","Assoc","Asst"),
text.col = c("burlywood","skyblue4","skyblue"))

# Add title, narrower bars, fill color, and change axis labels
ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
geom_bar(colour="black", fill="#DD8888", width=.8, stat="identity") +
guides(fill=FALSE) +
xlab("Time of day") + ylab("Total bill") +
ggtitle("Average bill for 2 people")

# Categorical data 2 variables
ggplot(data = diamonds) +
geom_count(mapping = aes(x = cut, y = color))

######creating dummies for categorical variables
install.packages("dummies")
library(dummies)
attach(Salaries)
head(Salaries)
names(Salaries)
str(Salaries)
Salaries.dummy<- dummy.data.frame(Salaries, sep = ".")
names(Salaries.dummy)
head(Salaries.dummy)

detach(Salaries)
###Missing data
library(mice)
install.packages("housingData")
housing.dat<-housingData::housing
attach(housing.dat)
str(housing.dat)
md.pattern(housing.dat)
library(VIM)
aggr_plot <- aggr(housing.dat, col=c('blue','red'), numbers=TRUE, sortVars=TRUE, labels=names(housing.dat), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


# rescaling
install.packages("scales")
library(scales)
head(housing.dat)
housing.dat$nSold.rescaled <- rescale(housing.dat$nSold)
head(housing.dat)
housing.dat$nSold.rescaled <- rescale(housing.dat$nSold, to = c(1, 100))
head(housing.dat)
#########################
# rescale.many function
###########################

rescale.many <- function(dat, column.nos) {
    nms <- names(dat)
    for(col in column.nos) {
        name <- paste(nms[col],".rescaled", sep = "")
        dat[name] <- rescale(dat[,col])
    }
    cat(paste("Rescaled ", length(column.nos),      " variable(s)n"))
    dat
}

rescale.many(housing.dat, c(5,7))


####################
# function scale.many
######################

scale.many <- function(dat, column.nos) {
    nms <- names(dat)
    for(col in column.nos) {
        name <- paste(nms[col],".z", sep = "")
        dat[name] <- scale(dat[,col])
    }
    cat(paste("Scaled ", length(column.nos), " variable(s)n"))
    dat
}
x<-scale.many(housing.dat, c(5,7))


##################
# MOSAIC plots
###################
library(vcd)
mosaic(HairEyeColor, shade=TRUE, legend=TRUE)

mosaic(Titanic,shade=TRUE, legend=TRUE)
## Highlighting:
mosaic(Survived ~ ., data = Titanic, shade=TRUE)
summary(Titanic)

# Association Plot Example
library(vcd)
assoc(HairEyeColor, shade=TRUE)

####
my_data <- PlantGrowth
# Show the group levels
levels(my_data$group)
#We want to know if there is any significant difference between
#the average weights of plants in the 3 experimental conditions.
kruskal.test(weight ~ group, data = my_data)

# From the output of the Kruskal-Wallis test, we know that
# there is a significant difference between groups,
# but we don’t know which pairs of groups are different.
# It’s possible to use the function pairwise.wilcox.test()
# to calculate pairwise comparisons between group levels

pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
p.adjust.method = "BH")
# with corrections for multiple testing.


####Matrix Scatterplot
#install.packages("Sleuth2")
library(Sleuth2)
attach(ex1713)
head(ex1713)
pairs(~ Distinct + Attend + NonChurch + StrongPct + AnnInc,
pch = 16,
col = "deepskyblue")

library(car)
scatterplotMatrix(~Distinct + Attend + NonChurch + StrongPct +
AnnInc)
#  scatter plot matrix w/out smoother & with
histograms
scatterplotMatrix(~Distinct + Attend + NonChurch + StrongPct
+ AnnInc, diagonal = "histogram",
smoother = NULL)
####Corrlation
#install.packages("corrplot")
library(Sleuth2)
library("corrplot")
attach(ex1713)
y = cor(ex1713[, 2:6]) # use all rows and columns 2-6
y
par(mfrow = c(2,2))
corrplot(y)  # default method is "circle"
corrplot(y, method = "color")
corrplot(y, method = "number")
corrplot(y, method = "ellipse", type = "lower")


library(GGally)
library(ggplot2)
data(tips, package="reshape")
ggpairs(data=tips, # data.frame with variables
columns=1:3, # columns to plot, default to all.
title="tips data", # title of the plot
) # aesthetics, ggplot2 style

ggpairs(data=tips, # data.frame with variables
columns=1:3, # columns to plot, default to all.
title="tips data", # title of the plot
ggplot2::aes(colour=sex)) # aesthetics, ggplot2 style

library(ggplot2)
head(tips)
str(tips)
sp <- ggplot(tips, aes(x=total_bill, y=tip/total_bill)) + geom_point(shape=1)
sp
# Divide by levels of "sex", in the horizontal direction
sp + facet_grid(. ~ sex)
