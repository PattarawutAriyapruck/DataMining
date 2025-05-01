"Project SCMA447 Data Mining 
Member: 1.Suchanya Suwanwathana 6405077
        2.Pattarawut Ariyapruck 6405305"

################################################################################
#############################   Data Visualization  ############################
################################################################################

#Library
library(gplots)
library(ggplot2)

#Import Data
data = read.csv("C:/Users/BOSS/Documents/Data Mining/Project/Determinants of Wages Data (CPS 1985).csv")

#Cleaning Data
data = data[, -1]

#check data
str(data)
summary(data)

#label
data_label = data
data_label[,c(5:11)] = lapply(data_label[,c(5:11)], function(x) as.numeric(as.factor(x)) - 1)
str(data)
summary(data)

#correlation
heatmap.2(cor(data_label), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(data_label),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

#histogram
hist(data$wage, main = "Distribution of wage", xlab = "wage")
hist(data$education, main = "Distribution of education", xlab = "education")
hist(data$experience, main = "Distribution of experience", xlab = "experience")
hist(data$age, main = "Distribution of age", xlab = "age")

#bar chart
barplot(table(data$ethnicity), main = "Bar chart of ethnicity", xlab = "ethnicity")
barplot(table(data$region), main = "Bar chart of region", xlab = "region")
barplot(table(data$gender), main = "Bar chart of gender", xlab = "gender")
barplot(table(data$occupation), main = "Bar chart of occupation", xlab = "occupation")
barplot(table(data$sector), main = "Bar chart of sector", xlab = "sector")
barplot(table(data$union), main = "Bar chart of union", xlab = "union")
barplot(table(data$married), main = "Bar chart of married", xlab = "married")

#add factor
data[,c(5:11)] = lapply(data[,c(5:11)], as.factor)
str(data)
summary(data)

#box plot
qplot(x = married, y = wage, data = data, geom = 'boxplot')
qplot(x = married, y = education, data = data, geom = 'boxplot')
qplot(x = married, y = experience, data = data, geom = 'boxplot')
qplot(x = married, y = age, data = data, geom = 'boxplot')

#stacked plot
attach(data)
barplot(prop.table(table(ethnicity, married), 2), 
        main = "Stacked plot between married and ethnicity", 
        xlab = "married", col = c("gray","lightgray", "whitesmoke"))

barplot(prop.table(table(region, married), 2), 
        main = "Stacked plot between married and region", 
        xlab = "married", col = c("gray","lightgray"))

barplot(prop.table(table(gender, married), 2), 
        main = "Stacked plot between married and gender", 
        xlab = "married", col = c("gray","lightgray"))

barplot(prop.table(table(occupation, married), 2), 
        main = "Stacked plot between married and occupation", 
        xlab = "married", col = c("gray","lightgray", "whitesmoke", "ghostwhite", 
                                  "lavender", "lightsteelblue"))

barplot(prop.table(table(sector, married), 2), 
        main = "Stacked plot between married and sector", 
        xlab = "married", col = c("gray","lightgray", "whitesmoke"))

barplot(prop.table(table(union, married), 2), 
        main = "Stacked plot between married and union", 
        xlab = "married", col = c("gray","lightgray"))

################################################################################
#############################          kNN          ############################
################################################################################

#Library
library(DMwR2)
library(caret)

############## kNN CV
#scale data
data = data[, -c(5:10)]
data[,c("wage","education","experience","age")] = scale(data[,c("wage","education","experience","age")])
colnames(data)
str(data)

# 90,10
set.seed(123)
idxs = sample(1:nrow(data), as.integer(0.9*nrow(data)))
train_data = data[idxs,]
test_data = data[-idxs,]
table(train_data$married)
table(test_data$married)

#married ~ wage + education + experience + age # 90,10
#kNN  k = 1 
nn1 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn1)
#kNN  k = 3 
nn3 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 3)
table(test_data[, "married"], nn3)
#kNN  k = 5 
nn5 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 5)
table(test_data[, "married"], nn5)
#kNN  k = 7 
nn7 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 7)
table(test_data[, "married"], nn7)
#kNN  k = 9 
nn9 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 9)
table(test_data[, "married"], nn9)
#kNN  k = 11 
nn11 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 11)
table(test_data[, "married"], nn11)
#kNN  k = 13
nn13 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 13)
table(test_data[, "married"], nn13)
#kNN  k = 15
nn15 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 15)
table(test_data[, "married"], nn15)
#kNN  k = 17 
nn17 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 17)
table(test_data[, "married"], nn17)
#kNN  k = 19
nn19 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn19)

#married ~ wage  + experience + age # 90,10
#kNN  k = 1 
nn1 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn1)
#kNN  k = 3 
nn3 = kNN(married ~ wage + experience + age, train_data, test_data,stand = FALSE, k = 3)
table(test_data[, "married"], nn3)
#kNN  k = 5 
nn5 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 5)
table(test_data[, "married"], nn5)
#kNN  k = 7 
nn7 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 7)
table(test_data[, "married"], nn7)
#kNN  k = 9 
nn9 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 9)
table(test_data[, "married"], nn9)
#kNN  k = 11 
nn11 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 11)
table(test_data[, "married"], nn11)
#kNN  k = 13
nn13 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 13)
table(test_data[, "married"], nn13)
#kNN  k = 15
nn15 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 15)
table(test_data[, "married"], nn15)
#kNN  k = 17 
nn17 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 17)
table(test_data[, "married"], nn17)
#kNN  k = 19
nn19 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 19)
table(test_data[, "married"], nn19)

#married ~  education + experience + age # 90,10
#kNN  k = 1 
nn1 = kNN(married ~ education + experience + age, train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn1)
#kNN  k = 3 
nn3 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 3)
table(test_data[, "married"], nn3)
#kNN  k = 5 
nn5 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 5)
table(test_data[, "married"], nn5)
#kNN  k = 7 
nn7 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 7)
table(test_data[, "married"], nn7)
#kNN  k = 9 
nn9 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 9)
table(test_data[, "married"], nn9)
#kNN  k = 11 
nn11 = kNN(married ~ education + experience + age, train_data, test_data,stand = FALSE, k = 11)
table(test_data[, "married"], nn11)
#kNN  k = 13
nn13 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 13)
table(test_data[, "married"], nn13)
#kNN  k = 15
nn15 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 15)
table(test_data[, "married"], nn15)
#kNN  k = 17 
nn17 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 17)
table(test_data[, "married"], nn17)
#kNN  k = 19
nn19 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 19)
table(test_data[, "married"], nn19)

#married ~  experience + age # 90,10
#kNN  k = 1 
nn1 = kNN(married ~ experience + age, train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn1)
#kNN  k = 3 
nn3 = kNN(married ~  experience + age, train_data, test_data,stand = FALSE, k = 3)
table(test_data[, "married"], nn3)
#kNN  k = 5 
nn5 = kNN(married ~  experience + age, train_data, test_data,stand = FALSE, k = 5)
table(test_data[, "married"], nn5)
#kNN  k = 7 
nn7 = kNN(married ~  experience + age, train_data, test_data,stand = FALSE, k = 7)
table(test_data[, "married"], nn7)
#kNN  k = 9 
nn9 = kNN(married ~ experience + age, train_data, test_data,stand = FALSE, k = 9)
table(test_data[, "married"], nn9)
#kNN  k = 11 
nn11 = kNN(married ~  experience + age, train_data, test_data,stand = FALSE, k = 11)
table(test_data[, "married"], nn11)
#kNN  k = 13
nn13 = kNN(married ~  experience + age, train_data, test_data,stand = FALSE, k = 13)
table(test_data[, "married"], nn13)
#kNN  k = 15
nn15 = kNN(married ~  experience + age, train_data, test_data,stand = FALSE, k = 15)
table(test_data[, "married"], nn15)
#kNN  k = 17 
nn17 = kNN(married ~ experience + age, train_data, test_data,stand = FALSE, k = 17)
table(test_data[, "married"], nn17)
#kNN  k = 19
nn19 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 19)
table(test_data[, "married"], nn19)

#married ~  experience # 90,10
#kNN  k = 1 
nn1 = kNN(married ~ experience , train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn1)
#kNN  k = 3 
nn3 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 3)
table(test_data[, "married"], nn3)
#kNN  k = 5 
nn5 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 5)
table(test_data[, "married"], nn5)
#kNN  k = 7 
nn7 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 7)
table(test_data[, "married"], nn7)
#kNN  k = 9 
nn9 = kNN(married ~ experience , train_data, test_data,stand = FALSE, k = 9)
table(test_data[, "married"], nn9)
#kNN  k = 11 
nn11 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 11)
table(test_data[, "married"], nn11)
#kNN  k = 13
nn13 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 13)
table(test_data[, "married"], nn13)
#kNN  k = 15
nn15 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 15)
table(test_data[, "married"], nn15)
#kNN  k = 17 
nn17 = kNN(married ~ experience , train_data, test_data,stand = FALSE, k = 17)
table(test_data[, "married"], nn17)
#kNN  k = 19
nn19 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 19)
table(test_data[, "married"], nn19)

#married ~ age # 90,10
#kNN  k = 1 
nn1 = kNN(married ~ age, train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn1)
#kNN  k = 3 
nn3 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 3)
table(test_data[, "married"], nn3)
#kNN  k = 5 
nn5 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 5)
table(test_data[, "married"], nn5)
#kNN  k = 7 
nn7 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 7)
table(test_data[, "married"], nn7)
#kNN  k = 9 
nn9 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 9)
table(test_data[, "married"], nn9)
#kNN  k = 11 
nn11 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 11)
table(test_data[, "married"], nn11)
#kNN  k = 13
nn13 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 13)
table(test_data[, "married"], nn13)
#kNN  k = 15
nn15 = kNN(married ~ age, train_data, test_data,stand = FALSE, k = 15)
table(test_data[, "married"], nn15)
#kNN  k = 17 
nn17 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 17)
table(test_data[, "married"], nn17)
#kNN  k = 19
nn19 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 19)
table(test_data[, "married"], nn19)


# 80,20
set.seed(123)
idxs = sample(1:nrow(data), as.integer(0.8*nrow(data)))
train_data = data[idxs,]
test_data = data[-idxs,]
table(train_data$married)
table(test_data$married)

#married ~ wage + education + experience + age # 80,20
#kNN  k = 1 
nn1 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn1)
#kNN  k = 3 
nn3 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 3)
table(test_data[, "married"], nn3)
#kNN  k = 5 
nn5 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 5)
table(test_data[, "married"], nn5)
#kNN  k = 7 
nn7 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 7)
table(test_data[, "married"], nn7)
#kNN  k = 9 
nn9 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 9)
table(test_data[, "married"], nn9)
#kNN  k = 11 
nn11 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 11)
table(test_data[, "married"], nn11)
#kNN  k = 13
nn13 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 13)
table(test_data[, "married"], nn13)
#kNN  k = 15
nn15 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 15)
table(test_data[, "married"], nn15)
#kNN  k = 17 
nn17 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 17)
table(test_data[, "married"], nn17)
#kNN  k = 19
nn19 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn19)

#married ~ wage  + experience + age # 80,20
#kNN  k = 1 
nn1 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn1)
#kNN  k = 3 
nn3 = kNN(married ~ wage + experience + age, train_data, test_data,stand = FALSE, k = 3)
table(test_data[, "married"], nn3)
#kNN  k = 5 
nn5 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 5)
table(test_data[, "married"], nn5)
#kNN  k = 7 
nn7 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 7)
table(test_data[, "married"], nn7)
#kNN  k = 9 
nn9 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 9)
table(test_data[, "married"], nn9)
#kNN  k = 11 
nn11 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 11)
table(test_data[, "married"], nn11)
#kNN  k = 13
nn13 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 13)
table(test_data[, "married"], nn13)
#kNN  k = 15
nn15 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 15)
table(test_data[, "married"], nn15)
#kNN  k = 17 
nn17 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 17)
table(test_data[, "married"], nn17)
#kNN  k = 19
nn19 = kNN(married ~ wage  + experience + age, train_data, test_data,stand = FALSE, k = 19)
table(test_data[, "married"], nn19)

#married ~  education + experience + age # 80,20
#kNN  k = 1 
nn1 = kNN(married ~ education + experience + age, train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn1)
#kNN  k = 3 
nn3 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 3)
table(test_data[, "married"], nn3)
#kNN  k = 5 
nn5 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 5)
table(test_data[, "married"], nn5)
#kNN  k = 7 
nn7 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 7)
table(test_data[, "married"], nn7)
#kNN  k = 9 
nn9 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 9)
table(test_data[, "married"], nn9)
#kNN  k = 11 
nn11 = kNN(married ~ education + experience + age, train_data, test_data,stand = FALSE, k = 11)
table(test_data[, "married"], nn11)
#kNN  k = 13
nn13 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 13)
table(test_data[, "married"], nn13)
#kNN  k = 15
nn15 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 15)
table(test_data[, "married"], nn15)
#kNN  k = 17 
nn17 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 17)
table(test_data[, "married"], nn17)
#kNN  k = 19
nn19 = kNN(married ~  education + experience + age, train_data, test_data,stand = FALSE, k = 19)
table(test_data[, "married"], nn19)

#married ~  experience + age # 80,20
#kNN  k = 1 
nn1 = kNN(married ~ experience + age, train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn1)
#kNN  k = 3 
nn3 = kNN(married ~  experience + age, train_data, test_data,stand = FALSE, k = 3)
table(test_data[, "married"], nn3)
#kNN  k = 5 
nn5 = kNN(married ~  experience + age, train_data, test_data,stand = FALSE, k = 5)
table(test_data[, "married"], nn5)
#kNN  k = 7 
nn7 = kNN(married ~  experience + age, train_data, test_data,stand = FALSE, k = 7)
table(test_data[, "married"], nn7)
#kNN  k = 9 
nn9 = kNN(married ~ experience + age, train_data, test_data,stand = FALSE, k = 9)
table(test_data[, "married"], nn9)
#kNN  k = 11 
nn11 = kNN(married ~  experience + age, train_data, test_data,stand = FALSE, k = 11)
table(test_data[, "married"], nn11)
#kNN  k = 13
nn13 = kNN(married ~  experience + age, train_data, test_data,stand = FALSE, k = 13)
table(test_data[, "married"], nn13)
#kNN  k = 15
nn15 = kNN(married ~  experience + age, train_data, test_data,stand = FALSE, k = 15)
table(test_data[, "married"], nn15)
#kNN  k = 17 
nn17 = kNN(married ~ experience + age, train_data, test_data,stand = FALSE, k = 17)
table(test_data[, "married"], nn17)
#kNN  k = 19
nn19 = kNN(married ~ wage + education + experience + age, train_data, test_data,stand = FALSE, k = 19)
table(test_data[, "married"], nn19)

#married ~  experience # 80,20
#kNN  k = 1 
nn1 = kNN(married ~ experience , train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn1)
#kNN  k = 3 
nn3 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 3)
table(test_data[, "married"], nn3)
#kNN  k = 5 
nn5 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 5)
table(test_data[, "married"], nn5)
#kNN  k = 7 
nn7 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 7)
table(test_data[, "married"], nn7)
#kNN  k = 9 
nn9 = kNN(married ~ experience , train_data, test_data,stand = FALSE, k = 9)
table(test_data[, "married"], nn9)
#kNN  k = 11 
nn11 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 11)
table(test_data[, "married"], nn11)
#kNN  k = 13
nn13 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 13)
table(test_data[, "married"], nn13)
#kNN  k = 15
nn15 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 15)
table(test_data[, "married"], nn15)
#kNN  k = 17 
nn17 = kNN(married ~ experience , train_data, test_data,stand = FALSE, k = 17)
table(test_data[, "married"], nn17)
#kNN  k = 19
nn19 = kNN(married ~  experience , train_data, test_data,stand = FALSE, k = 19)
table(test_data[, "married"], nn19)

#married ~ age # 80,20
#kNN  k = 1 
nn1 = kNN(married ~ age, train_data, test_data,stand = FALSE, k = 1)
table(test_data[, "married"], nn1)
#kNN  k = 3 
nn3 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 3)
table(test_data[, "married"], nn3)
#kNN  k = 5 
nn5 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 5)
table(test_data[, "married"], nn5)
#kNN  k = 7 
nn7 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 7)
table(test_data[, "married"], nn7)
#kNN  k = 9 
nn9 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 9)
table(test_data[, "married"], nn9)
#kNN  k = 11 
nn11 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 11)
table(test_data[, "married"], nn11)
#kNN  k = 13
nn13 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 13)
table(test_data[, "married"], nn13)
#kNN  k = 15
nn15 = kNN(married ~ age, train_data, test_data,stand = FALSE, k = 15)
table(test_data[, "married"], nn15)
#kNN  k = 17 
nn17 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 17)
table(test_data[, "married"], nn17)
#kNN  k = 19
nn19 = kNN(married ~  age, train_data, test_data,stand = FALSE, k = 19)
table(test_data[, "married"], nn19)

############## kNN k-fold CV
#Import Data
data = read.csv("/Users/suchunyasuwanwathana/Downloads/Determinants of Wages Data (CPS 1985).csv")

#Cleaning Data
data = data[, -1]
data = data[, -c(5:10)]

############################# 10
# 10 -fold married ~ wage + education + experience + age (full)
trControl <- trainControl(method  = "cv",number = 10)
fit = train(married ~ wage + education + experience + age,
               method     = "knn",
               tuneGrid   = data.frame(k=seq(1,21,by=2)),
               preProc    = c("center", "scale"),
               trControl  = trControl,
               metric     = "Accuracy",
               data       = data)
fit
# 10 -fold married ~ wage + experience + age (without education)
trControl <- trainControl(method  = "cv",number = 10)
fit = train(married ~ wage + experience + age,
            method     = "knn",
            tuneGrid   = data.frame(k=seq(1,21,by=2)),
            preProc    = c("center", "scale"),
            trControl  = trControl,
            metric     = "Accuracy",
            data       = data)
fit
# 10 -fold married ~ education + experience + age (without wage)
trControl <- trainControl(method  = "cv",number = 10)
fit = train(married ~ education + experience + age,
            method     = "knn",
            tuneGrid   = data.frame(k=seq(1,21,by=2)),
            preProc    = c("center", "scale"),
            trControl  = trControl,
            metric     = "Accuracy",
            data       = data)
fit
# 10 -fold married ~  experience + age (without wage and education)
trControl <- trainControl(method  = "cv",number = 10)
fit = train(married ~  experience + age,
            method     = "knn",
            tuneGrid   = data.frame(k=seq(1,21,by=2)),
            preProc    = c("center", "scale"),
            trControl  = trControl,
            metric     = "Accuracy",
            data       = data)
fit

############################# 5
# 5 -fold married ~ wage + education + experience + age
trControl <- trainControl(method  = "cv",number = 5)
fit = train(married ~ wage + education + experience + age,
            method     = "knn",
            tuneGrid   = data.frame(k=seq(1,21,by=2)),
            preProc    = c("center", "scale"),
            trControl  = trControl,
            metric     = "Accuracy",
            data       = data)
fit
# 5 -fold married ~ wage + experience + age (without education)
trControl <- trainControl(method  = "cv",number = 5)
fit = train(married ~ wage + experience + age,
            method     = "knn",
            tuneGrid   = data.frame(k=seq(1,21,by=2)),
            preProc    = c("center", "scale"),
            trControl  = trControl,
            metric     = "Accuracy",
            data       = data)
fit
# 5 -fold married ~ education + experience + age (without wage)
trControl <- trainControl(method  = "cv",number = 5)
fit = train(married ~ education + experience + age,
            method     = "knn",
            tuneGrid   = data.frame(k=seq(1,21,by=2)),
            preProc    = c("center", "scale"),
            trControl  = trControl,
            metric     = "Accuracy",
            data       = data)
fit
# 5 -fold married ~  experience + age (without wage and education)
trControl <- trainControl(method  = "cv",number = 5)
fit = train(married ~  experience + age,
            method     = "knn",
            tuneGrid   = data.frame(k=seq(1,21,by=2)),
            preProc    = c("center", "scale"),
            trControl  = trControl,
            metric     = "Accuracy",
            data       = data)
fit

################################################################################
############################## Classification Tree #############################
################################################################################

#Library
library(rpart)
library(rpart.plot)

#Import Data
data = read.csv("C:/Users/BOSS/Documents/Data Mining/Project/Determinants of Wages Data (CPS 1985).csv")
data = data[, -1]

str(data)
summary(data)

data[,c(5:11)] = lapply(data[,c(5:11)], as.factor)
str(data)
summary(data)

#Name Columns
colnames(data)

################################### Try Model ##################################

##full model
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model
fit_model = rpart(married ~ wage + education + experience + age, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model without age
fit_model = rpart(married ~ wage + education + experience, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model without experience
fit_model = rpart(married ~ wage + education + age, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model without education
fit_model = rpart(married ~ wage + experience + age, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model without wage
fit_model = rpart(married ~ education + experience + age, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model without union 
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + 
                    region + gender + occupation + sector, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model without sector 
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + 
                    region + gender + occupation + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model without occupation
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + 
                    region + gender + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model without gender
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + 
                    region + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model without region
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + 
                    gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model without ethnicity
fit_model = rpart(married ~ wage + education + experience + age + region + 
                    gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model without age 
fit_model = rpart(married ~ wage + education + experience + ethnicity + region + 
                    gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model without experience
fit_model = rpart(married ~ wage + education + age + ethnicity + region + 
                    gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model without education 
fit_model = rpart(married ~ wage + experience + age + ethnicity + region + 
                    gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model without wage
fit_model = rpart(married ~ education + experience + age + ethnicity + region + 
                    gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model wage
fit_model = rpart(married ~ wage, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model wage and education
fit_model = rpart(married ~ wage + education, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model wage and experience
fit_model = rpart(married ~ wage + experience, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model wage and age
fit_model = rpart(married ~ wage + age, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model education
fit_model = rpart(married ~ education, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model education and experience
fit_model = rpart(married ~ education + experience, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model education and age
fit_model = rpart(married ~ education + age, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model experience
fit_model = rpart(married ~ experience, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)
##quantitative model experience and age
fit_model = rpart(married ~ experience + age, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model age
fit_model = rpart(married ~ age, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity
fit_model = rpart(married ~ wage + education + experience + age + ethnicity, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model region
fit_model = rpart(married ~ wage + education + experience + age + region, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model gender 
fit_model = rpart(married ~ wage + education + experience + age + gender, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model occupation
fit_model = rpart(married ~ wage + education + experience + age + occupation, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model sector
fit_model = rpart(married ~ wage + education + experience + age + sector, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model union
fit_model = rpart(married ~ wage + education + experience + age + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity and region
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + region, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity and gender 
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + gender, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity and occupation 
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + occupation, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity and sector
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + sector, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity and union
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model region and gender
fit_model = rpart(married ~ wage + education + experience + age + region + gender, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model region and occupation
fit_model = rpart(married ~ wage + education + experience + age + region + occupation, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model region and sector 
fit_model = rpart(married ~ wage + education + experience + age + region + sector, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model region and union
fit_model = rpart(married ~ wage + education + experience + age + region + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model gender and occupation 
fit_model = rpart(married ~ wage + education + experience + age + gender + occupation, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model gender and sector 
fit_model = rpart(married ~ wage + education + experience + age + gender + sector, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model gender and union 
fit_model = rpart(married ~ wage + education + experience + age + gender + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model occupation and sector
fit_model = rpart(married ~ wage + education + experience + age + occupation + sector, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model occupation and union 
fit_model = rpart(married ~ wage + education + experience + age + occupation + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model sector and union
fit_model = rpart(married ~ wage + education + experience + age + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, region and gender 
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + region + gender, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, region and occupation 
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + region + occupation, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, region and sector
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + region + sector, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, region and union
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + region + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, gender and occupation
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + gender + occupation, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, gender and sector
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + gender + occupation, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)

##quantitative model ethnicity, gender and union
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + gender + occupation, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, occupation and sector
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + occupation + sector, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, occupation and union 
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + occupation + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, region, gender and occupation
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + region + gender + occupation, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, region, gender and sector 
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + region + gender + sector, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, region, gender and union
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + region + gender + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, region, gender and union 
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + region + gender + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, region, gender, occupation and sector
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + region + gender + occupation + sector, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, region, gender, occupation and union
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + region + gender + occupation + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model wage
fit_model = rpart(married ~ wage + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model education
fit_model = rpart(married ~ education + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model experience 
fit_model = rpart(married ~ experience + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model age
fit_model = rpart(married ~ age + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model wage and education
fit_model = rpart(married ~ wage + education + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model wage and experience
fit_model = rpart(married ~ wage + experience + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model wage and age
fit_model = rpart(married ~ wage + age + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model education and experience 
fit_model = rpart(married ~ education + experience + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model education and age
fit_model = rpart(married ~ education + age + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model experience and age 
fit_model = rpart(married ~ experience + age + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

###################### Select Model After Try (Not Prune) ######################

##full model 
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model without education 
fit_model = rpart(married ~ wage + experience + age + ethnicity + region + 
                    gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model occupation 
fit_model = rpart(married ~ wage + education + experience + age + occupation, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity and occupation 
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + occupation, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model occupation and sector 
fit_model = rpart(married ~ wage + education + experience + age + occupation + sector, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model occupation and union 
fit_model = rpart(married ~ wage + education + experience + age + occupation + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##quantitative model ethnicity, region and occupation 
fit_model = rpart(married ~ wage + education + experience + age + ethnicity + region + occupation, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model experience 
fit_model = rpart(married ~ experience + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model education and experience 
fit_model = rpart(married ~ education + experience + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

##full model experience and age 
fit_model = rpart(married ~ experience + age + ethnicity + 
                    region + gender + occupation + sector + union, 
                  method = "class", data = data, control = rpart.control(minsplit = 1, xval = 10))
printcp(fit_model)
rpart.plot(fit_model)

########################## Prune Classification Tree ###########################

pfit_model = prune(fit_model, cp = fit_model$cptable[which.min(fit_model$cptable[,"xerror"]),"CP"])
printcp(pfit_model)
rpart.plot(pfit_model)

################################################################################
############################## Naive Bayes #####################################
################################################################################

#Library
library(e1071)
library("klaR")
library("caret")
library(classInt)

#Import Data
data = read.csv("C:/Users/BOSS/Documents/Data Mining/Project/Determinants of Wages Data (CPS 1985).csv")
data = data[, -1]

str(data)
summary(data)

data[,c(5:11)] = lapply(data[,c(5:11)], as.factor)
str(data)
summary(data)

################################# Intervals ####################################

##Intervals wage
x = classIntervals(data$wage, 4, style = 'quantile')
x
data$wage_I = cut(data$wage, breaks = unique(x$brks), include.lowest = TRUE, labels = c("Low", "Medium", "High", "Very High"))

##Intervals education
x = classIntervals(data$education, 4, style = 'quantile')
x
data$education_I = cut(data$education, breaks = unique(x$brks), include.lowest = TRUE, labels = c("Low", "Medium", "High"))

##Intervals experience
x = classIntervals(data$experience, 4, style = 'quantile')
x
data$experience_I = cut(data$experience, breaks = unique(x$brks), include.lowest = TRUE, labels = c("Low", "Medium", "High", "Very High"))

##Intervals age
x = classIntervals(data$age, 4, style = 'quantile')
x
data$age_I = cut(data$age, breaks = unique(x$brks), include.lowest = TRUE, labels = c("Low", "Medium", "High", "Very High"))

#Name Columns
colnames(data)

########################### Without Cross Validation ###########################

##Full Model 0.7041199 *******
model = naiveBayes(married ~ wage_I + education_I + experience_I + age_I + ethnicity + 
                     region + gender + occupation + sector + union, data = data)
model

##Full Model with Intervals 0.7059925 *******
model = naiveBayes(married ~ wage_I + education_I + experience_I + age_I, data = data)
model

##Model with Intervals and ethnicity 0.7022472
model = naiveBayes(married ~ wage_I + education_I + experience_I + age_I + ethnicity, data = data)
model

##Model with Intervals and region 0.7059925 *******
model = naiveBayes(married ~ wage_I + education_I + experience_I + age_I + region, data = data)
model

##Model with Intervals and gender 0.7059925 *******
model = naiveBayes(married ~ wage_I + education_I + experience_I + age_I + gender, data = data)
model

##Model with Intervals and occupation 0.7022472
model = naiveBayes(married ~ wage_I + education_I + experience_I + age_I + occupation, data = data)
model

##Model with Intervals and sector 0.7116105 *******
model = naiveBayes(married ~ wage_I + education_I + experience_I + age_I + sector, data = data)
model

##Model with Intervals and union 0.7059925 *******
model = naiveBayes(married ~ wage_I + education_I + experience_I + age_I + union, data = data)
model

##Full Model with categorical 0.659176 ******
model = naiveBayes(married ~ ethnicity + region + gender + occupation + sector + union, data = data)
model

##Model with categorical and without ethnicity 0.6554307
model = naiveBayes(married ~ region + gender + occupation + sector + union, data = data)
model

##Model with categorical and without region 0.6610487
model = naiveBayes(married ~ ethnicity + gender + occupation + sector + union, data = data)
model

##Model with categorical and without gender 0.6554307
model = naiveBayes(married ~ ethnicity + region + occupation + sector + union, data = data)
model

##Model with categorical and without occupation 0.6554307
model = naiveBayes(married ~ ethnicity + region + gender + sector + union, data = data)
model

##Model with categorical and without sector 0.6573034
model = naiveBayes(married ~ ethnicity + region + gender + occupation + union, data = data)
model

##Model with categorical and without union 0.6573034
model = naiveBayes(married ~ ethnicity + region + gender + occupation + sector, data = data)
model

##Model with Intervals and sector, region 0.7134831 *******
model = naiveBayes(married ~ wage_I + education_I + experience_I + age_I + sector + region, data = data)
model

##Model with Intervals and sector, gender 0.7134831 *******
model = naiveBayes(married ~ wage_I + education_I + experience_I + age_I + sector + gender, data = data)
model

##Full Model with categorical with wage_I 0.6516854
model = naiveBayes(married ~ ethnicity + region + gender + occupation + sector + union + wage_I, data = data)
model

##Full Model with categorical with education_I 0.6610487
model = naiveBayes(married ~ ethnicity + region + gender + occupation + sector + union + education_I, data = data)
model

##Full Model with categorical with experience_I 0.6985019
model = naiveBayes(married ~ ethnicity + region + gender + occupation + sector + union + experience_I, data = data)
model

##Full Model with categorical with age_I 0.6966292
model = naiveBayes(married ~ ethnicity + region + gender + occupation + sector + union + age_I, data = data)
model

##Full Model with categorical with wage_I and education_I 0.6629213
model = naiveBayes(married ~ ethnicity + region + gender + occupation + sector + union + 
                     wage_I + education_I, data = data)
model

##Full Model with categorical with wage_I and experience_I 0.7041199 ******
model = naiveBayes(married ~ ethnicity + region + gender + occupation + sector + union + 
                     wage_I + experience_I, data = data)
model

##Full Model with categorical with wage_I and age_I 0.6947566
model = naiveBayes(married ~ ethnicity + region + gender + occupation + sector + union + 
                     wage_I + age_I, data = data)
model

##Full Model with categorical with education_I and experience_I 0.6985019
model = naiveBayes(married ~ ethnicity + region + gender + occupation + sector + union + 
                     education_I + experience_I, data = data)
model

##Full Model with categorical with education_I and age_I 0.7003745
model = naiveBayes(married ~ ethnicity + region + gender + occupation + sector + union + 
                     education_I + age_I, data = data)
model

##Full Model with categorical with experience_I and age_I 0.7022472
model = naiveBayes(married ~ ethnicity + region + gender + occupation + sector + union + 
                     experience_I + age_I, data = data)
model

##Prediction
pred = predict(model, data)
Table = table(pred, data$married)
Table

##Accuracy
acc = (Table[1,1] + Table[2,2])/nrow(data)
acc

######################### With k-fold Cross Validation #########################

#Full Model 
x = data.frame(data$ethnicity, data$region, data$gender, data$occupation, data$sector, data$union,
               data$wage_I, data$education_I, data$experience_I, data$age_I)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals 
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and ethnicity
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$ethnicity)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and region 
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$region)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and gender
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$gender)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and occupation
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$occupation)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and sector 
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$sector)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and union 
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$union)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with categorical
x = data.frame(data$ethnicity, data$region, data$gender, data$occupation, data$sector, data$union) 
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Model with categorical and without ethnicity
x = data.frame(data$region, data$gender, data$occupation, data$sector, data$union) 
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Model with categorical and without region
x = data.frame(data$ethnicity, data$gender, data$occupation, data$sector, data$union) 
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Model with categorical and without gender
x = data.frame(data$ethnicity, data$region, data$occupation, data$sector, data$union) 
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Model with categorical and without occupation
x = data.frame(data$ethnicity, data$region, data$gender, data$sector, data$union) 
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Model with categorical and without sector
x = data.frame(data$ethnicity, data$region, data$gender, data$occupation, data$union) 
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Model with categorical and without union
x = data.frame(data$ethnicity, data$region, data$gender, data$occupation, data$sector) 
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and sector, region 
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$sector, data$region)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and sector, gender 
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$sector, data$gender)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

######################### Select Model With k-fold cv ##########################

#Full Model 
x = data.frame(data$ethnicity, data$region, data$gender, data$occupation, data$sector, data$union,
               data$wage_I, data$education_I, data$experience_I, data$age_I)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

##Full Model with categorical 
x = data.frame(data$ethnicity, data$region, data$gender, data$occupation, data$sector, data$union)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with categorical with wage_I and experience_I 
x = data.frame(data$ethnicity, data$region, data$gender, data$occupation, data$sector, data$union,
               data$wage_I, data$experience_I) 
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals 
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and region 
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$region)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and gender 
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$gender)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and sector 
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$sector)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and union 
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$union)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and sector, region 
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$sector, data$region)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model

#Full Model with intervals and sector, gender 
x = data.frame(data$wage_I, data$education_I, data$experience_I, data$age_I, data$sector, data$gender)
y = data$married

model = train(x, y, "nb", trControl = trainControl(method = 'cv', number = 10))
model