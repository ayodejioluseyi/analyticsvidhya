
library(tidyverse)
library(purrr)
library(caret)


train <- read.csv("./data/train.csv", stringsAsFactors=F)
test <- read.csv("./data/test.csv", stringsAsFactors=F)



str(train)

# Convert variables to factors
cols <- names(train[, 1:11])


train[,cols] <- map(train[,cols], factor)

top_10 <- train %>% group_by(User_ID, Gender) %>% 
  summarize(money_spent = sum(Purchase)) %>%
  arrange(desc(money_spent)) %>% head(30) 
  ggplot(aes(User_ID, money_spent, fill = Gender)) + geom_bar(stat = "identity")

top_10


# Training and test dataset
inTraining <- createDataPartition(train$Purchase, p = .70, list = FALSE)
training_set <- train[ inTraining,]
testing_set  <- train[-inTraining,]


fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)

fit <- lm(Purchase ~ Gender + Marital_Status + Stay_In_Current_City_Years + 
             City_Category, data = train)
