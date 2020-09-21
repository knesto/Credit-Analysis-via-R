
# load data
credit <- read.csv("credit123.csv", sep = ";" , header = T )

library(ggplot2)

# Visualization of the data
# Bar chart of the loan amount
loanamount_barchart <- ggplot(data=credit, aes(amount)) + 
  geom_histogram(breaks=seq(0, 35000, by=1000), 
                 col="black", aes(fill=..count..)) +
  scale_fill_gradient("Count", low="green1", high="yellowgreen")+
  labs(title="Loan Amount", x="Amount", y="Number of Loans")
loanamount_barchart


# Box plot of loan amount
box_plot_stat <- ggplot(loan_data, aes(status, amount))
box_plot_stat + geom_boxplot(aes(fill = status)) +
  theme(axis.text.x = element_blank()) +
  labs(list(title = "Loan amount by status", x = "Loan Status", y = "Amount"))


credit <- credit[, c('duration', 'amount', 'installment', 'age', 'history', 'purpose', 'housing','Default')]

# check if all columns are numeric
str(credit)

# create function to convert non-numeric data.frame columns to numeric
as.numeric_from_fractor <- function(x) if(is.factor(x)) as.integer(factor(x)) else x
# apply the function to convert credit data.frame
credit[] <- lapply(credit, as.numeric_from_fractor)
# check data frame again
str(credit)

head(credit)

library(caTools)

# spliting the credit data
split = sample.split(credit$Default, SplitRatio = 0.9)
train_credit = subset(credit, split == TRUE)
test_credit = subset(credit, split == FALSE)

# test the output
nrow(train_credit)
nrow(test_credit)


# create model
credit_LR <- glm(Default~.,data=train_credit,family=binomial())

# display model details
summary(credit_LR)

library(lmtest)
library(rcompanion)
library(caret)

predicted_test <- predict(credit_LR,newdata=test_credit,type="response")
data.frame(test_credit$Default,predicted_test)[1:10,] # [actual, predicted]

# confusion matrix

confusionMatrix(data = as.factor(predicted_test>0.5), reference = as.factor(test_credit$Default>0.5))

# Analysis of variance $
anova(credit_LR, test="Chisq")

# Pseudo-R-squared
nagelkerke(credit_LR)

# Overall p-value for model
anova(credit_LR,update(credit_LR, ~1), test="Chisq")

library(ROCR)

LR_pred <- predict(credit_LR,type='response',newdata=subset(credit))
credit_pred <- prediction(LR_pred,credit$Default)
credit_perf <- performance(credit_pred,"tpr","fpr")

# Plot Logistic Regression Model for  Credit
plot(credit_perf,col="red")