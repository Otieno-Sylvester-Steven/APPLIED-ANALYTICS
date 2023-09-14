#Reading the Data
credit_data<-read.csv("credit.csv")
#Exploratory Data Analysis
head(credit_data) # to show the first five rows
tail(credit_data) # to show the last five rows
dim(credit_data)  # to show the number of rows and columns in the data frame
summary(credit_data)
## sample data plots
par(mfrow=c(2,2))

#frequency plots of numeric variables
hist(credit_data$default, main = "plot of Debtors Default in Credit History", xlab = "Default")
hist(credit_data$age, main = "plot of Age Default in Credit History", xlab = "Age")

#frequency plots of character variables
plot(as.factor(credit_data$job), main = "plot of job groups of debtors in credit history", xlab = "job groups")
plot(as.factor(credit_data$housing), main = "plot of Housing of debtors in credit history", xlab = "Housing")

# DATA PREPROCESSING

## character to numeric data
#Regression models necessitate that the data should be numeric. Otherwise, we need to implement classification models such as K-means, KNN, tree based methods
# Factor out numeric columns
num_data<-credit_data[ ,1:8]
head(num_data)

# A function to change categorical character to numeric data
checking_balance<-factor(credit_data$checking_balance, ordered = TRUE)
head(checking_balance)

head(as.numeric(checking_balance))

#Generalizing the function to apply to all columns
char_cols<-credit_data[ , 9:ncol(credit_data)]

fact_function<-function(x){
  return(as.numeric(factor(x, ordered = TRUE)))
}

char_data<-lapply(char_cols, fact_function)
char_data<-as.data.frame(char_data, col.names = colnames(char_cols))
head(char_data)

#Final data set
data<-cbind.data.frame(num_data, char_data)
head(data)

#Fitting Linear Regression Model
linearmodel<-lm(data$default~., data = data)

summary(linearmodel)

#multiple R-squared tells us that 20.12% variation in y can be explained by x variables


###checking for homoskedasticity 
par(mfrow = c(2,2))
plot(linearmodel)

# second regressive model for default against significant variables

data2 <- data[ , c("default", "checking_balance", "checking_balance1", "months_loan_duration", "checking_balanceunknown", "credit_historyfully repaid", "credit_historyfully repaid this bank", "credit_historyrepaid ", "purposeeducation ", "savings_balanceunknown ", "installment_rate", "personal_status", "foreign_worker")]
linear_model2<-lm(data2$default~., data = data2)

summary(linear_model2)


