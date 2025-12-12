read.csv("loan_data.csv")

df <- read.csv("loan_data.csv")
#print original dataframe
print(df)
#stats about original dataframe
head(df)

#
summary(df$loan_amnt)
summary(df$person_income)
#initial boxplot
boxplot(df$loan_amnt)

#Subset of two columns(that we are interested in)
new_df <- subset(df, select = c(person_income, loan_amnt))
#print subset of df
print(new_df)


#Removing outliers
Q1 <- apply(new_df, 2, quantile, 0.25)
Q3 <- apply(new_df, 2, quantile, 0.75)
IQR <- Q3 - Q1
IQR

#outliers
otls <- (new_df < (Q1 - 1.5 * IQR)) | (new_df > (Q3 + 1.5 * IQR))
head(otls)
#Dataframe after removing outliers
d_c <- new_df[!apply(otls, 1, any), ]
head(d_c)

#Summary after removing outliers
summary(d_c $loan_amnt)
summary(d_c $person_income)


#Draw Histogram with normal curve
hist <- hist(
  d_c$loan_amnt,
  main = 'Loan amount distribution',
  xlab = 'Loan amount',
  ylim =c(0,2500),
  xlim=c(0,36000),
  col='yellow',
  breaks = 25,
  
)

xfit<-seq(min(d_c$loan_amnt),max(d_c$loan_amnt),length=40)
yfit<-dnorm(xfit,mean=mean(d_c$loan_amnt),sd=sd(d_c$loan_amnt))
yfit <- yfit*diff(hist$mids[1:2])*length(d_c$loan_amnt)

lines(xfit, yfit, col="red", lwd=3)


### Scatter plot
plot(
  d_c$loan_amnt, 
   d_c$person_income,
   xlab = 'Loan Amount',
   ylab = 'Personal Income',
   main = 'Loan amount vs Personal Income',
  pch=20
)

model <- lm(d_c$person_income ~ d_c$loan_amnt, data = d_c)
abline(model, col ='red')
#Correlation test
cor.test(d_c$loan_amnt, d_c$person_income, method = "spearman")
#Box plot to show spread
boxplot(d_c$loan_amnt)




