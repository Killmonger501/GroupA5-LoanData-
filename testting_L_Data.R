read.csv("Book1.csv")

df <- read.csv("Book1.csv")
#print original dataframe
print(df)
#stats about original dataframe
head(df)

#Subset of two columns(that we are interested in)
new_df <- subset(df, select = c(person_income, loan_amnt))
#print subset of df
print(new_df)
#
#hist(new_df$loan_amnt)

#plot(new_df$loan_amnt ~ new_df$person_income)

#Remove outliers
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

#Draw Histogram with normal curve
hist(
  d_c$loan_amnt,
  probability = T#,
  #ylim = c(0,4000)
  , main = 'Histogram of Loan amount',
  xlab = 'Loan amount'
)
#lines(density(d_c$loan_amnt, na.rm = TRUE))
curve(dnorm(x, mean = mean(d_c$loan_amnt, na.rm = TRUE),
            sd = sd(d_c$loan_amnt, na.rm = TRUE)), add = TRUE, col = 'red')



###
lines(density(d_c$loan_amnt))

hist(d_c$person_income)
plot(d_c$loan_amnt, d_c$person_income)

plot(
  d_c$loan_amnt, 
   d_c$person_income,
   xlab = 'Loan Amounts',
   ylab = 'Personal Income',
   main = 'Scatter plot of Loan amount and personal income'
)

model <- lm(d_c$person_income ~ d_c$loan_amnt, data = d_c)
abline(model, col ='red')

cor.test(d_c$loan_amnt, d_c$person_income, method = "spearman")

boxplot(d_c$loan_amnt)

summary(df$loan_amnt)
summary(df$person_income)

lines(density(d_c$loan_amnt))

####Section 2
#Heat map
#dataframe of 
#new_df1 <- subset(df, select = c(person_age, person_income, loan_amnt, person_emp_exp, credit_score ))

#correlation matrix
#cor_mat <- round(cor(new_df1))
#head(cor_mat)

#correlation Heatmap
#library(tidyverse)
#gat_cor_mat <- gather(cor_mat)
#head(gat)


