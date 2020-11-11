#Assignment 1
#SS 3859
#Yizhou Tang
#250888541

#Question 1

#H0: u = 16, H1: u <16, alpha = 0.05
# The test is one-sided.
x = c(12.21, 14.37, 17.18, 11.74, 13.84, 14.26, 15.42, 13.52, 17.97)

sample_mean = mean(x) # x_bar
sample_sd = sd(x) # s

t_stat = (sample_mean - 16)/(sample_sd/sqrt(9)) # (x_bar-mu0)/(s/sqrt(n))
t_stat # test statistic

# calculating the p_value
A = 1 - pt(abs(t_stat),df=8) # function "abs" gives the absolute value.
p_val = A #One sided
p_val # If p_val < alpha (0.05), reject H0

cv = qt(0.95,8) # Gives t_value at which the p-value becomes 0.05
abs(t_stat) > cv # TRUE means |t_stat| was greater than the critical value -> Evidence against H0


#Question 4

hw1_data = read.csv("https://raw.githubusercontent.com/hgweon2/ss3859/master/hw1_data1.csv")

# a)
# Count the number of observations whose x1 are greater than 6
length(hw1_data$x1[hw1_data$x1>6])

# b)Count the number of observations whose x1 are greater than 6 and x2 equal to H
temp <- hw1_data[hw1_data$x1>6,]
length(temp$x2[temp$x2 == "H"])

# c)Consider a subset A that contains all observations with x2 = H. Compute the mean,
#median and standard deviation of the x1 values in subset A.
A <- hw1_data[hw1_data$x2 == "H",]
mean(A$x1) #mean
median(A$x1) #median
sd(A$x1) #standard deviation

# d)The sample mean of x1 is 4.435. Can we argue that the true mean of x1 differs from 4?
#Conduct a t-test at significance level ?? = 0.05. Give the test statistic (t-value), p-value and
#your conclusion

sample_mean = 4.435 # x_bar
sample_sd = sd(hw1_data$x1) # s
n = length(hw1_data$x1)#n
t_stat = (sample_mean - 4)/(sample_sd/sqrt(n)) # (x_bar-mu0)/(s/sqrt(n))
t_stat # test statistic

# calculating the p_value
A = 1 - pt(abs(t_stat),df=n-1) # function "abs" gives the absolute value.
p_val = 2*A #Two sided
p_val 
# Conclusion: Since p_val > alpha (0.05), failed to reject H0

# e)Consider the statement: "Given that x2 equals to H, the true mean of x1 is larger than
#4." Is this statement convincing? Use a t-test (?? = 0.05) to support your answer. 

#Hypothesis: 
#H0: mu = 4, H1: mu >4

x = hw1_data[hw1_data$x2 == "H",]
x = x$x1
sample_mean = mean(x) # x_bar
sample_sd = sd(x) # s
n = length(x)#n
t_stat = (sample_mean - 4)/(sample_sd/sqrt(n)) # (x_bar-mu0)/(s/sqrt(n))
t_stat # test statistic

cv = qt(0.95,n-1) #Critical value
cv
abs(t_stat) > cv 
# Conclusion: Since it is TRUE, there is evidence against H0, null hypothesis is rejected,
# Hence, the gievn statement is convicing.

#5)
set.seed (50)
idx = sample(nrow(cars),40,replace=FALSE)
cars2 = cars[idx,]

y = cars2$dist
x = cars2$speed
n = 40
#a)
plot(cars2$speed,cars2$dist)
# According to the plot, it appears that there is a linear relationship between speed and dist

#b)
cars_lm = lm(dist~speed,data=cars2) 
fitted_y = cars_lm$fitted.values
summary(cars_lm)
#B0 =  -17.2369
#B1 = 3.8820

#Unbiased sigma^2 estimator from LSE
est_var = sum((y-fitted_y) ^2)/(n-2)
est_var
#Unbiased estimate of sigma^2 is 173.4714

#c)
#Residuals e4,e7, e10:
e4 = y[4] - fitted_y[4]
e7 = y[7] - fitted_y[7]
e10 =  y[10] - fitted_y[10]
e4
e7
e10

#d) 
residuals = y - fitted_y
#residuals whose absolute values are greater than 20:
residuals[abs(residuals) > 20]
idx = abs(residuals)>20

plot(cars2$speed,cars2$dist)
points(cars2$speed[idx],cars2$dist[idx], col = "red",pch = 18,cex=2)

#e)
sum(y-fitted_y)
#sum of residuals = 0, as expected

#f)
#Report the fitted model:
summary(cars_lm)
#Add a fitted regression line
abline(cars_lm)
#Predict the distance when the speed is 17
predict(cars_lm,newdata=data.frame(speed=17))

#Distance is estimated to be 48.7571 when sped is 17

#g)
#To measure goodness of fit, we can use R^2:
summary(cars_lm)$r.squared 
#According to R^2,64% of the variation is explained by the model

#h) 
# This statement is assuming that the model is able to predict every outcome with residual = 0; which is very unlikely to happen. Include a confidence interval for E(Y|x = 100) could be a way to provide a better description for the model.

#i)
#Confidence interval for the beta parameters
confint(cars_lm,level = 0.90)
#Confidence for B1 is ( 3.089992,  4.673977)

#j)
# Confidence interval for the mean response at speed=15
predict(cars_lm,newdata=data.frame(speed=15),interval="confidence",level=0.95)
#Confidence interval for E(Y|x = 15) is (35.89159,46.09413)






