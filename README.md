# Doing Data Science Case Study 2 Employee Data Analysis  

The case study includes the following topics  
  a. Overview  
  b. Attrition Analysis  
  c. Monthly Income Analysis  
  d. Job Role related Trends  
  e. Other Trends  
  f. Recommendations  
  
  ##Overview  
  Data Provided: Employee Data Set  
  We have 870 observations and 35 features in Employee data  
  Most of the features are either integers or factors(string & numeric).  
  
  ##Attrition Analysis   
  Using the 36 features available we will find out the features contributing to attrition
  Features considered are Work Life Balance, Job Involvement, Job Satisfaction, Over Time, Job Level, Environment Satisfaction, Number of Companies Worked, Total Working Years & Marital Status  
  Method used for Top Feature Selection: Linear Regression & t-test  
  
  ### Top 3 Factors in Employee Attrition Analysis  

After fitting the factors such as Work Life Balance, Job Involvement, Job Satisfaction, Over Time, Environment Satisfaction, Number of Companies Worked, Total Working Years, Marital Status on a linear regression plane, we evaluated the summary using t-test.  

Identified the top 3 factors by looking at the t-score and p value(higher the t-value and lower the probability means those features are strong determining factors for Attrition)  

Top 3 Factors found from this data set are Over Time, Total Working Years and Job Involvement  

### Employee Attrition Model  

Top 3 factors in Employee data are used to predict Attrition  

The dataset is imbalanced on Attrition(Yes-16%; No-84%), so we tried oversampling and under sampling techniques on this data set.  

Oversampling technique was found to be more effective for train-test phase for the classification models.  

K Nearest Neighbor(KNN) Model performed better compared to Naive Bayes. So we are using KNN for the classification.  

Below code is used to find mean accuracy for both KNN and Naive Bayes by iterating them 100 times using various train-test samples from an over sampled data set.  

KNN model with K=2 provided best Accuracy.  

## Employee Monthly Income Analysis  

Using 870 observations and 35 features in Employee Dataset, we trying to figure out the features contributing to Monthly Income  
Features considered are Department, Job Role, Job Level, Total Working Years, Age, Education, Years At Company, Years In Current Role, Years With Current Manager  

Method used for Top Feature Selection: Correlation Matrix using ggcorrplot, Linear Regression & t-test   

We will find top 3 factors from the set of set of features  

### Top 3 Factors in Employee Monthly Income Analysis  

After fitting the factors such as Department, Job Role, Job Level, Total Working Years, Age, Education, Years At Company, Years In Current Role and Years With Current Manager on a linear regression plane, we evaluated the summary using t-test.  

Identified the top 3 factors by looking at the t score and p value(higher the t-value and lower the probability help us understand that those features are strong determining factors for Monthly Income)  

Top 3 Factors found from this data set are Job Level, Total Working Years and Job Role  

### Employee Monthly Income Model  

Top 3 factors in Employee data are used to predict Monthly Income  
  
Linear Regression Model is used to predict the values  

## Job Role related Trends

  
For various job roles, we looked at number of years employee worked in a company. The evidence suggest that most of the employees are leaving the company within first 10 years with exceptions to the Job Roles such as  ‘Manager’ and ‘Research Director’  
  
## Other related Trends  

How Attrition is related to other factors such as Business Travel, Stock Option Level, Job satisfaction and Work life balance  

Evidence suggests that  

-Employees who travel frequently as part of their job are likely to leave the company more often  

-Employees who have no stock options tend to  leave the company more often  

-Other factors such as low job satisfaction and low work life balance force employees to leave the company  

## Recommendations  

To retain employees, Company should  
  
- Provide required compensation to employees who do over time or travel frequently  
  
- Provide basic stock option level(1)  
  
- Figure out the more common issues related to low job involvement and work life balance and find solutions  
  
- Pay scale and promotions for employees below the age of 35 should be reviewed/revised  
  
- Newly joined(< 1 year) employees should be provided with perks, recognition and rewards  
  
## Summary  
  
As per our analysis,   
  
- There are multiple factors related to Attrition and top ones are Over Time, Job Involvement and Total working years  
  
- Top factors driving monthly income are Job level, Job Role and Total working years  
  
- For most of the job roles, employees tend to leave the company within 10 years for their service at the company  
  
- Frito Lay to consider one or more of the recommendations  