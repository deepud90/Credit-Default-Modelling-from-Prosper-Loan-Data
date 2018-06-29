# Modelling_Prosper
Deepu  
5/29/2018  




```r
library(dplyr)
library(ggplot2)
library(knitr)
library(data.table)
library(ROCR)
library(caTools)
library(corrplot)
library(ggcorrplot)
library(woeBinning)
library(MASS)
```

## R Markdown
So far we have performed the exploratory analysis of the prosper loan data, and have come up with a number of insights. <describe a short summary > 
Let us get into the predictive modelling part now. We have come to realize the difficulty in exploring the data when the number of features are large - It is hard to say conclusively exactly what features determine a loan default. When building a predictive model, it is important for us to have the right set of features, else the development of a robust model becomes difficult. As a start, one possibility is to include all the features in the model, and let the algorithm determine the most important features. However, there are some other approaches that have been used in the credit risk modelling world. Here I will be describing one such approach

### 1)  Variable importance and feature selection from univariate Gini

The performance of a classifier is often described with the AUC or Area Under the Curve metric. It is basically a measure of the area under the True Positive Rate - False Positive Rate curve of a classifier. The higher the AUC, the better the classifier performance. In Credit Risk modelling, model performances are often reported using Somers'D or Gini. Gini is an effective measure of the effectiveness of the model in discriminating between goods and bads. For a binary classifier such as the one we will be building, a 'Lift curve' or 'Lorenz curve' represents an increase in the positive class's occurence rate due to the predictive model over a random guess. The Lorenz curve can be seen in the link below
 https://stats.stackexchange.com/questions/24325/lorenz-curve-and-gini-coefficient-for-measuring-classifier-performance?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

Gini is  the area between the Lorenze Curve and the bottom left to top right diagonal divided by the area under the bottom left to top right diagonal. So in the graph above this works out as (Area A)/(Area A + AreaB).
Note that mathematically the value of Gini can be calculated as 2*AUC - 1, where AUC is the area under the curve of an ROC curve. Another statistic that mathematically assumes the same value as Gini is Somers' D. Technically, Somers' D  can be defined as (the number of Concordant Good Bad Pairs - the Number of Discordant Pairs) / (Total Number of Pairs Including Ties). A concordant pair is a good bad pair where the Good (or event, or class 1 in most binary classifier) has a higher predicted probability than the bad (or non event, or class 0), a discordant pair is where the good has a predicted probability score than the bad and a tie is where the good has and equal score to the bad. 

Now, using this metric, we can acutally find the importance of a variable in our model. This is done by taking each of our feature individually, and fitting a logistic regression model using only that feature. The AUC/Gini of the resulting model for each variable is then recorded. (Hence the term univariate Gini) A variable with a higher predictive power will exhibit a higher gini, and in turn can be said to be more important for our model. Now before proceeding to building our model, one more concept needs to be introduced 

Information Value and Weight of Evidence:

The weight of evidence and information value indicates the predictive power of an independent variable in relation to the dependent variable. It is considered a measure of the separation of events and non events in a binary classification model. It is calculated by taking the natural logarithm (log to base e) of division of % of non-events and % of events. Equation for weight of evidence can be seen at :

https://www.listendata.com/2015/03/weight-of-evidence-woe-and-information.html
        
The calculation of WOE is done as follows. In case of continuous variables, the data is split into 10 deciles or lesser depending on the distribution. The number of events and non-events in each decile (bin) is then calculated, along with the percentage of events and non-events in each decile out of the total. The WOE is then calculated for each bin by taking the natural log of the % of events and non-events. In case of categorical variables the process is the same, except that the variable needn't be divided as in the case of continuous variables.

In case of binary classification problems (and specially in the domain of credit Risk modelling), it is a common practice to replace the raw features with their weight of evidence values which are instead used in creating the predictive model. An advantage of this approach is that it clubs together values in the form of bins, which might make the predictive power of the variable stronger, as well as doing away with any non-linearity in the relationship of the feature with the target variable. This is more significant in case of logistic regression models which assumes a linear relationship between the response variable and the predictor. 

Let us now apply the above concepts to our dataset. Before taking on the modelling aspect, we will need to perform data manipulation operatinons. The data set at hand contains loans which were currently active at the time of preparation of the same, as well as those that were compeleted/defaulted. From the perspective of credit default modelling problem, the loans with active statuses are not useful, since we wish to build a model that can identify which of the loans would default. For training such a model, we need to know if he loans eventually completed without default or not. Hence, let us first segreate the completed loans from the active loans. But first let us look at the percentage of loans by LoanStatus

Around 50% of the dataset comprises of loans that are currently active. We will have to remove these loans and work with the remaining data. Basically the categories which we are interested in are 'Chargedoff','Defaulted' and 'Completed'. Hence we will use only these loans for our predictive model

```r
loans = read.csv("prosperLoanData.csv",na.strings=c("NA",""))
loans_sub  =  loans[!(loans$LoanStatus %in% c("Current","Cancelled",
                                                "Past Due (1-15 days)",
                                                "Past Due (16-30 days)",
                                                "Past Due (31-60 days)",
                                                "Past Due (61-90 days)",
                                                "FinalPaymentInProgress")),]
```
Selecting for only the above mentioned loan statuses leaves about 55404 rows. Now this modelling dataset has about 30% of the loans defaulted which is fairly high in comparison to real life scenarios. However, we will still have to make do with the data that we have. After selecting the dataset, some checks for the data quality are performed. Listed below are the percentage of missing values in each column in the dataset 


```r
#Check for missing values

mis_val = colSums(is.na(loans_sub))/nrow(loans_sub)*100
sort(mis_val,decreasing = T)
```

```
##         ScorexChangeAtTimeOfListing                   TotalProsperLoans 
##                          80.9797127                          80.8010252 
##          TotalProsperPaymentsBilled               OnTimeProsperPayments 
##                          80.8010252                          80.8010252 
## ProsperPaymentsLessThanOneMonthLate     ProsperPaymentsOneMonthPlusLate 
##                          80.8010252                          80.8010252 
##            ProsperPrincipalBorrowed         ProsperPrincipalOutstanding 
##                          80.8010252                          80.8010252 
##                            GroupKey       LoanFirstDefaultedCycleNumber 
##                          77.1225904                          69.4173706 
##             EstimatedEffectiveYield                       EstimatedLoss 
##                          52.4853801                          52.4853801 
##                     EstimatedReturn             ProsperRating..numeric. 
##                          52.4853801                          52.4853801 
##               ProsperRating..Alpha.                        ProsperScore 
##                          52.4853801                          52.4853801 
##                         CreditGrade                    AmountDelinquent 
##                          47.7510649                          13.7499098 
##            EmploymentStatusDuration                  CurrentCreditLines 
##                          13.7372753                          13.7174211 
##                     OpenCreditLines           PublicRecordsLast12Months 
##                          13.7174211                          13.7174211 
##              RevolvingCreditBalance                 BankcardUtilization 
##                          13.7174211                          13.7174211 
##             AvailableBankcardCredit                         TotalTrades 
##                          13.6091257                          13.6091257 
##  TradesNeverDelinquent..percentage.             TradesOpenedLast6Months 
##                          13.6091257                          13.6091257 
##                       BorrowerState                   DebtToIncomeRatio 
##                           9.9487402                           7.6871706 
##                          Occupation                    EmploymentStatus 
##                           4.0881525                           4.0646885 
##                      TotalInquiries             DelinquenciesLast7Years 
##                           2.0846870                           1.7814598 
##             FirstRecordedCreditLine          TotalCreditLinespast7years 
##                           1.2562270                           1.2562270 
##                InquiriesLast6Months                CurrentDelinquencies 
##                           1.2562270                           1.2562270 
##            PublicRecordsLast10Years               CreditScoreRangeLower 
##                           1.2562270                           1.0649051 
##               CreditScoreRangeUpper                          ClosedDate 
##                           1.0649051                           0.5775756 
##                         BorrowerAPR                          ListingKey 
##                           0.0451231                           0.0000000 
##                       ListingNumber                 ListingCreationDate 
##                           0.0000000                           0.0000000 
##                                Term                          LoanStatus 
##                           0.0000000                           0.0000000 
##                        BorrowerRate                         LenderYield 
##                           0.0000000                           0.0000000 
##           ListingCategory..numeric.                 IsBorrowerHomeowner 
##                           0.0000000                           0.0000000 
##                    CurrentlyInGroup                    DateCreditPulled 
##                           0.0000000                           0.0000000 
##               OpenRevolvingAccounts         OpenRevolvingMonthlyPayment 
##                           0.0000000                           0.0000000 
##                         IncomeRange                    IncomeVerifiable 
##                           0.0000000                           0.0000000 
##                 StatedMonthlyIncome                             LoanKey 
##                           0.0000000                           0.0000000 
##           LoanCurrentDaysDelinquent          LoanMonthsSinceOrigination 
##                           0.0000000                           0.0000000 
##                          LoanNumber                  LoanOriginalAmount 
##                           0.0000000                           0.0000000 
##                 LoanOriginationDate              LoanOriginationQuarter 
##                           0.0000000                           0.0000000 
##                           MemberKey                  MonthlyLoanPayment 
##                           0.0000000                           0.0000000 
##                 LP_CustomerPayments        LP_CustomerPrincipalPayments 
##                           0.0000000                           0.0000000 
##                  LP_InterestandFees                      LP_ServiceFees 
##                           0.0000000                           0.0000000 
##                   LP_CollectionFees               LP_GrossPrincipalLoss 
##                           0.0000000                           0.0000000 
##                 LP_NetPrincipalLoss     LP_NonPrincipalRecoverypayments 
##                           0.0000000                           0.0000000 
##                       PercentFunded                     Recommendations 
##                           0.0000000                           0.0000000 
##          InvestmentFromFriendsCount         InvestmentFromFriendsAmount 
##                           0.0000000                           0.0000000 
##                           Investors 
##                           0.0000000
```

We see that there are many columns in our data with fairly large number of missing values. Columns with large number of missing values does not actually prove very useful for our modelling purporses. Hence, those columns which have at least 20% of the data missing are dropped. The only exceptions are the columns "ProsperScore", "CreditGrade","ProsperRating..nummeric" and "ProsperRating..Alpha." These columns indicate the different metrics used by prosper to rate the risk factor of the loan during different time periods - pre-2009 (CreditGrade, ProsperScore) and post-2009 (ProserRating..numeric and ProsperRating..Alpha.). Hence depending on the time of origination of the loan, one set of ratings might be missing. 


```r
columns_to_impute = mis_val[mis_val>0 & mis_val<20]
columns_to_drop = mis_val[mis_val>=20]
columns_to_drop = columns_to_drop[!(names(columns_to_drop) %in% c("ProsperScore","CreditGrade",
                                           "ProsperRating..numeric.","ProsperRating..Alpha."
                                           ))]
```

# Missing Value Imputation

The character variables are imputed with the most frequent values, whereas the numeric variables are impute with the median value in case of CreditScoreRangeLower,CreditScoreRangeUpper and EmploymentStatus duration. The rest of the numeric variables are imputed with zero. 

```r
columns_to_impute  = c("BorrowerAPR",
              "CurrentCreditLines","OpenCreditLines",
              "TotalCreditLinespast7years","InquiriesLast6Months",
              "TotalInquiries","CurrentDelinquencies","AmountDelinquent",
              "DelinquenciesLast7Years","PublicRecordsLast10Years",
              "PublicRecordsLast12Months","RevolvingCreditBalance",
              "BankcardUtilization","AvailableBankcardCredit","TotalTrades",
              "TradesNeverDelinquent..percentage.","TradesOpenedLast6Months",
              "DebtToIncomeRatio")

for (column in columns_to_impute){
  if (sum(is.na(loans_sub[[column]]))>0)
    loans_sub[is.na(loans_sub[[column]]),column]=0
 
}

med_CredScoreRangeLower = median(loans_sub$CreditScoreRangeLower,na.rm=T)
med_CredScoreRangeUpper = median(loans_sub$CreditScoreRangeUpper,na.rm=T)
loans_sub[is.na(loans_sub$CreditScoreRangeLower),"CreditScoreRangeLower"]=med_CredScoreRangeLower
loans_sub[is.na(loans_sub$CreditScoreRangeUpper),"CreditScoreRangeUpper"]=med_CredScoreRangeUpper


loans_sub$BorrowerState[is.na(loans_sub$BorrowerState)]="CA"
loans_sub$Occupation[is.na(loans_sub$Occupation)]="Other"
loans_sub$EmploymentStatus[is.na(loans_sub$EmploymentStatus)]="Employed"

loans_sub$EmploymentStatusDuration[is.na(loans_sub$EmploymentStatusDuration)]=
median(loans_sub$EmploymentStatusDuration,na.rm = T)
```


```r
loans_sub = subset(loans_sub,select=(!(names(loans_sub) %in% names(columns_to_drop))))
loans_sub = subset(loans_sub,select=(!(names(loans_sub)%in%c("ListingKey","ListingNumber",
                                                              "ListingCreationDate","ClosedDate",
                                                              "DateCreditPulled","FirstRecordedCreditLine",
                                                              "LoanKey","LoanNumber","MemberKey",
                                                              "LoanOriginationDate","LoanOriginationQuarter",
                                                              "ProsperRating..numeric.","ProsperScore",
                                                              "CreditGrade","ProsperRating..Alpha."))))
```


```r
sort(colSums(is.na(loans_sub)),decreasing =T)
```

```
##                               Term                         LoanStatus 
##                                  0                                  0 
##                        BorrowerAPR                       BorrowerRate 
##                                  0                                  0 
##                        LenderYield          ListingCategory..numeric. 
##                                  0                                  0 
##                      BorrowerState                         Occupation 
##                                  0                                  0 
##                   EmploymentStatus           EmploymentStatusDuration 
##                                  0                                  0 
##                IsBorrowerHomeowner                   CurrentlyInGroup 
##                                  0                                  0 
##              CreditScoreRangeLower              CreditScoreRangeUpper 
##                                  0                                  0 
##                 CurrentCreditLines                    OpenCreditLines 
##                                  0                                  0 
##         TotalCreditLinespast7years              OpenRevolvingAccounts 
##                                  0                                  0 
##        OpenRevolvingMonthlyPayment               InquiriesLast6Months 
##                                  0                                  0 
##                     TotalInquiries               CurrentDelinquencies 
##                                  0                                  0 
##                   AmountDelinquent            DelinquenciesLast7Years 
##                                  0                                  0 
##           PublicRecordsLast10Years          PublicRecordsLast12Months 
##                                  0                                  0 
##             RevolvingCreditBalance                BankcardUtilization 
##                                  0                                  0 
##            AvailableBankcardCredit                        TotalTrades 
##                                  0                                  0 
## TradesNeverDelinquent..percentage.            TradesOpenedLast6Months 
##                                  0                                  0 
##                  DebtToIncomeRatio                        IncomeRange 
##                                  0                                  0 
##                   IncomeVerifiable                StatedMonthlyIncome 
##                                  0                                  0 
##          LoanCurrentDaysDelinquent         LoanMonthsSinceOrigination 
##                                  0                                  0 
##                 LoanOriginalAmount                 MonthlyLoanPayment 
##                                  0                                  0 
##                LP_CustomerPayments       LP_CustomerPrincipalPayments 
##                                  0                                  0 
##                 LP_InterestandFees                     LP_ServiceFees 
##                                  0                                  0 
##                  LP_CollectionFees              LP_GrossPrincipalLoss 
##                                  0                                  0 
##                LP_NetPrincipalLoss    LP_NonPrincipalRecoverypayments 
##                                  0                                  0 
##                      PercentFunded                    Recommendations 
##                                  0                                  0 
##         InvestmentFromFriendsCount        InvestmentFromFriendsAmount 
##                                  0                                  0 
##                          Investors 
##                                  0
```

After removing the columns with over 20% data missing, and imputing the remaining, we are left with a dataset that still has some columns that are not quite useful for our modelling activity. They are the following variables
- [ListingKey,ListingNumber,ListingCreationDate,ClosedDate,DateCreditPulled,FirstRecordedCreditLine,LoanKey,LoanNumber,MemberKey,LoanOriginationDate,LoanOriginationQuarter,ProsperRating..numeric.,ProsperScore,CreditGrade,ProsperRating..Alpha.]. These are either date variables or unique identification keys kind of variables, except the last four. The last four variables are excluded since the are already risk scores assigned by Prosper to the loan, which is something we are trying to replicate in our modelling work. Including them would favorably bias our model considerably, and are hence removed. 
With the dataset prepared we proceed to the modelling part. As discussed earlier, we would be using feature selection by looking at the univariate gini of the individual features. In order to automate this process, a function was first created to calculate the univariate gini, as shown below


```r
cal_gini = function(var_string,df_train,df_test){ 
  formula = paste0('target~',var_string) 
  model= glm(as.formula(formula),data=df_train,family=binomial(link='logit')) 
  predictinos = predict(model,newdata = df_test,type='response') 
  pred = prediction(predictinos,df_test$target) 
  auc.perf = performance(pred,measure = 'auc') 
  auc = auc.perf@y.values 
  gini = 2*auc[[1]]-1 
  return(gini) 
}
```

Next we will loop through all the individual variables and calculate the univariate gini by fitting a univariate logistic regression model on the training set and validating it on the test set. Thus, this process requires us split our dataset into a training set and a test set. The train-test split can be done in R using the library caTools, which will maintain the same proportion of the binary variable in the training and test set. After creating the training and test dataset, the process of calculating univariate gini was performed. Shown below are the results from the exercise. 


```r
loans_sub$target = 1
loans_sub$target[loans_sub$LoanStatus=="Completed"]=0
```


```r
set.seed(123)
split = sample.split(loans_sub$target,SplitRatio=0.7) 
train = subset(loans_sub,split==TRUE) 
test = subset(loans_sub,split==FALSE)
```


```r
gini_list = c()
variables_list = names(loans_sub)

for (variable in variables_list)
{
#  cat("Now Calcualting Gini for ",variable)
  gini_var = cal_gini(variable,train,test)
#  cat('\n')
#  cat("Gini for ",variable,"=",gini_var)
#  cat('\n')
  gini_list = append(gini_list,gini_var)
  
}
names(gini_list)=variables_list
```


Now let us have a look at the univariate gini.


```r
sort(gini_list,decreasing = T)
```

```
##                         LoanStatus          LoanCurrentDaysDelinquent 
##                        1.000000000                        1.000000000 
##              LP_GrossPrincipalLoss                LP_NetPrincipalLoss 
##                        0.977880362                        0.960376996 
##       LP_CustomerPrincipalPayments                LP_CustomerPayments 
##                        0.735382301                        0.613730100 
##                       BorrowerRate                        LenderYield 
##                        0.329303727                        0.325735466 
##              CreditScoreRangeLower              CreditScoreRangeUpper 
##                        0.307791300                        0.307791300 
##                        BorrowerAPR                  LP_CollectionFees 
##                        0.293820596                        0.249284147 
##               InquiriesLast6Months            AvailableBankcardCredit 
##                        0.216479241                        0.214770974 
##                     TotalInquiries    LP_NonPrincipalRecoverypayments 
##                        0.201443670                        0.192152337 
##               CurrentDelinquencies TradesNeverDelinquent..percentage. 
##                        0.178094942                        0.174063416 
##                         Occupation                        IncomeRange 
##                        0.171254934                        0.152910721 
##                     LP_ServiceFees                StatedMonthlyIncome 
##                        0.151586655                        0.149096699 
##         LoanMonthsSinceOrigination                   EmploymentStatus 
##                        0.143961642                        0.124463647 
##          ListingCategory..numeric.              OpenRevolvingAccounts 
##                        0.123869605                        0.119236896 
##            DelinquenciesLast7Years             RevolvingCreditBalance 
##                        0.115349048                        0.111389159 
##                 CurrentCreditLines                        TotalTrades 
##                        0.108860573                        0.107101719 
##                    OpenCreditLines        OpenRevolvingMonthlyPayment 
##                        0.106951810                        0.094719163 
##                   AmountDelinquent                          Investors 
##                        0.093280165                        0.090321068 
##                   CurrentlyInGroup           PublicRecordsLast10Years 
##                        0.086420388                        0.079261838 
##                      BorrowerState                 MonthlyLoanPayment 
##                        0.077414945                        0.059386631 
##            TradesOpenedLast6Months                  DebtToIncomeRatio 
##                        0.057604555                        0.054662062 
##                               Term         TotalCreditLinespast7years 
##                        0.048214288                        0.037728724 
##                IsBorrowerHomeowner                   IncomeVerifiable 
##                        0.035768718                        0.023370266 
##                 LoanOriginalAmount                 LP_InterestandFees 
##                        0.022271601                        0.020526236 
##           EmploymentStatusDuration         InvestmentFromFriendsCount 
##                        0.015409281                        0.013024052 
##        InvestmentFromFriendsAmount                    Recommendations 
##                        0.012944416                        0.011562152 
##          PublicRecordsLast12Months                      PercentFunded 
##                        0.006373005                        0.002908965 
##                             target                BankcardUtilization 
##                        0.000000000                       -0.009651335
```
At the top we see a bunch of suspiciously high gini values. This is because, these are features calculated based on the status of the loan default, and hence they would obviously have high predictive power. This also means that we will need to remove them from our model, since they are not available a-priori. So is the case with the variables such as "BorrowerRate","LenderYield","BorrowerAPR" etc. Afte eliminating those variables with such highly suspicious predictive power, we will isolate those features which are available to us a-priori as well as have high gini. For our purposes, we will take a gini as low as 2%, meaning those variables with gini lower than that will be ignored. We finally select the following variables for our predictive model


```r
predictors =
   c( 'CreditScoreRangeLower'	,
      'CreditScoreRangeUpper'	,
	    'InquiriesLast6Months'	,
      'AvailableBankcardCredit'	,
      'TotalInquiries'	,
      'CurrentDelinquencies'	,
      'TradesNeverDelinquent..percentage.'	,
      'Occupation'	,
      'IncomeRange'	,
	    'StatedMonthlyIncome'	,
      'LoanMonthsSinceOrigination'	,
      'EmploymentStatus'	,
      'ListingCategory..numeric.'	,
      'OpenRevolvingAccounts'	,
      'DelinquenciesLast7Years'	,
      'RevolvingCreditBalance'	,
      'CurrentCreditLines'	,
      'TotalTrades'	,
      'OpenCreditLines'	,
      'OpenRevolvingMonthlyPayment'	,
      'AmountDelinquent'	,
      'Investors'	,
      'CurrentlyInGroup'	,
      'PublicRecordsLast10Years'	,
      'BorrowerState'	,
      'MonthlyLoanPayment'	,
      'TradesOpenedLast6Months'	,
      'DebtToIncomeRatio'	,
      'Term'	,
      'TotalCreditLinespast7years'	,
      'IsBorrowerHomeowner'	,
      'IncomeVerifiable'	,
      'LoanOriginalAmount'	)
```

Now before we put these into our model, we need to check which ones amongst these variables are correlated with each other. Let us look first subset our dataframe with just predictors and then look at their correlation plot of the numeric variables


```r
# columns = names(loans_sub)
# col_types = (sapply(loans_sub,class))
# char_vars = col_types[(col_types)=="factor"]

loans_sub = subset(loans_sub,select=names(loans_sub) %in% append(predictors,"target"))
loans_sub_numeric = subset(loans_sub,select=!(sapply(loans_sub,class)%in%"factor"))
corr_mat = cor(loans_sub_numeric)
ggcorrplot(corr_mat)
```

![](ML_Modelling_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
Looking at the correlation plot, we do see a bunch of highly correlated variables. Based on our visual inspection of the the correlation plot, we decide to remove the following variables

```r
var_remove = c('CreditScoreRangeLower'	,
              'OpenCreditLines'	,
              'TotalCreditLinespast7years'	,
              'OpenRevolvingAccounts'	,
              'InquiriesLast6Months'	,
              'TotalTrades'	,
              'TradesNeverDelinquent..percentage.'	,
              'MonthlyLoanPayment'	,
              'Investors'	)
```


```r
#predictors = predictors[!(predictors %in% var_remove)]
predictors =
   c( 'CreditScoreRangeLower'	,
      # 'CreditScoreRangeUpper'	,
	    # 'InquiriesLast6Months'	,
      'AvailableBankcardCredit'	,
      'TotalInquiries'	,
      'CurrentDelinquencies'	,
      # 'TradesNeverDelinquent..percentage.'	,
      # 'Occupation'	,
      # 'IncomeRange'	,
	    'StatedMonthlyIncome'	,
      'LoanMonthsSinceOrigination'	,
      # 'EmploymentStatus'	,
      'ListingCategory..numeric.'	,
      # 'OpenRevolvingAccounts'	,
      'DelinquenciesLast7Years'	,
      'RevolvingCreditBalance'	,
      'CurrentCreditLines'	,
      # 'TotalTrades'	,
      # 'OpenCreditLines'	,
      'OpenRevolvingMonthlyPayment'	,
      'AmountDelinquent'	,
      # 'Investors'	,
      'CurrentlyInGroup'	,
      'PublicRecordsLast10Years'	,
      # 'BorrowerState'	,
      # 'MonthlyLoanPayment'	,
      'TradesOpenedLast6Months'	,
      'DebtToIncomeRatio'	,
      'Term'	,
      # 'TotalCreditLinespast7years'	,
      'IsBorrowerHomeowner'	,
      'IncomeVerifiable'	,
      'LoanOriginalAmount'	)
```
So far, we conducted the variable selection based on univariate gini and correlation plot. Having chosen some initial set of predictor variables, let us create the first cut of our model. However, prior to doing that let us scale the numeric predictor variables


```r
for (var in predictors){
  if (sapply(loans_sub[var],class) %in% c('integer','numeric')){
    loans_sub[var]=scale(loans_sub[var])
  }
  
}
```

Now we create the first cut of the model

```r
loans_modelling=loans_sub
#loans_modelling = subset(loans_sub,select=(names(loans_sub) %in% predictors))
#loans_modelling$target = loans_sub$target
set.seed(123)
split = sample.split(loans_modelling$target,SplitRatio=0.7) 
train = subset(loans_modelling,split==TRUE) 
test = subset(loans_modelling,split==FALSE)
var_string = paste0(predictors,collapse="+")
formula = paste0('target~',var_string) 
model= glm(as.formula(formula),data=train,family=binomial(link='logit')) 
predictinos = predict(model,newdata = test,type='response') 
pred = prediction(predictinos,test$target) 
auc.perf = performance(pred,measure = 'auc') 
auc = auc.perf@y.values 
gini = 2*auc[[1]]-1 
gini 
```

```
## [1] 0.4329354
```
The model yielded an overall gini of 0.433 which is not bad for a first cut. A gini value of 0.433 translates to an AUC value of 0.71. Although AUCs above 0.7 are considered acceptable, we can still work on improving our model. The previous model was built on the raw features without applying any transformation to it. However, the concept of weight of evidence introduced earlier can be applied to our model, which could possibly improve its performance. R provides a nice package called "woeBinning" which performs the automatic binning of the variables, as well as creation of weight f evidence values for the variables in the dataset. With the help of this package we creaete the evidence features for the variables in our dataset. For each variable in the dataset, this package creates a new variable by the name woe.<variable name>.binned, and attaches to our original dataframe. Once after applying the package we can get the follownig variables in our dataset.



```r
loans_modelling_woe = loans_modelling
binning = woe.binning(loans_modelling_woe,target.var = 'target',loans_modelling_woe,min.perc.total = 0.01,stop.limit = 0.01)
loans_modelling_woe = woe.binning.deploy(loans_modelling_woe,binning = binning,add.woe.or.dum.var = 'woe')
```

Now let us create a model with predictors as the transformed weight of evidence variables.


```r
predictors =append(predictors,c('woe.CreditScoreRangeLower.binned'		,
'woe.AvailableBankcardCredit.binned'		,
'woe.TotalInquiries.binned'		,
'woe.CurrentDelinquencies.binned'		,
'woe.Occupation.binned'		,
'woe.IncomeRange.binned'		,
'woe.StatedMonthlyIncome.binned'		,
'woe.LoanMonthsSinceOrigination.binned'		,
'woe.EmploymentStatus.binned'		,
'woe.ListingCategory..numeric..binned'		,
'woe.DelinquenciesLast7Years.binned'		,
'woe.RevolvingCreditBalance.binned'		,
'woe.CurrentCreditLines.binned'		,
'woe.OpenRevolvingMonthlyPayment.binned'		,
'woe.AmountDelinquent.binned'		,
'woe.CurrentlyInGroup.binned'		,
'woe.PublicRecordsLast10Years.binned'		,
'woe.BorrowerState.binned'		,
'woe.TradesOpenedLast6Months.binned'		,
'woe.DebtToIncomeRatio.binned'		,
'woe.Term.binned'		,
'woe.IsBorrowerHomeowner.binned'		,
'woe.IncomeVerifiable.binned'		,
'woe.LoanOriginalAmount.binned'	))
```


```r
set.seed(123)
split = sample.split(loans_modelling_woe$target,SplitRatio=0.7) 
train = subset(loans_modelling_woe,split==TRUE) 
test = subset(loans_modelling_woe,split==FALSE)
var_string = paste0(predictors,collapse="+")
formula = paste0('target~',var_string) 
model_woe= glm(as.formula(formula),data=train,family=binomial(link='logit')) #%>% stepAIC(trace = FALSE)
predictinos = predict(model_woe,newdata = test,type='response') 
pred = prediction(predictinos,test$target) 
auc.perf = performance(pred,measure = 'auc') 
auc = auc.perf@y.values 
gini = 2*auc[[1]]-1 
gini 
```

```
## [1] 0.4977377
```

With the variables having gone through a weight of evidence transformation we get a significant increase in the gini. Now let us choose the final set of predictors to create our final logistic regression model. The only difference here is that some of the weight of evidence variables have been replaced with the original variable wherever the univariate gini of the original raw variable have been found to be higher.

```r
final_predictors = c('StatedMonthlyIncome'	,
'LoanMonthsSinceOrigination'	,
'ListingCategory..numeric.'	,
'DelinquenciesLast7Years'	,
'RevolvingCreditBalance'	,
'CurrentCreditLines'	,
'OpenRevolvingMonthlyPayment'	,
'AmountDelinquent'	,
'CurrentlyInGroup'	,
'TradesOpenedLast6Months'	,
'DebtToIncomeRatio'	,
'Term'	,
'IsBorrowerHomeowner'	,
'IncomeVerifiable'	,
'LoanOriginalAmount'	,
'woe.CreditScoreRangeLower.binned'	,
'woe.AvailableBankcardCredit.binned'	,
'woe.TotalInquiries.binned'	,
'woe.CurrentDelinquencies.binned'	,
'woe.Occupation.binned'	,
'woe.IncomeRange.binned'	,
'woe.StatedMonthlyIncome.binned'	,
'woe.LoanMonthsSinceOrigination.binned'	,
'woe.EmploymentStatus.binned'	,
'woe.ListingCategory..numeric..binned'	,
'woe.AmountDelinquent.binned'	,
'woe.BorrowerState.binned'	,
'woe.Term.binned'	,
'woe.LoanOriginalAmount.binned'	)
```


```r
set.seed(123)
split = sample.split(loans_modelling_woe$target,SplitRatio=0.7) 
train = subset(loans_modelling_woe,split==TRUE) 
test = subset(loans_modelling_woe,split==FALSE)
var_string = paste0(final_predictors,collapse="+")
formula = paste0('target~',var_string) 
model_woe= glm(as.formula(formula),data=train,family=binomial(link='logit')) #%>% stepAIC(trace = FALSE)
predictinos = predict(model_woe,newdata = test,type='response') 
pred = prediction(predictinos,test$target) 
auc.perf = performance(pred,measure = 'auc') 
roccur = performance(pred,'tpr','fpr')
auc = auc.perf@y.values 
gini = 2*auc[[1]]-1 
gini 
```

```
## [1] 0.4864061
```

```r
plot(roccur)
```

![](ML_Modelling_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

Our final model also has close to 0.5 gini, and shown above is the ROC curve of the model. It is not the best classifier, however it does much better than a random classifier. 

```r
cutoffs = data.frame(prob = roccur@alpha.values[[1]],tpr=roccur@y.values[[1]],fpr=roccur@x.values[[1]])
cutoffs
```

To chose a threshold of probability above which we classify a loan as default depends on what we consider as more important in the context of the business. As we lower the threshold we can be sure of increasing our sensitivity, however, we will also increase our false positive rate significantly. Ideally we want a balance between our sensitivity and precision. For this, I set a threshold probability of 0.4


```r
library(caret)
prediction_classes = if_else(predictinos>0.4,1,0)
confusionMatrix(prediction_classes,test$target,positive = '1')
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    0    1
##          0 9193 2579
##          1 2229 2620
##                                           
##                Accuracy : 0.7107          
##                  95% CI : (0.7038, 0.7176)
##     No Information Rate : 0.6872          
##     P-Value [Acc > NIR] : 2.371e-11       
##                                           
##                   Kappa : 0.3146          
##  Mcnemar's Test P-Value : 4.824e-07       
##                                           
##             Sensitivity : 0.5039          
##             Specificity : 0.8049          
##          Pos Pred Value : 0.5403          
##          Neg Pred Value : 0.7809          
##              Prevalence : 0.3128          
##          Detection Rate : 0.1576          
##    Detection Prevalence : 0.2917          
##       Balanced Accuracy : 0.6544          
##                                           
##        'Positive' Class : 1               
## 
```
We see that for the set threshold, we get an accuracy of 0.71, a recall of 0.5 and a precision of 0.54. However, for a problem as this one, it makes more sense to look at the decile-wise percentage capture rate of defaults, for the predicted probabilities. What this essentially means is we look at the percentage of actual defaults captured by our model, in each decile of our predicted probabilities after we arrange them in ascending or descending order. This is done below


# Percentage capture rate in train and test


```r
perc.rank = function(x) trunc(rank(x))/length(x)
test$predictions = predictinos
test = within(test, perc_rank <- perc.rank(predictinos))
```


```r
perc.rank = function(x) trunc(rank(x))/length(x)
test$predictions = predictinos
test = within(test, perc_rank <- perc.rank(predictinos))
 
test$decile <- cut(test$perc_rank, breaks = seq(0, 1, 0.1))
 
 test_decilewise_capture <- test%>%
   group_by(decile)%>%
   arrange((decile))%>%
   summarise(captured = sum(target),
             total = n())%>%
   mutate(perc_captured = round(100*captured/sum(captured), 2))%>%
   as.data.frame
kable(test_decilewise_capture)
```



decile       captured   total   perc_captured
----------  ---------  ------  --------------
(0,0.1]            76    1662            1.46
(0.1,0.2]         153    1662            2.94
(0.2,0.3]         285    1662            5.48
(0.3,0.4]         388    1662            7.46
(0.4,0.5]         451    1662            8.67
(0.5,0.6]         520    1662           10.00
(0.6,0.7]         651    1662           12.52
(0.7,0.8]         724    1662           13.93
(0.8,0.9]         847    1662           16.29
(0.9,1]          1104    1663           21.23
We see that the about 64% of the defaults are captured in the first four deciles of the model. Also, in the first decile, we have 1104 defaults out of 1663 cases. This is about 66%, indicating that if the predicted probability of a loan to default falls in this decile, we have on the average 66% chance of defaulting. This measure can be used to decide if we should reject a loan, or assign a risk score based on which the interest rates will be assigned.

With the logistic model in place,we will now try an xgboost classifier. For the xgboost classifier we will first try with the raw variables and then see if woe variables improve the performance

```r
library(xgboost)
library(data.table)
library(Matrix)
library(bit64)
library(caret)

#loans_modelling_woe = read.csv('loans_modelling_woe.csv')
```


```r
library(caTools)
# Create a subset of dataframe with only the predictors
xgboost_predictors = c('StatedMonthlyIncome'	,
'LoanMonthsSinceOrigination'	,
'ListingCategory..numeric.'	,
'DelinquenciesLast7Years'	,
'RevolvingCreditBalance'	,
'CurrentCreditLines'	,
'OpenRevolvingMonthlyPayment'	,
'AmountDelinquent'	,
'CurrentlyInGroup'	,
'TradesOpenedLast6Months'	,
'DebtToIncomeRatio'	,
'Term'	,
'IsBorrowerHomeowner'	,
'IncomeVerifiable'	,
'LoanOriginalAmount'	,
'CreditScoreRangeLower'	,
'AvailableBankcardCredit'	,
'TotalInquiries'	,
'CurrentDelinquencies'	,
'Occupation'	,
'IncomeRange'	,
'StatedMonthlyIncome'	,
'LoanMonthsSinceOrigination'	,
'EmploymentStatus'	,
'ListingCategory..numeric.'	,
'AmountDelinquent'	,
'BorrowerState'	,
'Term'	,
'LoanOriginalAmount',
'woe.CreditScoreRangeLower.binned'		,
'woe.AvailableBankcardCredit.binned'		,
'woe.TotalInquiries.binned'		,
'woe.CurrentDelinquencies.binned'		,
'woe.Occupation.binned'		,
'woe.IncomeRange.binned'		,
'woe.StatedMonthlyIncome.binned'		,
'woe.LoanMonthsSinceOrigination.binned'		,
'woe.EmploymentStatus.binned'		,
'woe.ListingCategory..numeric..binned'		,
'woe.DelinquenciesLast7Years.binned'		,
'woe.RevolvingCreditBalance.binned'		,
'woe.CurrentCreditLines.binned'		,
'woe.OpenRevolvingMonthlyPayment.binned'		,
'woe.AmountDelinquent.binned'		,
'woe.CurrentlyInGroup.binned'		,
'woe.PublicRecordsLast10Years.binned'		,
'woe.BorrowerState.binned'		,
'woe.TradesOpenedLast6Months.binned'		,
'woe.DebtToIncomeRatio.binned'		,
'woe.Term.binned'		,
'woe.IsBorrowerHomeowner.binned'		,
'woe.IncomeVerifiable.binned'		,
'woe.LoanOriginalAmount.binned')

xgboost_vars = append(xgboost_predictors,'target')
loans_modelling_woe_xgb = subset(loans_modelling_woe,select=xgboost_vars)
set.seed(123)
split = sample.split(loans_modelling_woe_xgb$target,SplitRatio=0.7) 
train = subset(loans_modelling_woe_xgb,split==TRUE) 
test = subset(loans_modelling_woe_xgb,split==FALSE)

train_label = train$target
test_label = test$target

train_d =sparse.model.matrix(target~.-1,data = train)
test_d =sparse.model.matrix(target~.-1,data = test)


train_DM =xgb.DMatrix(data = train_d,label = train_label)
test_DM =xgb.DMatrix(data = test_d,label = test_label)
```



```r
param <- list(booster = "gbtree",
              objective = "binary:logistic", 
              eval_metric = 'auc',
              eta=0.1,
              max_depth=3,
              subsample=1, 
              min_child_weight=1, 
              colsample_bytree=0.6,
              #scale_pos_weight = 10,
              gamma=10,
              alpha = 0,
              nthreads=-1
              # seed = 123)
)
set.seed(123)


xgbcv <- xgb.cv( params = param, data = train_DM,
                 nrounds = 1000, nfold = 5, 
                 showsd = T, stratified = T, 
                 print_every_n = 10, 
                 prediction = TRUE,
                 early_stopping_rounds = 20
                 ,maximize = TRUE
                 
)
```

```
## [1]	train-auc:0.649839+0.004925	test-auc:0.645141+0.007128 
## Multiple eval metrics are present. Will use test_auc for early stopping.
## Will train until test_auc hasn't improved in 20 rounds.
## 
## [11]	train-auc:0.703616+0.002000	test-auc:0.696139+0.006445 
## [21]	train-auc:0.726635+0.002226	test-auc:0.717974+0.004974 
## [31]	train-auc:0.738022+0.002117	test-auc:0.728230+0.005351 
## [41]	train-auc:0.745895+0.001917	test-auc:0.735099+0.006033 
## [51]	train-auc:0.751180+0.001813	test-auc:0.739077+0.006208 
## [61]	train-auc:0.755035+0.001916	test-auc:0.741940+0.006205 
## [71]	train-auc:0.757916+0.001700	test-auc:0.744315+0.006142 
## [81]	train-auc:0.760387+0.001658	test-auc:0.746239+0.006273 
## [91]	train-auc:0.762393+0.001589	test-auc:0.747990+0.006575 
## [101]	train-auc:0.763995+0.001689	test-auc:0.749247+0.006814 
## [111]	train-auc:0.765721+0.001883	test-auc:0.750288+0.006662 
## [121]	train-auc:0.766792+0.001934	test-auc:0.750993+0.006703 
## [131]	train-auc:0.766989+0.002042	test-auc:0.751106+0.006714 
## [141]	train-auc:0.766989+0.002042	test-auc:0.751106+0.006714 
## Stopping. Best iteration:
## [124]	train-auc:0.766955+0.002040	test-auc:0.751114+0.006720
```
We will do a grid search for tuning the hyperparameters before finalizing on the model. This is shown below


```r
#####################################################  Grid-Search ##################################################
# Grid Search Algorithm ######

searchGridSubCol <- expand.grid(#subsample = c(0.5, 0.6), 
                                colsample_bytree = c(0.5, 0.6),
                                max_depth = c(3, 4,5),
                                min_child = seq(1), 
                                eta = c(0.1,0.3)
)



TunedHyperparameters = apply(searchGridSubCol, 1, function(parameterList){
  
  #Extract Parameters to test
  #currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  currentDepth <- parameterList[["max_depth"]]
  currentEta <- parameterList[["eta"]]
  currentMinChild <- parameterList[["min_child"]]
  
  param <- list(booster = "gbtree",
                objective = "binary:logistic", 
                eval_metric = 'auc',
                eta=currentEta,
                max_depth=currentDepth,
                #subsample=currentSubsampleRate , 
                min_child_weight= currentMinChild, 
                colsample_bytree=currentColsampleRate,
                #scale_pos_weight = 10,
                gamma=0)
                # seed = 123
  
  
  
  xgboostModelCV <- xgb.cv( params = param, data = train_DM,
                            nrounds = 1000, nfold = 5, 
                            showsd = T, stratified = T, 
                            print_every_n = 10, 
                            prediction = TRUE,
                            early_stopping_rounds = 20
                            ,maximize = TRUE
  )
                            xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
                            train_gini_mean <- tail(xvalidationScores$train_auc_mean, 1)
                            test_gini_mean <- tail(xvalidationScores$test_auc_mean,1)
  
  output <- return(c(train_gini_mean, test_gini_mean, currentColsampleRate, currentDepth, currentEta, currentMinChild))

})

#output <- as.data.frame(t(TunedHyperparameters))

####################################### End Grid Search ##############################################################
```
The grid searh results, (not shown here) yielded the following parameter values
eta:0.1, max_depth=4, colsample_bytree=0.6 and subsample=1

We don't see a significant improvement of the model performance through parameter tuning, nevertheless we will use the most optimum parameters. With that in place, let us now create the final xgboost model trained on the full dataset. We will also see the feature importance, as predicted by the xgboost model.


```r
nround=250 # Based on Cross Validation results
set.seed(123)


param <- list(booster = "gbtree",
              objective = "binary:logistic", 
              eval_metric = 'auc',
              eta=0.1,
              max_depth=4,
              subsample=1, 
              min_child_weight=1, 
              colsample_bytree=0.6,
              #scale_pos_weight = 10,
              gamma=0,
              alpha = 10
              # seed = 123)
)
watch_list = list(train=train_DM, test=test_DM)
mod <-xgb.train(data = train_DM,params = param,nrounds = nround,watchlist = watch_list 
                ,print_every_n = 10
                ,maximize = TRUE
                ,early_stopping_rounds = 30
)
```

```
## [1]	train-auc:0.665983	test-auc:0.660032 
## Multiple eval metrics are present. Will use test_auc for early stopping.
## Will train until test_auc hasn't improved in 30 rounds.
## 
## [11]	train-auc:0.717702	test-auc:0.710456 
## [21]	train-auc:0.739707	test-auc:0.729486 
## [31]	train-auc:0.750184	test-auc:0.738519 
## [41]	train-auc:0.756277	test-auc:0.743624 
## [51]	train-auc:0.760761	test-auc:0.747555 
## [61]	train-auc:0.764386	test-auc:0.750311 
## [71]	train-auc:0.766984	test-auc:0.752086 
## [81]	train-auc:0.769163	test-auc:0.753747 
## [91]	train-auc:0.771626	test-auc:0.755720 
## [101]	train-auc:0.773656	test-auc:0.756830 
## [111]	train-auc:0.775486	test-auc:0.757845 
## [121]	train-auc:0.777033	test-auc:0.758795 
## [131]	train-auc:0.778478	test-auc:0.759394 
## [141]	train-auc:0.779877	test-auc:0.759886 
## [151]	train-auc:0.780899	test-auc:0.760413 
## [161]	train-auc:0.782327	test-auc:0.760873 
## [171]	train-auc:0.783358	test-auc:0.761295 
## [181]	train-auc:0.784283	test-auc:0.761733 
## [191]	train-auc:0.785278	test-auc:0.761889 
## [201]	train-auc:0.786383	test-auc:0.762195 
## [211]	train-auc:0.787384	test-auc:0.762408 
## [221]	train-auc:0.788218	test-auc:0.762526 
## [231]	train-auc:0.788942	test-auc:0.762611 
## [241]	train-auc:0.789833	test-auc:0.762787 
## [250]	train-auc:0.790598	test-auc:0.763093
```


```r
imp <-xgb.importance(feature_names = train_d@Dimnames[[2]],model = mod)

#features <-head(imp,80)

xgb.plot.importance(imp,top_n = 30,rel_to_first = TRUE)
```

![](ML_Modelling_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

```r
#colnames(train_d)

pred_train = predict(object = mod,newdata = train_d)
pred_test = predict(object = mod,newdata = test_d)
```
The plot above shows the feature importances from the xgboost model. Obvious enough, that CrediScoreRange variable turns out to be the most significant of the lot, that too by a significant margin. This is followed by other variables, of which notable ones are  Total inquiries, Loan Amount, Current Delinquencies etc. An interesting thing to note is that none of the weight of evidence variables come at the top in importance, indicating that weight of evidence transformation does not really help in xgboost models.

Like with logistic regression earlier, we can set a threshold of our probabilities for our classifier and get the model performance metrics such as sensitivity, specificity etc. An 



```r
prediction_classes = if_else(pred_test>0.4,1,0)
confusionMatrix(factor(prediction_classes,levels = min(prediction_classes):max(prediction_classes)),
                factor(test$target,levels=min(test$target):max(test$target)),positive = '1')
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    0    1
##          0 9263 2426
##          1 2159 2773
##                                           
##                Accuracy : 0.7241          
##                  95% CI : (0.7173, 0.7309)
##     No Information Rate : 0.6872          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.3492          
##  Mcnemar's Test P-Value : 8.552e-05       
##                                           
##             Sensitivity : 0.5334          
##             Specificity : 0.8110          
##          Pos Pred Value : 0.5622          
##          Neg Pred Value : 0.7925          
##              Prevalence : 0.3128          
##          Detection Rate : 0.1668          
##    Detection Prevalence : 0.2967          
##       Balanced Accuracy : 0.6722          
##                                           
##        'Positive' Class : 1               
## 
```

At a threshold of 0.4, we seem to be getting the best precision-recall tradeoff. However, as mentioned earlier, it is better to look at the percentage capture rate according to the model deciles. This is done below

```r
perc.rank = function(x) trunc(rank(x))/length(x)
test$predictions = pred_test
test = within(test, perc_rank <- perc.rank(pred_test))
 
test$decile <- cut(test$perc_rank, breaks = seq(0, 1, 0.1))
 
 test_decilewise_capture <- test%>%
   group_by(decile)%>%
   arrange((decile))%>%
   summarise(captured = sum(target),
             total = n())%>%
   mutate(perc_captured = round(100*captured/sum(captured), 2))%>%
   as.data.frame
kable(test_decilewise_capture)
```



decile       captured   total   perc_captured
----------  ---------  ------  --------------
(0,0.1]            53    1662            1.02
(0.1,0.2]         155    1662            2.98
(0.2,0.3]         265    1662            5.10
(0.3,0.4]         342    1662            6.58
(0.4,0.5]         443    1662            8.52
(0.5,0.6]         506    1662            9.73
(0.6,0.7]         641    1662           12.33
(0.7,0.8]         732    1662           14.08
(0.8,0.9]         877    1662           16.87
(0.9,1]          1185    1663           22.79


The capture rate is very similar to that of the logistic regression problem, with a slighty higher number of defaults captured in the top 4 deciles. In fact, rather than setting a probabiltiy threshold and classifying loans as defaulted or not, we should be using the decile wise capture rate for our business purposes. For instance, the model tells us that in the top decile (0.9,1], we have 1185 defaults out of 1663 cases. This is over 70%, indicating that a case when the predicted probability falling in this decile has on the average 70% chance of defaulting. Hence those cases where the predicted probability comes in this decile, we should right away reject the application. For the subsequent deciles, we are still having loans of higher risk, however, instead of rejecting them right away, we should be charging higher interest on these loans since they pose higher risk.

# Summary

We created two differnt predictive mmodels for predicting loan default - a logistic regression model and an xgboost model. Feature selection was done using univariate gini analysis of individual variables. The raw variables were then transformed into their weight of evidences, which helped in improving the performance of the logistic regression classifier. The xgboost model performed slightly better than the logistic regression classifier, and that too without requiring any weight of evidence transformation. The logistic regression model gave an AUC of 0.74 (gini of 0.486) and the xgboost model gave an AUC of 0.76 (gini of 0.52)



