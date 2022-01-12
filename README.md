# Predicting customers' churn with survival analysis

This study project was aimed at prediction of the probability of customers’ attrition (churn) with methods from survival analysis. In comparison with standard classification approaches resulted in prediction of churn as a binary target variable or probability of churn over some fixed period of time, survival analysis can be useful in understanding the dynamic of customer retention and attrition over time after some starting point (usually the start of relationship - contract, subscription, etc.). It can help in answering such questions as when clients churn the most, whether there are periods of increasing, stable or decreasing churn rates, as well as whether these all are different depending on customers’ characteristics, their behavior or some contract options.

The dataset for the project was taken from Kaggle competition [“WSDM - KKBox’s Churn Prediction Challenge”](https://www.kaggle.com/c/kkbox-churn-prediction-challenge/) which was held in 2017. KKBox is an Asian company which offers subscription based music streaming service. 
The criteria of churn used in the competition is no new valid service subscription within 30 days after the current membership expires. 
I used the same definition, although I was predicting not just the probability of churn during the next 30 days after membership expiration as a binary outcome, but the distribution of time to churn and retention rates for different time points after the start of subscription. 
I used models from both statistical analysis and machine learning, as well as both baseline features (available at the start of subscription) and time-dependent variables (information on user’s behavior during subscription).

2 Files for transactions and 2 files for user logs given in the competition were combined into seperate files and cleaned of duplicates. After that I found the list of unique users' IDs which occured in transactions, user logs and membership files simultaneously - and made a random sample of 1% of them (19200 users). Their IDs could be found in [msno_sample.csv](data/msno_sample.csv) file. In my project I used data for only these users. As the dataset was still large in volume, I could not upload it here. But all data can be obtained by the given IDs. I did it using SQL.

Exploratory data analysis and feature engineering were realized in R (see [Kaggle_kkbox_part1_EDA.R](scripts/Kaggle_kkbox_part1_EDA.R), as well as data preparation for both [time-independent](scripts/Kaggle_kkbox_part2_Time-independent.R) and [time-dependent](scripts/Kaggle_kkbox_part3_Time-dependent.R) models.

Train/test split and estimation of the models with time-independent variables were done using library `scikit-survival` in Python (through `reticulate` in R Notebook). For convenience, all Python chunks were saved in separate [Kaggle_kkbox_part2_Time-independent.py](scripts/Kaggle_kkbox_part2_Time-independent.py) file.

Models with time-dependent variables were estimated using libraries `survival` and `LTRCforest` in R (see [Kaggle_kkbox_part3_Time-dependent.R](scripts/Kaggle_kkbox_part3_Time-dependent.R)).

Variable importance in Cox regressions was calculated as a proportion of explainable log-likelihood using functions in `rms` package for R, in random survival forests - using a permutation method in `sklearn` package for Python.

The full text with literature survey and results, as well as with all code chunks for R and Python can be found in [Kaggle_kkbox_survival.html](Kaggle_kkbox_survival.html) file (in English). It was obtained from R Notebook [Kaggle_kkbox_survival.Rmd](Kaggle_kkbox_survival.Rmd) and published at [Rpubs](https://rpubs.com/omironenko/survival_churn). 

Presentation of the main project ideas and results can be found in [Churn_Prediction_Survival_inRussian.pptx](Churn_Prediction_Survival_inRussian.pptx) file (in Russian).
