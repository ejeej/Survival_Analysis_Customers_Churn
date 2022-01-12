import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.inspection import permutation_importance
from sklearn.base import TransformerMixin
from sklearn.model_selection import train_test_split, GridSearchCV, KFold
from sksurv.preprocessing import OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sksurv.nonparametric import kaplan_meier_estimator
from sksurv.linear_model import CoxPHSurvivalAnalysis
from sksurv.ensemble import RandomSurvivalForest
from sksurv.metrics import concordance_index_censored, concordance_index_ipcw, brier_score, integrated_brier_score, as_concordance_index_ipcw_scorer

# Function to get predictions from estimator

def get_predictions(Xtrain, Xtest, ytrain, ytest, estimator):
  pred_train = estimator.predict(Xtrain)
  pred_test = estimator.predict(Xtest)
  pred_surv_train = estimator.predict_survival_function(Xtrain)
  pred_surv_test = estimator.predict_survival_function(Xtest)
  
  return pred_train, pred_test, pred_surv_train, pred_surv_test

# Function to get metrics for model

def get_metrics_df(Xtrain, Xtest, ytrain, ytest, estimator):
  pred_train = estimator.predict(Xtrain)
  pred_test = estimator.predict(Xtest)
  pred_surv_train = estimator.predict_survival_function(Xtrain)
  pred_surv_test = estimator.predict_survival_function(Xtest)

  hc_train = concordance_index_censored(ytrain["Churn"], ytrain["Time"], pred_train)[0]
  hc_test = concordance_index_censored(ytest["Churn"], ytest["Time"], pred_test)[0]
  uc_train = concordance_index_ipcw(ytrain, ytrain, pred_train)[0]
  uc_test = concordance_index_ipcw(ytest, ytest, pred_test)[0]
  
  times = np.array([7, 14, 30, 60, 90, 180, 360, 720])
  preds_train = np.asarray([[fn(t) for t in times] for fn in pred_surv_train])
  preds_test = np.asarray([[fn(t) for t in times] for fn in pred_surv_test])

  bs_train = brier_score(ytrain, ytrain, preds_train, times)
  bs_test = brier_score(ytest, ytest, preds_test, times)
  
  times2 = np.arange(1, 720)
  #preds_train2 = np.asarray([[fn(t) for t in times2] for fn in pred_surv_train])
  preds_test2 = np.asarray([[fn(t) for t in times2] for fn in pred_surv_test])
  
  #ibs_train = integrated_brier_score(ytrain, ytrain, preds_train2, times2)
  ibs_test = integrated_brier_score(ytest, ytest, preds_test2, times2)
  
  results = pd.DataFrame(
    {"Metric": ['Harrel`s C', 'Uno`s C', 'IBS 1-720d', 'BS 7d', 'BS 14d', 'BS 30d', 'BS 60d',
      'BS 90d', 'BS 180d', 'BS 360d', 'BS 720d'],
      "Train": [hc_train, uc_train, 'NULL'] + [i for i in bs_train[1]],
      "Test": [hc_test, uc_test, ibs_test] + [i for i in bs_test[1]]})
  
  return results, bs_train, bs_test

# Function to plot time-dependent Brier score

def plot_td_brier(bs_tuple):
  plt.figure()
  plt.plot([i for i in bs_base_test[0]], [i for i in bs_base_test[1]], marker="o")
  plt.xlabel("Days from start of subscription")
  plt.ylabel("Time-dependent Brier score")
  plt.xticks([7,30,60,90,180,360,720], [7,30,60,90,180,360,720])
  plt.grid()
  plt.show()


# Getting data from R (in python chunks in R Notebook)

X_base = r.X_base
X_base.shape

X = r.X
X.shape

cat_columns = r.cat_columns
num_columns = r.num_columns
cat_base_columns = r.cat_base_columns
num_base_columns = r.num_base_columns

aux = [tuple([bool(i[0]), i[1]]) for i in r.Y_clmns.to_numpy()]
y = np.array(aux, dtype=[('Churn', '?'), ('Time', '<f8')])
y
y.shape

random_state = 42

# Train and test split

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=random_state)
X_base_train, X_base_test, _, _ = train_test_split(X_base, y, test_size=0.3, random_state=random_state)

sample_X_test_id = r.sample_X_test_id
sample_X_test = X_test.loc[sample_X_test_id['ID'].values]
sample_X_base_test = X_base_test.loc[sample_X_test_id['ID'].values]

sample_y_churn = [int(i[0]) for i in y[sample_X_test_id['ID'].values]]
sample_y_time = [int(i[1]) for i in y[sample_X_test_id['ID'].values]]

sample_sort = np.argsort(sample_y_time)
sample_sort = sample_sort[::-1]

sample_X_test = sample_X_test.iloc[sample_sort]
sample_X_base_test = sample_X_base_test.iloc[sample_sort]

sample_y_churn = [sample_y_churn[i] for i in sample_sort]
sample_y_time = [sample_y_time[i] for i in sample_sort]

feature_names = X_train.columns.tolist()
feature_names_base = X_base_train.columns.tolist()


# Baseline Cox without data on previous subscription

cox_ph_base = Pipeline(
  [
    ('encode', OneHotEncoder()),
    ("model", CoxPHSurvivalAnalysis())
  ]
)

encoder_names = OneHotEncoder()

cox_ph_base.fit(X_base_train, y_train)

cox_ph_base_coef = pd.Series(cox_ph_base.named_steps['model'].coef_, index=encoder_names.fit_transform(X_base_train).columns)
cox_ph_base_coef = cox_ph_base_coef.reset_index()
cox_ph_base_coef.columns = ['Variable', 'Coef']
cox_ph_base_coef.sort_values(by='Coef', key=abs, ascending=False, inplace=True)
cox_ph_base_coef['HR'] = cox_ph_base_coef['Coef'].apply(lambda x: np.exp(x))
cox_ph_base_coef

cox_base_pred_train, cox_base_pred_test, cox_base_surv_train, cox_base_surv_test = get_predictions(X_base_train, X_base_test, y_train, y_test, cox_ph_base)

cox_base_results, bs_base_train, bs_base_test = get_metrics_df(X_base_train, X_base_test, y_train, y_test, cox_ph_base)

cox_base_results

plot_td_brier(bs_base_test)

pred_surv = cox_ph_base.predict_survival_function(sample_X_base_test)

time_points = np.arange(1, 720)
plt.figure()
for i, surv_func in enumerate(pred_surv):
    plt.step(time_points, surv_func(time_points), where="post",
             label="Churn={churn}, Time={time}d".format(churn=sample_y_churn[i], time=sample_y_time[i]))
plt.ylabel("Est. probability of survival $\hat{S}(t)$")
plt.xlabel("Time $t$")
plt.title('Cox PH baseline')
plt.legend(bbox_to_anchor=(0.7, 0.7))
plt.show()


# Cox with data on previous subscription

cox_ph = Pipeline(
  [
    ('encode', OneHotEncoder()),
    ("model", CoxPHSurvivalAnalysis())
  ]
)

cox_ph.fit(X_train, y_train)

cox_ph_coef = pd.Series(cox_ph.named_steps['model'].coef_, index=encoder_names.fit_transform(X_train).columns)
cox_ph_coef = cox_ph_coef.reset_index()
cox_ph_coef.columns = ['Variable', 'Coef']
cox_ph_coef.sort_values(by='Coef', key=abs, ascending=False, inplace=True)
cox_ph_coef['HR'] = cox_ph_coef['Coef'].apply(lambda x: np.exp(x))
cox_ph_coef

cox_pred_train, cox_pred_test, cox_surv_train, cox_surv_test = get_predictions(X_train, X_test, y_train, y_test, cox_ph)

cox_results, bs_train, bs_test = get_metrics_df(X_train, X_test, y_train, y_test, cox_ph)

cox_results

plot_td_brier(bs_test)


pred_surv = cox_ph.predict_survival_function(sample_X_test)

time_points = np.arange(1, 720)
plt.figure()
for i, surv_func in enumerate(pred_surv):
    plt.step(time_points, surv_func(time_points), where="post",
             label="Churn={churn}, Time={time}d".format(churn=sample_y_churn[i], time=sample_y_time[i]))
plt.ylabel("Est. probability of survival $\hat{S}(t)$")
plt.xlabel("Time $t$")
plt.title('Cox PH prev')
plt.legend(bbox_to_anchor=(0.7, 0.7))
plt.show()


# RSF tuning

X_base_tt, X_base_val, y_tt, y_val = train_test_split(X_base_train, y_train, test_size=0.2, random_state=random_state)
X_tt, X_val, _, _ = train_test_split(X_train, y_train, test_size=0.2, random_state=random_state)

rsf_base_scores = []

for md in np.arange(5, 11, dtype=int):
  rsf = RandomSurvivalForest(n_estimators=500, max_depth=md, max_features="sqrt", min_samples_leaf=10, min_samples_split=10, n_jobs=-1, random_state=random_state)
  rsf_fit = rsf.fit(X_base_tt, y_tt)
  rsf_base_scores += [rsf.score(X_base_val, y_val)]
  print md, rsf_base_scores[-1]

for msl in np.array([5,10,15,20], dtype=int):
  rsf = RandomSurvivalForest(n_estimators=500, max_depth=14, max_features="sqrt", min_samples_leaf=msl, min_samples_split=10, n_jobs=-1, random_state=random_state)
  rsf_fit = rsf.fit(X_base_tt, y_tt)
  rsf_base_scores += [rsf.score(X_base_val, y_val)]
  print(msl, rsf_base_scores[-1])
  

# RSF with baseline data

rsf_base = Pipeline(
  [
    ('encode', OneHotEncoder()),
    ("model", RandomSurvivalForest(n_estimators=500, max_depth=14, max_features="sqrt", min_samples_leaf=5, min_samples_split=10, n_jobs=-1, random_state=random_state))
  ]
)

rsf_base.fit(X_base_train, y_train)

rsf_base_pred_train, rsf_base_pred_test, rsf_base_surv_train, rsf_base_surv_test = get_predictions(X_base_train, X_base_test, y_train, y_test, rsf_base, rf=True)

rsf_base_results, rsf_base_bs_train, rsf_base_bs_test = get_metrics_df(X_base_train, X_base_test, y_train, y_test, rsf_base)

rsf_base_results

pred_surv = rsf_base.predict_survival_function(sample_X_base_test)

time_points = np.arange(1, 720)
plt.figure()
for i, surv_func in enumerate(pred_surv):
    plt.step(time_points, surv_func(time_points), where="post",
             label="Churn={churn}, Time={time}d".format(churn=sample_y_churn[i], time=sample_y_time[i]))
plt.ylabel("Est. probability of survival $\hat{S}(t)$")
plt.xlabel("Time $t$")
plt.legend(bbox_to_anchor=(0.7, 0.7))
plt.title('RSF baseline')
plt.show()

rsf_imp_base = permutation_importance(rsf_base, X_base_test, y_test, n_repeats=10, random_state=random_state)
sorted_idx = rsf_imp_base.importances_mean.argsort()

fig, ax = plt.subplots()
ax.boxplot(
    rsf_imp_base.importances[sorted_idx].T, vert=False, labels=X_base_train.columns[sorted_idx]
)
ax.set_title("RSF baseline, variable importance (permutation)")
fig.tight_layout()
plt.show()


# RSF with data on previous subscriptions

rsf = Pipeline(
  [
    ('encode', OneHotEncoder()),
    ("model", RandomSurvivalForest(n_estimators=500, max_depth=14, max_features="sqrt", min_samples_leaf=5, min_samples_split=10, n_jobs=-1, random_state=random_state))
  ]
)

rsf.fit(X_train, y_train)

rsf_pred_train, rsf_pred_test, rsf_surv_train, rsf_surv_test = get_predictions(X_train, X_test, y_train, y_test, rsf, rf=True)

rsf_results, rsf_bs_train, rsf_bs_test = get_metrics_df(X_train, X_test, y_train, y_test, rsf)

rsf_results

pred_surv = rsf.predict_survival_function(sample_X_test)

time_points = np.arange(1, 720)
plt.figure()
for i, surv_func in enumerate(pred_surv):
    plt.step(time_points, surv_func(time_points), where="post",
             label="Churn={churn}, Time={time}d".format(churn=int(y_test[sample_mask.index[i]][0]), time=int(y_test[sample_mask.index[i]][1])))
plt.ylabel("Est. probability of survival $\hat{S}(t)$")
plt.xlabel("Time $t$")
plt.legend(bbox_to_anchor=(0.7, 0.7))
plt.title('RSF prev')
plt.show()

rsf_imp = permutation_importance(rsf, X_test, y_test, n_repeats=10, random_state=random_state)
sorted_idx = rsf_imp.importances_mean.argsort()

fig, ax = plt.subplots()
ax.boxplot(
    rsf_imp.importances[sorted_idx].T, vert=False, labels=X_train.columns[sorted_idx]
)
ax.set_title("RSF prev, variable importance (permutation)")
fig.tight_layout()
plt.show()
