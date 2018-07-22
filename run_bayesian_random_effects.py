#!/usr/bin/env python
import pylab as plt
import seaborn as sns
import numpy as np
from scipy.stats.stats import pearsonr, ttest_1samp, percentileofscore, linregress
import os
import numpy as np
import pandas as pd
import pymc3 as pm

#bayesian_random_effects

n_samples=5000
#n_samples=500
n_burnin=500
#n_burnin=100
    
#preparing the data
data = pd.read_csv('data_hier.csv')
labels = data[['age','loss_avg_dec_thr']].copy()
studies = data["study_id"].unique()
data['study_code'] = data['study_id'].values
n_studies = len(data["study_id"].unique())
study_idx = data['study_code'].values

#setting up the model
with pm.Model() as hierarchical_model:
    # Hyperpriors for group nodes
    group_intercept_mean = pm.Normal('group intercept (mean)', mu=0., sd=100**2)
    group_intercept_variance = pm.Uniform('group intercept (variance)', lower=0, upper=100)
    group_slope_mean = pm.Normal('group slope (mean)', mu=0., sd=100**2)
    group_slope_variance = pm.Uniform('group slope (variance)', lower=0, upper=100)

    individual_intercepts = pm.Normal('individual intercepts', mu=group_intercept_mean, sd=group_intercept_variance, shape=n_studies)
    individual_slopes = pm.Normal('individual slopes', mu=group_slope_mean, sd=group_slope_variance, shape=n_studies)

    # Model error
    residuals = pm.Uniform('residuals', lower=0, upper=100)

    model_est =  individual_slopes[study_idx] * labels['age'].values + individual_intercepts[study_idx]

    # Data likelihood
    model_like = pm.Normal('model_like', mu=model_est, sd=residuals, observed=labels['loss_avg_dec_thr'])

    start = pm.find_MAP()
    step = pm.NUTS(scaling=start)
    hierarchical_trace = pm.sample(n_samples, step, start=start, tune=1000, progressbar=True)

mean_slope = hierarchical_trace['group slope (mean)'][n_burnin:].mean()
zero_percentile = percentileofscore(hierarchical_trace['group slope (mean)'][n_burnin:], 0)
print "Mean group level slope was %g (zero was %g percentile of the posterior distribution)"%(mean_slope, zero_percentile)

#show the distribution of the group slope (mean)
#pm.summary(hierarchical_trace[n_burnin:], hierarchical_trace['group slope (mean)'])
#optionally, show distribution of the group & individual intercepts, mean, variance, and residuals
pm.summary(hierarchical_trace[n_burnin:])


#traceplot
pm.traceplot(hierarchical_trace[n_burnin:])

#plot regression slopes across studies
# selection = studies
# fig, axis = plt.subplots(2, 3, figsize=(12, 6), sharey=True, sharex=True)
# axis = axis.ravel()
# xvals = np.linspace(labels['age'].min(), labels['loss_avg_dec_thr'].max())
# for i, c in enumerate(selection):
#     c_data = data.ix[data['study_id'] == c]
#     c_data = c_data.reset_index(drop = True)
#     z = list(c_data['study_code'])[0]
#     for a_val, b_val in zip(hierarchical_trace['individual intercepts'][n_burnin::10][z], hierarchical_trace['individual slopes'][n_burnin::10][z]):
#         axis[i].plot(xvals, a_val + b_val * xvals, 'g', alpha=.1)
#     axis[i].plot(xvals, hierarchical_trace['individual intercepts'][n_burnin:][z].mean() + hierarchical_trace['individual slopes'][n_burnin:][z].mean() * xvals,
#                     'g', alpha=1, lw=2.)
#     axis[i].hexbin(labels['age'], labels['loss_avg_dec_thr'], mincnt=1, cmap=plt.cm.YlOrRd_r)
#     axis[i].set_title(c)
#     axis[i].set_xlabel(labels['age'])
#     axis[i].set_ylabel(labels['loss_avg_dec_thr'])

plt.show()

print mean_slope, zero_percentile