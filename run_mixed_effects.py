#!/usr/bin/env python
import pylab as plt
import seaborn as sns
import numpy as np
from scipy.stats.stats import pearsonr, ttest_1samp, percentileofscore, linregress
import os
import numpy as np
import pandas as pd


# read the data frame
df = pd.read_csv('data_hier.csv')

# approximate_random_effects
#slope[0], intercept[1], r_value[2], p_value[3], std_err[4]
correlation_per_study = {}
#correlation_per_study = df.groupby('study_id').apply(lambda v: linregress(v.age, v.gain_avg_dec_thr)[0])
correlation_per_study = df.groupby('study_id').apply(lambda v: linregress(v.age, v.loss_avg_dec_thr)[0])

correlation_df = correlation_per_study.reset_index()
correlation_df.columns = ['study_id', 'slope']

average_slope = np.mean(correlation_df.slope)
t, p_val = ttest_1samp(correlation_df.slope, 0)

print "Averaged slope across studies = %g (t=%g, p=%g)"%(average_slope, t, p_val)
sns.violinplot(correlation_df.slope, inner="points", names=["studies"])
plt.ylabel("Linear regression slopes between age and decision threshold")
plt.axhline(0, color="red")

#sns.lmplot('age', 'gain_avg_dec_thr', data=df, hue='study_id', col='study_id', col_wrap=3)
sns.lmplot('age', 'loss_avg_dec_thr', data=df, hue='study_id', col='study_id', col_wrap=3)
plt.show()

print average_slope, t, p_val
