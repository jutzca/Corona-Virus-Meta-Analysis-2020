import numpy as np
import pandas as pd


df = pd.read_csv('../data/Corona_review_labor.csv')

for col1 in df.columns:
    if col1.endswith('_Mean'):
        col2 = col1.replace('_Mean', '') + '_SD'

        means = df[col1]
        stds = df[col2]

        df[col1] = [f'{mean} (+- {std})' for mean, std in zip(means, stds)]

df.to_csv('../data/Corona_review_labor_edited.csv', index=False)
