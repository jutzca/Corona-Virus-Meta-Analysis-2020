#!/usr/bin/env python3
#
# Simulates skew normal distributions for measurements that are given as
# mean and standard deviation, respectively, and converts them to median
# and IQR.

import pandas as pd


def standard_approximation(mean, std):
    """Calculate standard approximation of mean and standard deviation.

    Calculates the standard approximation of mean and standard deviation
    by assuming that we are dealing with a normal distribution.

    Parameters
    ----------
    mean : float
        Mean of the observed distribution

    std : float
        Standard deviation of the observed distribution

    Returns
    -------
    Median and IQR under the assumption that the values have been
    observed from a normal distribution.
    """
    median = mean
    iqr = std * 1.35

    return median, iqr


if __name__ == '__main__':
    filename = '../data/data.xlsx'

    df = pd.read_excel(filename)

    missing_entries = df[df['Age_Q1'].isnull()]
    for index, row in missing_entries.iterrows():

        # This is the entry we are interested in. We try to parse it
        # afterwards and then continue with the simulation.
        age_reported = row['Age_reported']
