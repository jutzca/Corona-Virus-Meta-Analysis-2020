#!/usr/bin/env python3
#
# Simulates skew normal distributions for measurements that are given as
# mean and standard deviation, respectively, and converts them to median
# and IQR.

import argparse
import os

import numpy as np
import pandas as pd

import scipy.stats as stats


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


def extract_mean_and_std(entry):
    """Extract mean and standard deviation from an entry.

    Parses an entry in string format and tries to extract the mean and
    standard deviation. The following formats are supported:

    - `mean (+- std)`
    - `mean (+/- std)`
    - `mean (std)`

    Parameters
    ----------
    entry : str
        Entry from which to parse mean and standard deviation. If the
        mean fails, a tuple of NaN values will be returned.

    Returns
    -------
    Tuple of (`mean`, `std`). Can be NaN in case the parsing failed.
    """
    try:
        # First split according to whitespace, such that next, we are
        # dealing with two tuples.
        mean, std = entry.split(maxsplit=1)
    except ValueError:
        return np.nan, np.nan

    try:
        # This should always work for valid entries; if not, there is
        # nothing we can do here.
        mean = float(mean)
    except ValueError:
        mean = np.nan

    # This ensures that all valid formats are covered. If we are able to
    # convert `std` into a float after removing the nuisance characters,
    # we have a valid number.
    std = std.replace('(', '')
    std = std.replace(')', '')
    std = std.replace('+-', '')
    std = std.replace('+/-', '')

    try:
        std = float(std)
    except ValueError:
        std = np.nan

    return mean, std


def _delta(alpha):
    """Calculate `delta` auxiliary parameter from skewness parameter."""
    return alpha / np.sqrt(1 + alpha**2)


def calculate_scale(alpha, std):
    """Calculate scale from standard deviation and skewness."""
    delta = _delta(alpha)
    scale = np.sqrt(std**2 / (1 - 2 * delta**2 / np.pi))

    return scale


def calculate_location(alpha, mean, std):
    """Calculate location from standard deviation, mean, and skewness."""
    delta = _delta(alpha)
    scale = calculate_scale(alpha, std)
    location = mean - scale * delta * np.sqrt(2 / np.pi)

    return location


def skew_normal_approximation(mean, std, alpha0, alpha1):
    """Approximate mean and standard deviation based on skew distribution."""
    # Require this in order to check it later against our new skewed
    # approximation.
    median_, iqr_ = standard_approximation(mean, std)

    medians = []
    iqrs = []

    alpha_grid = np.linspace(
        alpha0,
        alpha1,
        dtype=float,
        endpoint=True,
    )

    for alpha in alpha_grid:
        loc = calculate_location(alpha, mean, std)
        scale = calculate_scale(alpha, std)

        # Sanity check: make sure that our fit is correct and we are
        # able to approximate mean and standard deviation correctly.
        mean_approx = stats.skewnorm.mean(alpha, loc, scale)
        std_approx = stats.skewnorm.std(alpha, loc, scale)

        assert np.allclose(mean_approx, mean)
        assert np.allclose(std_approx, std)

        median = stats.skewnorm.median(alpha, loc, scale)
        q1 = stats.skewnorm.ppf(0.25, alpha, loc, scale)
        q3 = stats.skewnorm.ppf(0.75, alpha, loc, scale)
        iqr = q3 - q1

        medians.append(median)
        iqrs.append(iqr)

    # This is the proper assumption here, since we are interested in an
    # *expected* value.
    median = np.mean(medians)
    iqr = np.mean(iqr)

    print(f'{median:.2f} [{iqr:.2f}] vs. {median_:.2f} [{iqr_:.2f}]')
    return median, iqr


if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-i', '--input',
        default='../data/data.xlsx',
        help='Input filename'
    )

    parser.add_argument(
        '-c', '--column',
        type=str,
        default='Age_reported',
        help='Column to use for simulation'
    )

    parser.add_argument(
        '-m', '--missing',
        type=str,
        default='Age_Q1',
        help='Column to use for missingness indicator',
    )

    parser.add_argument(
        '-a', '--alpha0',
        default=0.0,
        type=float,
        help='Lower range of skew parameter'
    )

    parser.add_argument(
        '-A', '--alpha1',
        default=1.0,
        type=float,
        help='Lower range of skew parameter'
    )

    args = parser.parse_args()

    filename = args.input

    if os.path.splitext(filename)[1] == '.xlsx':
        df = pd.read_excel(filename)
    else:
        df = pd.read_csv(filename)

    print(f'Simulation of skewed normal distributions\n'
          f'- Alpha range: [{args.alpha0:.2f}, {args.alpha1:.2f}]\n'
          f'- Column: {args.column}\n')

    medians = []
    iqrs = []

    missing_entries = df[df[args.missing].isnull()]
    for index, row in missing_entries.iterrows():

        # This is the entry we are interested in. We try to parse it
        # afterwards and then continue with the simulation.
        value = row[args.column]
        mean, std = extract_mean_and_std(value)

        if not np.isfinite(mean) or not np.isfinite(std):
            continue

        median, iqr = skew_normal_approximation(
            mean,
            std,
            args.alpha0,
            args.alpha1
        )

        medians.append(median)
        iqrs.append(iqr)

    median = np.median(medians)
    iqr = np.median(iqrs)

    print(f'\n'
          f'Overall summary:\n'
          f'{median:.2f} s[{iqr:.2f}]')
