# Blindness-and-Vision-Loss-2021
=========================================================================================================================
Global, Regional, and National Trends in Blindness and Vision Loss, 1990–2021: a Secondary Ecological Trend Analysis Based on Modelled Population Estimates

This repository contains the analysis scripts and reproduction package for the study of global vision loss burden. Our reporting adheres to the GRABDROP (Guidelines for Reporting Analyses of Big Data Repositories Open to Public) and GATHER guidelines.

📂 Repository Structure

Joinpoint_Analysis/: Scripts for temporal trend characterization using the Joinpoint Regression Program.

BAPC_Forecasting/: R scripts for Bayesian Age-Period-Cohort forecasting through 2050.

🛠 Methodology & Software Requirements

1. Joinpoint Regression
   Software: Joinpoint Regression Program (Version 4.9.0.0).
   Analysis: Used to calculate Annual Percent Change (APC) and Average Annual Percent Change (AAPC).
   Settings: Log-linear model, maximum 5 joinpoints, Monte Carlo permutation test (P < 0.05).

2. BAPC Forecasting
   Environment: R (Version 4.3.0) & Python (Version 3.9+).
   Core Packages:
     BAPC (v0.0.36): For Bayesian inference.
     INLA: For Integrated Nested Laplace Approximations.
   Model Config: Poisson likelihood, Log link function, and Second-order Random Walk (RW2) priors to ensure smoothing.

=========================================================================================================================
