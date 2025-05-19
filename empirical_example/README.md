# Empirical Example: Evaluating Causal Dominance Hypotheses in Multilevel VAR(1) Models

This empirical example demonstrates how to fit and evaluate hypotheses in a multilevel bivariate VAR(1) model using Bayesian methods implemented in R with JAGS. The example includes:

* Data preparation and transformation for multilevel analysis.
* Bayesian estimation using the `rjags` package.
* Diagnostics and evaluation of posterior samples.
* Standardization of parameter estimates.
* Hypothesis testing using the Generalized Order-Restricted Information Criterion Approximation (GORICA).

## Folder Contents

* **Data**:

  * Example data used (`condition_9998-1.dat`).

* **Scripts**:

  * R script (`empirical_example.R`) containing the complete workflow:

    * Data preparation
    * Bayesian model fitting
    * Diagnostics and trace plots
    * Parameter extraction and standardization
    * GORICA hypothesis evaluation

* **Models**:

  * JAGS model specification file (`bivar_var1_covariate.jags`).

## Steps in Analysis

1. **Data Preparation**: Reading and preparing data for Bayesian VAR modeling.
2. **Bayesian Estimation**: Running MCMC chains to obtain posterior distributions.
3. **Diagnostics**: Assessing convergence and posterior distributions.
4. **Parameter Extraction and Standardization**: Computing standardized cross-lagged effects at person-level and model-level.
5. **Hypothesis Testing with GORICA**: Evaluating informative hypotheses at both the between-subject (model) and within-subject levels.


## Running the Example

Execute the script `empirical_example.R` in R, ensuring your working directory is set to the `empirical_example` folder.

## Outputs

The script produces:

* Trace plots for diagnostics (`traceplots.pdf`, `all_traceplots.pdf`).
* Standardized parameter estimates and covariance matrices.
* GORICA results tables evaluating hypotheses.
* A summary data frame of within- and between-subject results.

These outputs facilitate a comprehensive assessment of causal dominance hypotheses using Bayesian multilevel VAR(1) modeling.

