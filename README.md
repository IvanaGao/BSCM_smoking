# Bayesian Synthetic Control Method (BSCM)

This repository contains implementations of the Synthetic Control Method (SCM) and its Bayesian variants using different priors.

## 1. Standard SCM

Run the following script to execute the standard SCM:

```r
run_scm.R
````

## 2. Bayesian SCM with Horseshoe Prior

Run the following script to execute Bayesian SCM using the horseshoe prior:

```r
run_bscm_horseshoe.R
```

## 3. Bayesian SCM with Ridge Prior

Run the following script to execute Bayesian SCM using the ridge prior:

```r
run_bscm_ridge.R
```

## 4. Bayesian SCM with Distance Horseshoe Prior

Execute the following scripts in order:

```r
prepare_data.R
calculate_ds.R
run_bscm_distance_horseshoe.R
```

## 5. Visualization

Use the following scripts for visualizing the results:

```r
plot_posterior_comparison.R      # Compare posterior distributions
plot_predict_value.R             # Visualize predicted vs. observed values
```


