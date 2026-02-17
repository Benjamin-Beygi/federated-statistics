# Federated Statistics Demo (R)

### Privacy-Preserving Multi-Hospital Logistic Regression Infrastructure

------------------------------------------------------------------------

## Overview

This project implements a **federated logistic regression framework in
R**, simulating a multi-hospital research network where:

-   No patient-level data leaves the hospital
-   Only aggregated statistical quantities (gradient, Hessian,
    log-likelihood) are transmitted
-   Centralized optimization reconstructs full maximum likelihood
    estimates
-   Multiple inference regimes are supported:
    -   Model-based (Fisher Information)
    -   Cluster-robust (hospital-level sandwich)
    -   Leave-one-hospital-out jackknife (small-G robust)

The system runs entirely over HTTP using `plumber`, simulating real
hospital servers.

This repository demonstrates:

-   Mathematical validity (federated == pooled)
-   Technical feasibility (true process isolation)
-   GDPR-aligned privacy structure
-   Research-grade inference
-   Scalable infrastructure design

------------------------------------------------------------------------

## Key Features

### 1. True Federated Optimization

-   Damped Newton / IRLS
-   Monotone log-likelihood convergence
-   Line-search stabilization
-   No raw data transfer

### 2. Multi-Layer Inference

-   Model-based standard errors
-   Cluster-robust SE (hospital clustering)
-   Leave-one-hospital-out jackknife (robust for small number of
    hospitals)

### 3. Realistic Architecture

-   Separate R processes per hospital
-   HTTP communication via `plumber`
-   Background server orchestration using `callr`
-   One-command demo execution

### 4. One-Command Execution

Run:

``` r
source("demo/run_full_demo.R")
```

This: 1. Launches hospital A and B 2. Waits for servers to respond 3.
Executes federated logistic regression 4. Prints all inference layers 5.
Leaves servers running for inspection

------------------------------------------------------------------------

## Project Structure

    federated_statistics/
    │
    ├── client/
    │   ├── fed_engine.R                 # Core federated optimizer
    │   ├── http_server_adapter.R        # HTTP transport layer
    │   ├── client_fed_lr_http.R         # HTTP-based federated execution
    │
    ├── server/
    │   ├── api_server.R                 # Plumber API (hospital node)
    │   ├── server_fed_lr_functions.R    # Gradient/Hessian logic
    │   ├── server_factory.R             # Server creation utilities
    │   └── data/
    │       ├── site_A.csv
    │       └── site_B.csv
    │
    ├── demo/
    │   └── run_full_demo.R              # One-command demo runner
    │
    ├── README.md
    └── federated_statistics.Rproj

------------------------------------------------------------------------

## Statistical Model

We fit:

    logit(P(Y=1)) = β0 + β1*age + β2*sex + β3*x1 + β4*x2B + β5*x2C

Federated updates use:

-   Gradient: Xᵀ(y − p)
-   Hessian: −XᵀWX

Aggregated across hospitals.

------------------------------------------------------------------------

## Inference Methods

### 1. Model-Based (Fisher)

Assumes independent observations.

### 2. Cluster-Robust

Hospitals treated as clusters.

Correct when: - Large number of hospitals (G → ∞)

### 3. Jackknife (Small-G Robust)

Leave-one-hospital-out refitting.

Recommended when: - G \< 10 hospitals

------------------------------------------------------------------------

## Mathematical Validation

The federated solution matches pooled `glm()` results to numerical
precision.

This verifies correctness of:

-   Optimization logic
-   Transport layer
-   Aggregation
-   Variance reconstruction

------------------------------------------------------------------------

## Privacy Model

What leaves the hospital: - Gradient vector - Hessian matrix -
Log-likelihood scalar - Sample size

What never leaves: - Patient rows - Raw predictors - Outcome data

This structure aligns with: - GDPR constraints - Multicenter governance
requirements - DataSHIELD philosophy

------------------------------------------------------------------------

## Horizon Goals

### Short-Term

-   Simulate K-hospital scenarios (G = 5, 10, 20)
-   Compare inference methods (coverage, bias)
-   Produce publication-quality simulation study

### Medium-Term

-   Align architecture with DataSHIELD / Opal
-   Replace HTTP layer with secure VPN/TLS deployment
-   Add authentication and audit logging

### Advanced Research Directions

-   Federated mixed-effects models
-   Random intercept models for hospital heterogeneity
-   Survival analysis (Cox PH)
-   Distributed penalized regression
-   Secure aggregation protocols

------------------------------------------------------------------------

## Strategic Vision

This project is positioned at the intersection of:

-   Clinical multicenter collaboration
-   Statistical methodology
-   Federated learning infrastructure
-   Privacy-preserving data science

Potential use cases:

-   European neurosurgical registries
-   Multicenter outcome modeling
-   Cross-hospital benchmarking
-   Research networks under strict governance

------------------------------------------------------------------------

## Requirements

-   R \>= 4.0
-   plumber
-   httr
-   callr
-   stats

Install dependencies:

``` r
install.packages(c("plumber", "httr", "callr"))
```

------------------------------------------------------------------------

## Running the Demo

From project root:

``` r
source("demo/run_full_demo.R")
```

To stop servers afterwards:

``` r
hospA$kill()
hospB$kill()
```

------------------------------------------------------------------------

## License

MIT License

------------------------------------------------------------------------

## Author

Benjamin --- Federated Clinical Statistics Infrastructure Project

------------------------------------------------------------------------

## Closing Statement

This repository demonstrates that:

-   Federated clinical modeling is mathematically exact.
-   Distributed inference is achievable with correct variance
    reconstruction.
-   Small-cluster robustness can be addressed via jackknife methods.
-   Multicenter collaboration can be technically feasible without data
    pooling.

This is not a toy example --- it is a foundation for scalable,
privacy-aware clinical research networks.
