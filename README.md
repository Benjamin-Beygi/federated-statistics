# Federated Statistics Demo (R)

## Privacy-Preserving Multi-Hospital Statistical Infrastructure

------------------------------------------------------------------------

## Executive Summary

This repository implements a **mathematically exact federated
statistical framework in R**.\
It simulates a multi-hospital research network where:

-   **No patient-level data leaves the hospital**
-   Only aggregated sufficient statistics are transmitted
-   Centralized optimization reconstructs exact pooled estimators
-   Multiple inference regimes are supported
-   Results match pooled `glm()` / `lm()` to numerical precision

This is not a toy mockup --- it is a research-grade foundation for
privacy-aware multicenter modeling.

------------------------------------------------------------------------

# Core Capabilities

## 1. Exact Federated Estimation

Supported models:

-   Logistic regression (exact MLE via damped Newton / IRLS)
-   Linear regression (exact OLS via sufficient statistics)
-   Welch t-test
-   Chi-square (2x2 exact reconstruction)
-   Mean / variance / SD

All federated results are numerically identical to centralized pooled
analysis.

------------------------------------------------------------------------

## 2. Multi-Layer Inference

The framework supports:

### Model-Based (Fisher Information)

Standard GLM variance reconstruction.

### Cluster-Robust (Hospital-Level Sandwich)

Robust variance treating hospitals as clusters.

### Leave-One-Hospital-Out Jackknife

Small-G robust inference for limited number of hospitals.

------------------------------------------------------------------------

## 3. Architecture

-   Independent R process per hospital
-   HTTP communication via `plumber`
-   Background orchestration using `callr`
-   Gradient + Hessian transport (no row-level transfer)
-   Deterministic convergence with monotone log-likelihood stabilization

------------------------------------------------------------------------

# Project Structure

    federated_statistics/
    │
    ├── client/
    │   ├── fed_engine.R
    │   ├── http_server_adapter.R
    │   ├── client_fed_lr_http.R
    │
    ├── server/
    │   ├── api_server.R
    │   ├── server_fed_lr_functions.R
    │   ├── server_factory.R
    │   └── data/
    │       ├── site_A.csv
    │       └── site_B.csv
    │
    ├── demo/
    │   ├── run_full_demo.R
    │   └── run_swespine_victor_demo.R
    │
    ├── README.md
    └── federated_statistics.Rproj

------------------------------------------------------------------------

# Statistical Framework

For logistic regression:

    logit(P(Y=1)) = Xβ

Each hospital computes locally:

-   Gradient: Xᵀ(y − p)
-   Hessian: −XᵀWX
-   Log-likelihood
-   Sample size

The central node aggregates:

    Σ gradients
    Σ Hessians
    Σ log-likelihood

Newton updates reconstruct the exact pooled MLE.

No patient-level data is transmitted.

------------------------------------------------------------------------

# Mathematical Validation

The federated estimators satisfy:

    β_federated ≡ β_pooled  (within floating-point tolerance)

Demonstrated equivalence includes:

-   Logistic regression coefficients
-   Linear regression coefficients
-   Welch t-test statistics
-   Chi-square statistics
-   Means and standard deviations

Equivalence is typically exact to 1e-12 precision.

------------------------------------------------------------------------

# Privacy Model

What leaves each hospital:

-   Gradient vector
-   Hessian matrix
-   Log-likelihood scalar
-   Sufficient statistics (XtX, Xty, counts)

What never leaves:

-   Individual rows
-   Raw predictors
-   Outcome vectors
-   Identifiable patient data

This aligns with:

-   GDPR constraints
-   Multicenter governance frameworks
-   DataSHIELD-style philosophy
-   Federated learning principles

------------------------------------------------------------------------

# Swespine Demonstration

The repository includes a large-scale multicenter registry simulation:

    source("demo/run_swespine_victor_demo.R")

Demonstrates:

-   26 simulated hospitals
-   Exact central vs federated equivalence
-   Logistic regression
-   Linear regression
-   Welch t-test
-   Chi-square (Yates-corrected)

All results match pooled analysis exactly.

------------------------------------------------------------------------

# Running the Demo

From project root:

    source("demo/run_full_demo.R")

To stop background hospital processes:

    hospA$kill()
    hospB$kill()

------------------------------------------------------------------------

# Installation

Requirements:

-   R \>= 4.0
-   plumber
-   httr
-   callr

Install dependencies:

    install.packages(c("plumber", "httr", "callr"))

------------------------------------------------------------------------

# Research Applications

This framework supports:

-   Multicenter outcome modeling
-   Cross-hospital benchmarking
-   Registry-based research networks
-   Privacy-aware European collaborations
-   Federated clinical infrastructure development

------------------------------------------------------------------------

# Forward Roadmap

Short-Term: - K-hospital simulation studies - Coverage evaluation of
cluster-robust vs jackknife - Automated equivalence validation

Medium-Term: - Secure TLS deployment - Authentication + audit logging -
DataSHIELD / Opal alignment

Advanced: - Federated mixed-effects models - Random intercept models -
Survival analysis (Cox PH) - Penalized regression - Secure aggregation
protocols

------------------------------------------------------------------------

# Strategic Positioning

This project sits at the intersection of:

-   Clinical registry science
-   Statistical methodology
-   Federated optimization
-   Privacy-preserving infrastructure

It demonstrates that exact multicenter modeling is technically feasible
without centralizing patient data.

------------------------------------------------------------------------

# License

MIT License

------------------------------------------------------------------------

# Author

Benjamin\
Federated Clinical Statistics Infrastructure Project

------------------------------------------------------------------------

# Closing Statement

Federated clinical modeling can be:

-   Mathematically exact\
-   Statistically robust\
-   Privacy-aligned\
-   Infrastructure-scalable

This repository is a foundation for building real-world, GDPR-compliant,
multicenter research networks.
