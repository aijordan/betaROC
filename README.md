# betaROC

## Minimum distance estimation (MDE) for receiver operating characteristic (ROC) curves

ROC curves are used ubiquitously to evaluate scores, features, covariates or
markers as potential predictors in binary problems. There exists an equivalence
between ROC curves and cumulative distribution functions (CDFs), which supports
a subtle shift of paradigms in the statistical modelling of ROC curves
(Gneiting and Vogel 2021). The flexible two-parameter beta family can be used
in the context of fitting CDFs to empirical ROC curves, and in a range
of empirical examples the beta family fits better than the classical binormal
model, particularly under the vital constraint of the fitted curve being concave.

## Contents

The betaROC repository contains:

- an early development version of the betaROC package for the statistical programming language R,
- and replication material (folder "/replication") for the corresponding paper by Gneiting and Vogel (2021).

## References

Gneiting T, Vogel P (2021). Receiver Operating Characteristic (ROC) Curves:
Equivalences, Beta Model, and Minimum Distance Estimation. Preprint
[arXiv:1809.04808](https://arxiv.org/abs/1809.04808).
