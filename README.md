# Analysis scripts from "Predicting 3-year persistent or recurrent major depressive episode using machine learning techniques" <a href='https://github.com/brunomontezano/mdd-prediction/'><img src='figures/depression.jpg' align="right" height="139" /></a>

[![DOI](https://img.shields.io/badge/DOI-10.1016%2Fj.psycom.2022.100055-blue)](https://doi.org/10.1016/j.psycom.2022.100055)

This repository maintains the data analysis from the paper entitled "Predicting
3-year persistent or recurrent major depressive episode using machine learning
techniques".

The study aimed to predict major depressive disorder (MDD) recurrence and
severe depression in MDD patients after three years of follow-up.

The paper is published at [*Psychiatry Research
Communications*](https://www.journals.elsevier.com/psychiatry-research-communications).

You can click the badge at the top of README to read the article, or click
[here](https://doi.org/10.1016/j.psycom.2022.100055).

## What does the repo contain?

It contains:

- Scripts for each prediction model:
    - Recurrent or persistent depression;
    - Severity level of depression among all subjects;
    - Severe depression among depressed subjects.
- Scripts that generated paper figures;
- Script that generated paper descriptive table.

The scripts use brazilian portuguese on many sections and variable names. Some
files have comments in english to ease the reading for english speakers.

## Python libraries used:

The following Python libraries were used:
`matplotlib`,
`seaborn`,
`pandas`.

## R packages used:

The following R packages were used:
`dplyr`,
`caret`,
`pROC`,
`purrr`,
`glmnet`,
`table1`,
and `ggplot2`.

## Contact

You can contact me at
bmontezano@hcpa.edu.br
