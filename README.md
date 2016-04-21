BNP Paribas Cardif Claims Management
==================================================

## Overview
Kaggle competition https://www.kaggle.com/c/bnp-paribas-cardif-claims-management

The repo [wiki](https://github.com/jimthompson5802/kaggle-BNP-Paribas/wiki) contains
a high-level description of the overall model.

## Results
* First successful use of model stacking.
* Placed 134 (JMT5802) out of 2,947 teams (Pending final verification by Kaggle).  Top 
5% finisher.

## System Requirements
* In addition to R, the Anaconda Python distribution, which includes pandas and 
sci-kit learn, is required.  For this work Python 2.7 was used.

## Directory Overview
* **src** - contains R and Python source code to build and test models and to 
create Kaggle submissions
* **eda** - contains code for exploratory data analysis and extract data required
for creating features.
* **munge** - code to extract raw data for various purposes

To run the code in this repo requires creating the **data** sub-directory and 
downloading the Kaggle training and test data sets.




