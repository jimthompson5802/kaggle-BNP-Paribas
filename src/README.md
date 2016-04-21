Overview
=========

This directory contains all the source code to build models and submit predictions
to Kagggle.

A three-tiered model stacking approach is used for the solution.  The first tier, 
called Level 0 models that various forms of the raw features from the training
data set and generates Class probability predictions.  The Level 0 probability predictions 
are features into the Level 1 models which generate Class probablity predictions 
Level 2.  Finally, the Level 2 predictions are combined through a weighted 
geometric mean calculation to derive the final prediction that is sent to Kaggle 
for scoring.

## Directory structure
The names of the sub-directories within the **src** directory designates the type of
model.  Naming convention for sub-directories is of the form: \<level\>_\<model type\>\<numeric id\>.
* \<level\>: L0 - Level 0 model, L1 - Level 1 model, 
* \<model type\>: "gbm" - R gradient boosted tree model; "xgb" - R eXtreme Boosted Tree model;
"xtc" - sci-kit learn Extra Tree Classifier model, "nnet" - 1 hidden layer neural network

