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

## Process for Building Level 0 Models
* Specify model specific hyper-parameters in file **model_parameters.R**.  Hyper-paraemters
are determined by a separate procedure.  Note: for Python model ("xtc") the hyper-parameters
are specified in the module **train_model.py**.

* Run **create_level1_features.R** and **train_model.R**

The above modules will create model specific files of the form: **model_\<model type\>_datetime.RData and **\<model type\>_datetime.PyData**.  These model specific files will be pointed 
to by a file called "this_model".  Level 1 model building procedures will reference "this_model" 
to determine the model specific files required for generating features for training
Level 1 models.



