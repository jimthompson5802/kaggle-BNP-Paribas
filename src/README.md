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

## CommonFunctions.R
This is a shared module. Sets some global variables and houses all the routines
to pre-process raw data (train or test) to create various feature sets for training 
and making predictions.

## Process for Building Level 0 Models
* Each module in this directory is dependent on a variable called **WORK.DIR**.  Ensure
this variable is set to the directory that the module is contained in.  For example, if
module **train_model.R** is in the **L0_gbm21** directory, then **WORK.DIR <- "./src/L0_gbm21"**.

* Specify model specific hyper-parameters in file **model_parameters.R**.  Hyper-paraemters
are determined by a separate procedure.  Note: for Python model ("xtc") the hyper-parameters
are specified in the module **train_model.py**.

* Run **create_level1_features.R** and **train_model.R**  This will create model 
specific files of the form: **model_\<model type\>_\<datetime\>.RData** and 
**\<model type\>_\<datetime\>.PyData**.  These model specific files will be pointed 
to by a file called **this_model**.  Level 1 model building procedures will reference **this_model** 
to determine the Level 0 model specific files required for generating features for training
Level 1 models.

## Process for Building Level 1 Models
* Each module in this directory is dependent on a variable called **WORK.DIR**.  Ensure
this variable is set to the directory that the module is contained in.  For example, if
module **train_model.R** is in the **L1_nnet11** directory, then **WORK.DIR <- "./src/L1_nnet11"**.

* Run **train_model.R**.  This will create model 
specific files of the form: **model_\<model type\>_\<datetime\>.RData**.  These model 
specific files will be pointed to by a file called **this_model**.

* Run **create_level2_features.R**.  This will create data used for use in the Level 2
blending of Level 1 predictions to derive the overall prediction.

