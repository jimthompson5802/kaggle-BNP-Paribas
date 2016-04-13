# -*- coding: utf-8 -*-
"""
Created on Wed Mar  2 22:16:47 2016

@author: jim
"""

# py_train.tsv:  training data set

import sys
import pickle
import zlib
import pandas as pd
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import ExtraTreesClassifier
from sklearn import ensemble

# model data stricture
# parameters gleaned from R script submission: ExtraTreesClassifier (score 0.45911)
mdl_fit = ExtraTreesClassifier(n_estimators=700,max_features= 50, 
                               criterion = 'entropy',min_samples_split= 5,
                                max_depth= 50, min_samples_leaf= 5,n_jobs=-1)      
   
    
if __name__ == "__main__":
    print "Starting training"
    
    # retrieve work directory
    work_dir = sys.argv[1]
#    work_dir = "../../src/L0_xtc1"
    
    # generate training data set file name
    training_file = work_dir + "/py_train.tsv"
    
    print training_file
    
    
    
    # read training data
    train = pd.read_csv(training_file,sep="\t")
    
    # isoloate response variable
    response = [1 if x == 'Class_1' else 0 for x in train["response"]]
    
    # isolate predictors
    predictors = train.columns[1:len(train.columns)].values
    X_train = train[predictors]
    
    # fit model
    mdl_fit.fit(X_train,response) 
    
    # save fitted model structure
    model_dict = {'model':mdl_fit}    
    
    model_file = work_dir + "/possible_model"
    with open(model_file,"wb") as f:
        pickle.dump(model_dict,f)
        
    print "Saved " + model_file
        
