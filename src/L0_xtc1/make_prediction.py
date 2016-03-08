# -*- coding: utf-8 -*-
"""
Created on Wed Mar  2 22:16:47 2016

@author: jim
"""

# py_train.tsv:  training data set

import sys
import pickle
import pandas as pd
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import ExtraTreesClassifier
from sklearn import ensemble   
   
    
if __name__ == "__main__":
    print "Start making predictions"
    
    # retrieve work directory
#    work_dir = sys.argv[1]
#    model_file = sys.argv[2]
#    predictors = sys.argv[3]
    work_dir = "../../src/L0_xtc1"
    model_file = 'possible_model'
    predictors = 'py_test.tsv'
    
    
    # retrieve model data
    model_file = work_dir + "/" + model_file
    with open(model_file,"rb") as f:
        model_dict = pickle.load(f)
        
    mdl_fit = model_dict['model']
    
    # retrieve data set to make prediction
    predictor_file = work_dir + "/" + predictors
    X_predictors = pd.read_csv(predictor_file,sep="\t")
    
    # fit model
    prediction = mdl_fit.predict_proba(X_predictors) 
    
    
    
    # save fitted model structure
    model_dict = {'model':mdl_fit}    
    
    model_file = work_dir + "/possible_model"
    with open(model_file,"wb") as f:
        pickle.dump(model_dict,f)
        
