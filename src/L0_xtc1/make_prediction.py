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
    work_dir = sys.argv[1]  #work directory
    model_file = sys.argv[2]  # name of file containing pickled model object
    predictors_file = sys.argv[3]  # name of file contaiing predictor attributes
    predictions_file = sys.argv[4]  # name of file to contain predicted responses

#    work_dir = '../../src/L0_xtc1'   #debuging
#    model_file = 'possible_model'    #debugging
#    predictors_file = 'py_test.tsv'  #debugging
#    predictions_file = 'py_test_predictions.tsv'  #debugging
    
    
    # retrieve pickled model object
    model_file = work_dir + "/" + model_file
    print "Retrieving " + model_file
    with open(model_file,"rb") as f:
        model_dict = pickle.load(f)
        
    mdl_fit = model_dict['model']
    
    # retrieve data set to make prediction
    predictors_file = work_dir + "/" + predictors_file
    X_predictors = pd.read_csv(predictors_file,sep="\t")
    
    # predict probabilities
    predictions = mdl_fit.predict_proba(X_predictors) 
    
    # convert to DataFrame
    headers = ['Class_' + str(i) for i in mdl_fit.classes_]
    predictions = pd.DataFrame(predictions,columns=headers)
    
    # save predictions to file
    predictions_file = work_dir + "/" + predictions_file
    predictions.to_csv(predictions_file,sep='\t',index=False)
    
    
        
