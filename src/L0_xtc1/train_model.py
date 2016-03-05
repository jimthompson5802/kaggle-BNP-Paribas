# -*- coding: utf-8 -*-
"""
Created on Wed Mar  2 22:16:47 2016

@author: jim
"""

import pandas as pd
import numpy as np
import csv
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import ExtraTreesClassifier
from sklearn import ensemble

# model data stricture
mdl_fit = ExtraTreesClassifier(n_estimators=700,max_features= 50, criterion= 'entropy',min_samples_split= 5,
                                max_depth= 50, min_samples_leaf= 5)      
    


train_model():
    # read in training data
    
    extc.fit(X_train,target) 


save_model(file_name):
    
        
