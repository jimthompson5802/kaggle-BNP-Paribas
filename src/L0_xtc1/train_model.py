# -*- coding: utf-8 -*-
"""
Created on Wed Mar  2 22:16:47 2016

@author: jim
"""

import os
#import pandas as pd
import numpy as np
import csv
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import ExtraTreesClassifier
from sklearn import ensemble

# model data stricture
mdl_fit = ExtraTreesClassifier(n_estimators=700,max_features= 50, 
                               criterion = 'entropy',min_samples_split= 5,
                                max_depth= 50, min_samples_leaf= 5)      
   
def print_wd():
    print os.getcwd()


def train_model(train_data):
    # read in training data
    train = pd.DataFrame.from_csv(train_data,sep="\t")
    X_train = train
    response = ""
    mdl_fit.fit(X_train,response) 


def save_model(file_name):
    print "save model"
    
    
if __name__ == "__main__":
    print "Here is",print_wd()
        
