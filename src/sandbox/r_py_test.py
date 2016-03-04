# -*- coding: utf-8 -*-
"""
Created on Thu Mar  3 06:25:18 2016

@author: jim
"""

z = 3333

def test_func():
    return "Hello World", "abc"
    
def add_func(a,b):
    c = a + b
    return(c)
    
def return_z():
    return(z)
    
if __name__ == "__main__":
    print test_func()
    print add_func(3,4)
    print return_z()
    
