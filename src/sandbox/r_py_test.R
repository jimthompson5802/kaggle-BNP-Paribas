###
#  test for R/Python integration
###

library(rPython)

Sys.setenv(PYTHONPATH="/Users/jim/anaconda/lib/python2.7/site-packages")

python.load("./src/sandbox/r_py_test.py")

python.call("test_func")

python.call("add_func",6,7)

python.call("return_z")
python.get("z")

python.assign("z",123)
python.get("z")

