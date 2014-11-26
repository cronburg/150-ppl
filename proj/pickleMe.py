#!/usr/bin/env python
from pylab import *

files = ["out%02d.dat"%(i,) for i in range(5)]
for f in files: execfile(f)
data = x00 + x01 + x02 + x03 + x04

import pickle
pickle.dump(data, open("data.pkl",'w'))

