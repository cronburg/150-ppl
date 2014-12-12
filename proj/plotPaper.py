#!/usr/bin/env python
import pickle
#from pylab import *
import matplotlib.pyplot as plt
from pylab import transpose,where
import numpy as np

data = pickle.load(open("data/data.pkl",'r'))
param0,turn = transpose(data)
ymax2 = 250.0 / len(data)

def to_percentage(y,pos):
  ret = round(y * 100.0, 2)
  return str(ret) #+ '%'
#str(round( ( y / float(len(data)) ) * 100.0, 2)) + '%'

idx = [38, 35, 33, 31, 28] 
#clr = 'rgb'

fig,ax_array = plt.subplots(len(idx),1)
fig.set_figheight(15.0)
fig.set_figwidth(9.0)

c1 = tuple(np.array([255,165,0,125])/255.)
c2 = tuple(np.array([0,128,0,200])/255.)

for (t,i,ax1) in zip(idx,range(len(idx)),ax_array):
  
  #add_subplot(3, 1, i + 1) # draw the (i+1)th bar plot
  ax2 = ax1.twinx()
  ax2.yaxis.set_major_formatter(plt.FuncFormatter(to_percentage))
  #ax2.set_yticks([0,.02,.04,.06,.08])
  #ax2.set_ylim([0,.08])
  ax2.set_yticks(np.linspace(0.0, ymax2, 6)) #[0,.02,.04,.06,.08])
  ax2.set_ylim([0, ymax2])

  hist_data = param0[where(turn == t)[0]]
  weights = np.ones_like(hist_data) #/ len(hist_data)

#  ax2.hist(hist_data, bins=30, color=c1, weights=weights/len(hist_data))

  ax1.hist(hist_data, bins=30, color=c2)
  ax1.set_xlim(0.0, 1.0)
  ax1.set_ylim(0, 250)
  #ax1.set_yticks([
  #ax2.set_title("P($p_0$ | $t_e$ = %d)"%(t,))
  ax1.text(.5, .9, "P($p_0$ | $t_e$ = %d)"%(t,),
           horizontalalignment='center',
           transform=ax1.transAxes)
  if i == 2: ax1.set_ylabel("frequency")
  if i == 2: ax2.set_ylabel("probability $\\times$  100")

  if i + 1 < len(idx): ax1.get_xaxis().set_visible(False)

ax1.set_xlabel("param0 (Village-to-Chancellor buy ratio)")
plt.savefig("3-row-dist.png")

