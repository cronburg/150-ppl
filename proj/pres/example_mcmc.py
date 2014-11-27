#!/usr/bin/env python
from pylab import *
from scipy.stats import norm

x = np.linspace(-3,3,100)

rxs = 6*random_sample(1000)-3
rys = 0.4*random_sample(1000)
scatter(rxs,rys,c='g')
plot(x, norm.pdf(x,0,2), linewidth=3, c='red')
plot(x, norm.pdf(x,0,1), linewidth=3, c='blue')
xlim(-3,3)
ylim(0,0.4)
savefig("rejection_sampling.png")
close()

rxs = 6*random_sample(10000)-3
rys = 0.4*random_sample(10000)
cs = norm.pdf(rxs,0,2) * norm.pdf(rys,0.3,0.1)
scatter(rxs,rys,c=cs)
plot(x, norm.pdf(x,0,2), linewidth=3, c='red')
plot(x, norm.pdf(x,0,1), linewidth=3, c='blue')
xlim(-3,3)
ylim(0,0.4)
savefig("metropolis_hastings.png")
close()

