from SALib.sample import saltelli
import pandas as pd
import numpy as np

# define range
def DefRange(bound, gap):
  mult = (bound[1] - bound[0])*gap
  inter = [bound[0] - mult, bound[1] + mult]
  return(inter)

# generate sample
def GenSample(names, bounds, gap, quals, size):
    consts = np.repeat(gap, len(names))
    consts[quals] = 0
    bounds = [DefRange(bound, const) for bound, const in zip(bounds, consts)]
    problem = {'num_vars': len(names), 'names': names, 'bounds': bounds}
    param_values = saltelli.sample(problem, size)
    df = pd.DataFrame(param_values, columns = problem['names'])
    df.to_csv('~/git/cbcs/result/sobol_sample.csv', index = False)

# main code
names = ['hvac', 'afn', 'area', 'atm', 'azimuth', 'boundaries',
         'cop', 'envelope', 'lights', 'shgc', 'epw']
bounds = [[1, 3], [1, 5], [200, 1200], [5, 20], [0, 360], [1, 3],
          [3, 6], [1, 3], [10, 24], [0.3, 0.7], [1, 412]]
GenSample(names, bounds, 0.02, [0, 1, 5, 7, 10], 500)
