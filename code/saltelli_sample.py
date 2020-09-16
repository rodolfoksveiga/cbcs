import json
import numpy as np
import pandas as pd
from SALib.sample import saltelli

# define range
def DefRange(bound, gap):
  mult = (bound[1] - bound[0])*gap
  inter = [bound[0] - mult, bound[1] + mult]
  return(inter)

# generate sample
def GenSample(names, bounds, gap, qual_vars, size, output_dir, save_problem = True):
    consts = np.repeat(gap, len(names))
    consts[qual_vars] = 0
    bounds = [DefRange(bound, const) for bound, const in zip(bounds, consts)]
    problem = {'num_vars': len(names), 'names': names, 'bounds': bounds}
    param_values = saltelli.sample(problem, size)
    df = pd.DataFrame(param_values, columns = problem['names'])
    df.to_csv(output_dir + 'saltelli_sample.csv', index = False)
    if save_problem:
      problem_path = output_dir + 'sobol_problem.json'
      with open(problem_path, 'w') as file:
        json.dump(problem, file, indent = 4)

# main code
names = ['hvac', 'afn', 'area', 'atm', 'azimuth', 'boundaries',
         'cop', 'envelope', 'lights', 'shgc', 'epw']
bounds = [[1, 3], [1, 5], [200, 1200], [5, 20], [0, 360], [1, 3],
          [3, 6], [1, 3], [10, 24], [0.3, 0.7], [1, 412]]
qual_vars = [0, 1, 5, 7, 10]
output_dir = '/home/rodox/git/cbcs/result/'
GenSample(names, bounds, 0.02, qual_vars, 500, output_dir)
