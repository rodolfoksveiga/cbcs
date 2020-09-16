import json
import math
import numpy as np
import pandas as pd
from SALib.analyze import sobol

def SobolAnalysis(saltelli_path, problem_path, output_dir):
  with open(problem_path) as file:
    problem = json.load(file)
  # load sample
  saltelli = pd.read_csv(saltelli_path)
  # extract targets
  targ = np.asarray(saltelli['targ'])
  solution = sobol.analyze(problem, targ)
  # write solution
  for key in solution:
    solution[key] = list(solution[key])
  for key in ['S2','S2_conf']:
    for i in range(len(solution[key])):
      solution[key][i] = list(solution[key][i])
  with open (output_dir + 'sobol_analysis.json','w') as file:
    json.dump(solution, file, indent = 4)

# main code
problem_path = '/home/rodox/git/cbcs/result/sobol_problem.json'
saltelli_path = '/home/rodox/git/cbcs/result/saltelli_sample.csv'
output_dir = '/home/rodox/git/cbcs/result/'
SobolAnalysis(saltelli_path, problem_path, output_dir)
