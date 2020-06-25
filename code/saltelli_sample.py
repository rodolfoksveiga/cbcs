from SALib.sample import saltelli
import pandas as pd

# define the input number, its names and limits
problem = {
    'num_vars': 10,
    'names': ['afn', 'area', 'atm', 'azimuth', 'boundaries',
              'cdh', 'envelope', 'hvac', 'lights', 'shgc'],
    'bounds': [[0, 2],
               [0, 796],
               [3, 16],
               [-1, 361],
               [0, 2],
               [0, 120],
               [0, 2],
               [0, 2],
               [9.5, 24.5],
               [0.25, 0.75]]
}

# create a qasi-random sample following saltelli's methodology
param_values = saltelli.sample(problem, 400)
df = pd.DataFrame(param_values, columns = problem['names'])
df.to_csv('/home/rodox/git/commercial_model/code/sample_train.csv')
param_values = saltelli.sample(problem, 80)
df = pd.DataFrame(param_values, columns = problem['names'])
df.to_csv('/home/rodox/git/commercial_model/code/sample_test.csv')
