from SALib.sample import saltelli
import pandas as pd

# define the input number, its names and limits
problem = {
    'num_vars': 7,
    'names': ['hvac', 'envelope', 'lights', 'shgc',
			  'afn', 'azimuth', 'boundaries'],
    'bounds': [[0, 2],
			   [0, 2],
			   [10, 24],
			   [0.3, 0.7],
			   [0, 2],
			   [0, 360],
			   [0, 2]]
}

# create a qasi-random sample following saltelli's methodology
param_values = saltelli.sample(problem, 22)
df = pd.DataFrame(param_values, columns = problem['names'])
df.to_csv('/home/rodox/00.git/02.commercial_model/00.code/sample.csv')

