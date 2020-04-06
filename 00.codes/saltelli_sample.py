from SALib.sample import saltelli
import pandas as pd

# define the input number, its names and limits
problem = {
    'num_vars': 5,
    'names': ['x1', 'x2', 'x3', 'x4', 'x5'],
    'bounds': [[0, 5],
			   [2, 102],
			   [10023, 111000],
			   [-3, 3],
			   [-180, 1]]
}

# create a qasi-random sample following saltelli's methodology
param_values = saltelli.sample(problem, 1000)
df = pd.DataFrame(param_values, columns = problem['names'])
df.to_csv('/home/rodox/00.git/02.agencias_bancarias/inputs1.csv')

