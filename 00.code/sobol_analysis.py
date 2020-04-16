from SALib.sample import saltelli
from SALib.analyze import sobol
from SALib.test_functions import Ishigami
import numpy as np
import pandas as pd
from six.moves import xrange
import json

# define the input number, its names and limits
problem = {
    'num_vars': 8,
    'names': ['ghr','hvac','lights','boundaries','envelope','shgc','building','afn'],
    'bounds': [[1, 2],
               [9.9, 23.6],
               [1, 2],
               [2.5, 3.7],
               [0.3, 0.7],
               [1, 4],
               [0, 1]],
}

#IMPORTAR AS VARIÁVEIS ALEATÓRIAS CALCULADAS PELO MÉTODO DE SALTELLI PARA O DATA FRAME DE ENTRADA DO METAMODELO (LEMBRAR DE ARREDONDAR OS VALORES DE CT_COB E CT_PAR PARA 1, 2 E 3)

#CRIAR UM VETOR (Y) COM O RESULTADO DO CÁLCULO DO METAMODELO COMO ARQUIVO .csv
#IMPORTAR O VETOR (Y) PARA O PYTHON


outputs = pd.read_csv('.csv')
Y = np.asarray(Y)
Y_new = [x for x in range(len(Y))]
for x in xrange(len(Y)):
	Y_new[x] = Y[x][0]
Y = Y_new
Y = np.asarray(Y)
#REALIZAR A ANÁLISE DE SENSIBILIDADE DE SOBOL
sa = sobol.analyze(problem, Y, print_to_console = False)
#TRANSFORMAR O ARQUIVO EM .JSON
for key in sa:
	sa[key] = list(sa[key])
for key in ['S2','S2_conf']:
	for line in range (len(sa[key])):
		sa[key][line] = list(sa[key][line])
with open ('sa_c_sp_cob.json','w') as file:
	json.dump(sa,file)
