'''
Macro de Notepad++ que utiliza o plugin Python Script para Np++.
Ela converte todos os .md's em UTF-8 para o nanoc não reclamar.
O path precisa ser mudado para a pasta local que contenha os .mds.
'''

import os
import sys

path = "C:\Users\Seven\Documents\GitHub\R-adas\md"
for fn in os.listdir(path):
	if fn[-3:] == '.md':
		notepad.open(path + "\\" + fn)
		notepad.runMenuCommand("Encoding", "Convert to UTF-8")
		notepad.save()
		notepad.close()