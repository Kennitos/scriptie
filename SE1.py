# import
import json
import os
from collections import defaultdict
import numpy as np

folder = 'PythonDataScienceHandbook-notebooks'
files = [file for file in os.listdir(os.getcwd()+'\\'+folder) if file[-6:]=='.ipynb']
