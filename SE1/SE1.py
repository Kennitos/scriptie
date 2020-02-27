# IMPORTS
import os
import sys
# import pandas as pd
# from elasticsearch import Elasticsearch # <== May need to pip install this
# from tqdm import tqdm_notebook

# FOLDER IN WHICH THE PROJECT LOCATED
located_map = "SE1"

# ADD THE PARENT FOLDER
parent_dir_name = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
print(parent_dir_name)
sys.path.append(parent_dir_name+"\\"+located_map+"\\functions")
#
# # LOCAL IMPORTS
import import_data as id
import elastic_search as esimport

# CREATE DATAFRAME OFF ALL THE FILES
folder = 'PythonDataScienceHandbook-notebooks'
files = [file for file in os.listdir(os.getcwd()+'\\'+folder) if file[-6:]=='.ipynb']

ipynb_dict = id.file_to_dict(files)
notebooks_df = pd.DataFrame.from_dict(ipynb_dict,orient='index').reset_index(drop=True)



# SET UP LOCAL ELASTIC SEARCH
HOST = 'http://localhost:9200/'
es = Elasticsearch(hosts=[HOST])

INDEX="handboek"
TYPE= "record"

# split up the dataframe
chunks = esimport.split(notebooks_df, 9000)

# Now bulk index all the chunks
c = len(chunks)
for c in tqdm_notebook(chunks):
    if c.shape[0]>0:
        r = es.bulk(esimport.rec_to_actions(c)) # return a dict
print('Done')

#
