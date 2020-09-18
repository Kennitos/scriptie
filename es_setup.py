# imports
import os
import json

import pandas as pd
import numpy as np

from elasticsearch import Elasticsearch
from tqdm import tqdm_notebook

### functions

def code_output(cell,temp_dict):
    """
    Explanation function
    """
    if cell['outputs']!=[]:
        output_type = cell['outputs'][0]['output_type']
        temp_dict['output_type'] = output_type
        if output_type == 'stream':
            temp_dict['output'] = cell['outputs'][0]['text']
        elif output_type == 'error':
            temp_dict['output'] = cell['outputs'][0]['traceback']
        elif temp_dict['output_type'] == 'display_data':
            temp_dict['output'] = 'displayed data'
        elif 'data' in cell['outputs'][0].keys():
            temp_dict['output'] = list(cell['outputs'][0]['data'].values())
        elif 'text' in cell['outputs'][0].keys():
            temp_dict['output'] = cell['outputs'][0]['text']
        elif 'ename' in cell['outputs'][0].keys():
            temp_dict['output'] = cell['outputs'][0]['ename']+cell['outputs'][0]['evalue']
        else:
            temp_dict['output'] = 'unknown'
            print(cell)
    else:
        temp_dict['output'] = 'empty'
    return temp_dict


def read_ipynb_cell(cell_id,cell_dict,file,folder,location,repo):
    """
    Explanation function
    """
    with open(location,encoding="utf8") as notebook:
        data = json.load(notebook)
        file_cell = 0
        nbformat = data['nbformat']
#         print(nbformat,'- file_name',location)
        
        if nbformat == 4: # current nbformat
            data_cells =  data['cells']
        elif nbformat == 3: # old nbformat
            data_cells =  data['worksheets'][0]['cells']

        for cell in data_cells:
            temp_dict = {}
            if cell['cell_type'] == 'code' and nbformat == 3: #cell['source'] doesn't exist within this condition, use cell['input']
                text = cell['input']
            else:
                text = cell['source']
            clean_cell = list(map(lambda s: s.strip(), text)) #remove the '\n' at the end of each string in the list         
            single_string = ' '.join(clean_cell)
            lines = len(clean_cell)

            temp_dict['file_cell'] = file_cell
            temp_dict['file'] = file
            temp_dict['nbformat'] = data['nbformat']
            temp_dict['folder'] = folder
            temp_dict['repo'] =  repo
            temp_dict['location'] = location
            temp_dict['string'] = clean_cell
#             temp_dict['char'] = single_string
            temp_dict['lines'] = lines
            temp_dict['cell_type'] = cell['cell_type']
            if cell['cell_type'] == 'code':
                temp_dict = code_output(cell,temp_dict)

            cell_dict[cell_id] = temp_dict
            
            cell_id += 1
            file_cell += 1 
    return cell_id,cell_dict


def files_to_dict(file_dict):
    """
    Explanation function
    """
    cell_id = 0
    cell_dict = {}

    for file in file_dict.keys():
        try:
            file_name = file_dict[file]['file']
            folder = file_dict[file]['folder']
            location = file_dict[file]['location']
            repo = file_dict[file]['repo']
            #kan ik dit niet in één regel schrijven, ff controleren nog bijv a,b,c = dict.values()
            
            cell_id_dict = read_ipynb_cell(cell_id,cell_dict,file_name,folder,location,repo)
            cell_id = cell_id_dict[0]
            cell_dict = cell_id_dict[1]
        except Exception as e:
#             print(e)
            print("failed for file:",cell_dict[file]['file'])

        
    return cell_id,cell_dict


def rec_to_actions(df):
    for record in df.to_dict(orient="records"):
        yield ('{ "index" : { "_index" : "%s", "_type" : "%s" }}'% (INDEX, TYPE))
        yield (json.dumps(record, default=int))
      

def index_marks(nrows, chunk_size):
    return range(1 * chunk_size, (nrows // chunk_size + 1) * chunk_size, chunk_size)


def split(dfm, chunk_size):
    indices = index_marks(dfm.shape[0], chunk_size)
    return np.split(dfm, indices) 
    

def create_SE_from_folder(repo_name,es,folder,file_id):
    """
    Explenation
    """
    cwd = os.getcwd()
    if folder == 'single_files':
        path = cwd+"\\"+folder
    elif folder == 'notebooks1':
        path = 'E:\\Files\\Universiteit\\thesis\\notebooks1'
    elif folder == 'notebooks':
        path = cwd+"\\"+folder
    else:
        path = cwd+"\\"+folder+"\\"+repo_name
#         print(path)
    # CREATE DICT FOR ALL 'IPYNB' FILES
#     file_id = 0
    file_dict = {}
    
    for root, dirs, files in os.walk(path):
        for file in files:
            if file.endswith(".ipynb"):
                temp_dict = {}
                temp_dict['file'] = file
                temp_dict['folder'] = root.split('\\')[-1]
                temp_dict['location'] = os.path.join(root,file)
                temp_dict['repo'] = repo_name

                file_dict[file_id] = temp_dict
                file_id += 1
    
    # CREATE DICT FOR ALL CELLS
    cell_dict = files_to_dict(file_dict)[1]
    
    # CREATE DATAFRAME FROM DICT
    cell_df = pd.DataFrame.from_dict(cell_dict,orient='index')
    cell_df.index = cell_df.index.set_names(['cell_id'])
    cell_df = cell_df.fillna('empty').reset_index()
    return cell_df
    # PUT DATAFRAME INTO ELASTIC SEARCH
    
#     r = es.bulk(rec_to_actions(cell_df))
    for chuck in split(cell_df, 50000):
        r = es.bulk(rec_to_actions(chuck))
    return es,file_id,cell_df
#     return cell_df,es,file_id

### Create local elastic search variable

### Put single_files into elastic search

HOST = 'http://localhost:9200/'
es = Elasticsearch(hosts=[HOST]) 


INDEX = "gallery"
TYPE = "record"

gallery_single_df = create_SE_from_folder('unkown',es,'single_files',0)
gallery_single_df

for chuck in split(gallery_single_df, 500):
    try:
        r = es.bulk(rec_to_actions(chuck))
    except:
        pass
        # print('failed, skip this df')


### Put github repositories into elastic search

folder = 'repos'
path = os.getcwd()+'//'+folder
dir_list = os.listdir(path)
file_id = 0
gallery_repo_df = None

for repo in dir_list:
    try:
        df = create_SE_from_folder(repo,es,folder,file_id)

        if gallery_repo_df is None:
            gallery_repo_df = df
        else:
            gallery_repo_df = pd.concat([gallery_repo_df,df])
        file_id = 0
    except Exception as e:
        pass
        # print(e,repo)

# gallery_repo_df   
# cell_df,es,file_id

# HOST = 'http://localhost:9200/'
# es = Elasticsearch(hosts=[HOST]) 


# INDEX = "gallery_repos"
# TYPE = "record"

for chuck in split(gallery_repo_df, 500):
    try:
        r = es.bulk(rec_to_actions(chuck))
    except:
        pass
        # print('failed, skip this df')


### Put notebook collection into elastic search

HOST = 'http://localhost:9200/'
es = Elasticsearch(hosts=[HOST],timeout=30,max_retries=10, retry_on_timeout=True) 


INDEX = "restart"
TYPE = "record"

sample_df = create_SE_from_folder('sample',es,'notebooks',0)

sample_df[-1:]['']

# sample_df.drop(columns=['char'])

# set(sample_df.output_type)

sample_df[sample_df.output_type=='display_data']

for chuck in split(sample_df, 500):
    try:
        r = es.bulk(rec_to_actions(chuck))
    except:
        print('failed, skip this df') # UITELGGEN WAT ER MIS GING ZONDER DE TRY/EXCEPT > IN HET VERSLAG (224)
    
    
# for chuck in tqdm_notebook(split(sample_df.drop(columns=['char']), 10000)):
#     r = es.bulk(rec_to_actions(chuck))

# !curl -XDELETE "localhost:9200/sample_notebooks"
# !curl "http://localhost:9200/_cat/indices?v"





