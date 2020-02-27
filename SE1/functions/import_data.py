import os
import json
from collections import defaultdict

def read_ipynb(file,folder):
    cwd = os.getcwd()
    with open(cwd+'\\'+folder+'\\'+file,encoding="utf8") as file:
        data = json.load(file)
        code = []
        markdown = []
        modules = []
        heading = []
        code_output = []

        output_line = []
        for cell in data['cells']:
            clean_cell = list(map(lambda s: s.strip(), cell['source'])) #remove the '\n' at the end of each string in the list
            for line in clean_cell:
                if line[:6]=='import' or line[:4]=='from':
                    modules += [line]
                if line[:1] == '#':
                    heading += [line]

            if cell['cell_type'] == 'markdown':
                markdown += clean_cell

            if cell['cell_type'] == 'code':
                code += clean_cell
                if cell['outputs']!=[]:
                    output_type = cell['outputs'][0]['output_type']
                    if output_type == 'stream':
                        output_line = cell['outputs'][0]['text']
                    if output_type == 'execute_result':
                        output_line = cell['outputs'][0]['data']['text/plain']
                    code_output += output_line

    markdown_str = ' '.join(map(str, markdown))
    code_str = ' '.join(map(str, code))
    code_output_str = ' '.join(map(str, code_output))
    modules = list(set(modules))

    return sorted(modules),heading,markdown_str,code_str,code_output_str #markdown,code

def file_to_dict(files):
    ipynb_dict = defaultdict()
    for file in files:
        temp_dict = {}
        values = read_ipynb(file,folder)
        temp_dict['file_name'] = file
        temp_dict['modules'] = values[0]
        temp_dict['heading'] = values[1]
        temp_dict['markdown_str'] = values[2]
        temp_dict['code_str'] = values[3]
        temp_dict['code_output_str'] = values[4]
        ipynb_dict[file] = temp_dict
    return ipynb_dict
