def multi_match_query(code,markdown,modules,highlight):
    query_str = ''
    fields = []

    if code != None:
        fields += ['code_str']
        query_str += ' '+code
    if markdown != None:
        fields += ['markdown_str']
        query_str += ' '+markdown
    if modules != None:
        fields += ['modules']
        query_str += ' '+modules


    q = {}
    query_dict = {}
    multi_match = {}

    multi_match['fields'] = fields
    multi_match['query'] = query_str
    query_dict['multi_match'] = multi_match
    q['query'] = query_dict

    if highlight != None:
        highlight = {"pre_tags":["<b>"],
            "post_tags":["</b>"],
            "fields":{'markdown_str':{}}}
        q['highlight'] = highlight
    return q

def query_string_query(code,markdown,modules,highlight):
    query_str = ''
    fields = []

    if code != None:
        fields += ['code_str']
        query_str += '(code_str:'+code+')'
    if markdown != None:
        fields += ['markdown_str']
        if query_str != '':
            query_str += ' AND '
        query_str += '(markdown_str:'+markdown+')'
    if modules != None:
        fields += ['modules']
        if query_str != '':
            query_str += ' AND '
        query_str += '(modules:'+modules+')'


    q = {}
    query_dict = {}
    query_string = {}

    query_string['query'] = query_str
    query_dict['query_string'] = query_string
    q['query'] = query_dict

    if highlight == True:
        highlight = {"pre_tags":["<b>"],
                     "post_tags":["</b>"],
                     "order":"score",
                     "fields":{'markdown_str':{},'code_str':{}}}
        q['highlight'] = highlight
    return q

# "order":"score",
# "fields":{'_all':{}}} WERKT NIET....


# https://www.elastic.co/guide/en/elasticsearch/reference/6.8/search-request-highlighting.html
