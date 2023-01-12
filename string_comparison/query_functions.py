#%% 
import os
import datetime as dt

def save_query(filename: str, query: str, **kwargs):
 
    filename = filename if sql_in_name(filename) else filename + '.sql'
        
    filename = add_timestamp(filename) if filename in os.listdir() else filename
 
    
    with open(filename, 'w', **kwargs) as f:
        f.write(query)


def read_query_file(filename: str, **kwargs):

    filename = filename if sql_in_name(filename) else filename + '.sql'


    
    
    try:
        with open(filename, 'r', **kwargs) as f:
            query = f.read()
        return query

    except:
        print('File Does Not exist')


def sql_in_name(filename: str):

    return '.sql' == filename[-4:]

def add_timestamp(filename:str):

    return dt.datetime.now().strftime('%Y-%m-%d_%H%M%S_') + filename
