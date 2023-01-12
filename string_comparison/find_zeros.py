# %%

## Importing Libraries

from numpy.testing import verbose
import pandas as pd
import numpy as np
import scipy as sp
from scipy import stats
from connecting_db import connection
from query_functions import *
import re
# %%

## Functions to remove zeros

def is_number(serial: str):
    """
    Function returns False if there is any character that are not a number in the string
    """
    if len(serial)==0:
        return False
    return len(re.findall('\D', serial)) == 0

def convert_numbers(serial: str):

    """
    If the string contains only numbers, the function returns the converted string to integer
    """

    is_number_bool = is_number(serial)

    if is_number_bool:
        return int(serial)
    else: 
        return serial

# %%

## Nlyte preprocessing


### Nlyte Reading Data
nlyte_query = read_query_file('./query_files/query_maximo_nlyte.sql')
# %%
nlyte_assets = pd.read_sql_query(nlyte_query, connection)

# %%

### Getting No Zeros
"""
Removing zeros on the left of all serial numbers
"""

nlyte_assets.SerialNumber = nlyte_assets.SerialNumber.apply(str)
nlyte_assets['no_left_zeros'] = nlyte_assets.SerialNumber.apply(convert_numbers)

# %%

nlyte_assets.query('Description == "Active"')[['SerialNumber', 'no_left_zeros']]
# %%
nlyte_assets.columns
# %%
help(convert_numbers)
# %%
