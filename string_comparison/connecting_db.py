# %%


import sys

# sys.path.append('../')

from pyodbc import connect

try:
    from sql_reports.conf import pwd, uid, server
except:
    from conf import pwd, uid, server
# %%




connection = connect("Driver={SQL Server};"
               f"Server={server};"
               "Database=nlyte;"
               f"uid={uid};pwd={pwd}")

