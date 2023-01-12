# %%
import pandas as pd
import requests as r
from bs4 import BeautifulSoup
from selenium import webdriver
import time
import numpy as np

# %%
urls = {'serie_a' : 'https://www.oddsportal.com/soccer/brazil/serie-a/',
'serie_b' : 'https://www.oddsportal.com/soccer/brazil/serie-b/',
'copa_brasil' : 'https://www.oddsportal.com/soccer/brazil/copa-do-brasil/',
'libertadores' : 'https://www.oddsportal.com/soccer/south-america/copa-libertadores/',
'sulamericana' :  'https://www.oddsportal.com/soccer/south-america/copa-sudamericana/'}

 

# %%
firefox = webdriver.Firefox(executable_path=r"./geckodriver.exe")

# %%
soups = {}
for var, url in zip(urls.keys(), urls.values()):
    firefox.get(url)
    time.sleep(3)
    soups[var] = BeautifulSoup(firefox.page_source, 'html.parser')

firefox.close()

# %%
data = []
for var, soup in zip(soups.keys(), soups.values()):


    rows = soup.find('table', {'id': 'tournamentTable',
                    'class': 'table-main'}).tbody.find_all('tr')


    # data = []
    for row in rows:
        r = []
        if not row.find_all('td', class_='odds-nowrp') == []:
            try:
                game = row.find(
                    'td', class_='name table-participant').text.replace('\xa0', '').split(' - ')
            except:
                next
            if not game == []:
                r.append(var)
                r.append(game[0])
                r.append(game[1])
                for d in row.find_all('td', class_='odds-nowrp'):
                    r.append(float(d.text))

            data.append(r)

    # odds.append(data)

# %%
df=pd.DataFrame(data, columns=['competition','home', 'away', 'odd_h', 'odd_d', 'odd_a'])

# %%
odds_sum = (1/df[['odd_h', 'odd_d', 'odd_a']
                 ].values).sum(axis=1).reshape([-1, 1])

probs = (np.round(100*(1/df[['odd_h', 'odd_d', 'odd_a']].values)/odds_sum,2))


df[['p_h', 'p_d', 'p_a']] = probs

df

# %%



