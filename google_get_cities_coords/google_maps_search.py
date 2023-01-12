import re
from secrets import SystemRandom
from time import sleep

import pandas as pd
from selenium import webdriver

def get_lat_long(str_: str):
  
  found = re.findall(r"(?<=\@).+,.+?(?=\,)", str_)
  return found[0]

def search_google_maps(df: pd.core.frame.DataFrame, column: str, sleep_time: float=3.5, chrome_driver_path: str=None ):
  
  url_base = 'https://www.google.com.br/maps?hl=en&q='
  sr = SystemRandom()
  driver = webdriver.Chrome()
  df['url_search'] = url_base + df.loc[:, column].apply(str)
  print(df)
  search_set = set(df.url_search.to_list())
  
  search_list = []
  for url in search_set:
    driver.get(url)
    sleep(sleep_time + sr.uniform(-.200, .200))
    search_list.append((url, driver.current_url))
  
  search_list = dict(search_list)
  df['searched_url'] = df.url_search.map(search_list) 
  
  df['lat_long'] = df.searched_url.apply(get_lat_long)
  
  df['lat'] = df.lat_long.apply(lambda x: x.split(',')[0])
  df['long'] = df.lat_long.apply(lambda x: x.split(',')[1])
