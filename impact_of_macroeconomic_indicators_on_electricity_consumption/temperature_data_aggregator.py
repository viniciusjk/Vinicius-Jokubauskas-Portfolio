# %%
import pandas as pd

import numpy as np
import re

import os
import seaborn as sns

# %%
# %%


folders = ['2010', '2011', '2012', '2013', '2014',
           '2015', '2016', '2017', '2018', '2019', '2020', '2021']
# folders = [ '2013', '2014',
#            '2015', '2016', '2017', '2018', '2019',]
# folders = [  '2019','2020']

data_frames = []

# %%
counter = 0
for folder in folders:
    # folder = './2013/'
    folder = './'+folder+'/'
    files = os.listdir(folder)

    problems = []
    for file_ in files:

        if counter % 300 == 0:
            print(f'Year: {folder}  #: {counter}    File name: {file_}')
        try:
            df = pd.read_csv(folder+file_, skiprows=8,
                             delimiter=';', encoding='iso-8859-1')

            # df = pd.read_csv(
            #     './'+folder+files[0], skiprows=8, delimiter=';', encoding='iso-8859-1')
            try:

                df['date'] = pd.to_datetime(
                    df[['DATA (YYYY-MM-DD)', 'HORA (UTC)']].sum(axis=1), format='%Y-%m-%d%H:%M')
                df['year-month'] = df.date.dt.to_period('M')
                df['DATA (YYYY-MM-DD)'] = pd.to_datetime(df['DATA (YYYY-MM-DD)'])
                df['day'] = df.date.dt.to_period('D')
                columns_to_float = [
                    'PRECIPITAÇÃO TOTAL, HORÁRIO (mm)',
                    'PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB)',
                    'PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)',
                    'PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)',
                    'TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)',
                    'TEMPERATURA DO PONTO DE ORVALHO (°C)',
                    'TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C)',
                    'TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C)',
                    'TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C)',
                    'TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C)',
                    'VENTO, RAJADA MAXIMA (m/s)',
                    'VENTO, VELOCIDADE HORARIA (m/s)', ]
                
                for col in columns_to_float:
                    # print(col)
                    df[col] = df[col].str.replace(',', '.').apply(float)
            except:
                df['date'] = pd.to_datetime(df[['Data', 'Hora UTC']].sum(
                    axis=1), format='%Y/%m/%d%H%M UTC')
                df['year-month'] = df.date.dt.to_period('M')
                df['Data'] = pd.to_datetime(df['Data'])
                df['day'] = df.date.dt.to_period('D')
                columns_to_float = [
                    'PRECIPITAÇÃO TOTAL, HORÁRIO (mm)',
                    'PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB)',
                    'PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)',
                    'PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)',
                    'TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)',
                    'TEMPERATURA DO PONTO DE ORVALHO (°C)',
                    'TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C)',
                    'TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C)',
                    'TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C)',
                    'TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C)',
                    'VENTO, RAJADA MAXIMA (m/s)',
                    'VENTO, VELOCIDADE HORARIA (m/s)', ]
                for col in columns_to_float:

                    df[col] = df[col].str.replace(',', '.').apply(float)

            region_state_code_city = re.findall("(?<=\_).*?(?=\_)", file_)

            df['region'] = region_state_code_city[0]
            df['state'] = region_state_code_city[1]
            df['code'] = region_state_code_city[2]
            df['city'] = region_state_code_city[3]

            try:
                # print('tento 2')
                df = df[df['TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)'] > -15].\
                    pivot_table(index=['day',	'year-month',	'region'	, 'state',	'code',	'city'],
                                values='TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)',
                                aggfunc=(np.min, np.median, np.mean, np.max)).reset_index()
            except:
                # print('arrumou')
                df = df[df['TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)'] > -15].\
                    pivot_table(index=['day',	'year-month',	'region'	, 'state',	'code',	'city'],
                                values='TEMPERATURA DO AR - BULBO SECO, HORARIA (°C)',
                                aggfunc=(np.min, np.median, np.mean, np.max)).reset_index()

            data_frames.append(df)
            del(df)
        except:
            problems.append(file_)
            pass
        counter = counter + 1
# %%

# %%
temp_data = pd.concat(data_frames, ignore_index=True)

with open('problems.txt', 'w')as f:
    f.write('\n'.join(problems))


# %%



temp_data.to_csv('temperature.csv', index=False)

temp_data.to_pickle('temperature.pkl.gz')
# %%
del(temp_data)
del(data_frames)

# %%
