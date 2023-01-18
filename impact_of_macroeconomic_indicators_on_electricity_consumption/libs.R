libs <- c('tidyverse', 
          'fable',
          'tmap',
          'e1071', ## Required for tmap
          'rgdal',
          'tsibble',
          'lubridate',
          'zoo',
          'plotly',
          'nortest',
          'olsrr',
          'jsonlite',
          'readxl',
          'PerformanceAnalytics',
          'scales')

sapply(libs[!libs %in% installed.packages()], install.packages, character=T)
sapply(libs[libs %in% installed.packages()], require, character=T)

step_wise_k <-  3.841459

## Downloading shapefile


if (!'BR.zip' %in% list.files()){
  print('R')
  download.file('ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2021/Brasil/BR/BR_UF_2021.zip',
                './data/BR.zip',
                method='curl')
  unzip('BR.zip', exdir='BR')
}
