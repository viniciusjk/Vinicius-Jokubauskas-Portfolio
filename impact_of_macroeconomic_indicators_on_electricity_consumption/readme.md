# Impact of macroeconomic indicators on electricity consumption

This is the final project for my MBA course in Data Science and Analytics, taken at Universidade de São Paulo (USP) from May 2021 to December 2022. This project aims to investigate the impacts of macroeconomic indicators on Brazil's power consumption, as described in the abstract below:

**Abstract**

> The power consumption present in several social sectors could work as an interesting gauge of the current economic situation. The relationship between power consumption and macroeconomic and environmental investigation can bring more understanding of the economic relations, working by itself as another gauge. The project's goal is to seek relevant statistical  relationships  between  power  consumption  and  the  economic  and  environmental selected indicators — PIB, IPCA, SELIC, temperature, credit availability (recursos livres), and minimum wage—, using three different slices of our data — the total power consumption, the residential  power  consumption,  and  the  low-income  residential  power  consumption  —  for  a better understanding of the  relationship  of  the indicators and power consumption. To investigate  the  relationships,  it  was  made  linear  models  for  each  of  the  selected  indicators, and  for  each  of  the  sectors  it  was  created  new  variables,  such  as  moving  averages  and accumulated values; after the individual models were created and studied it was made three new models for each of the economical slices using all indicators with statistical significance together. The general power consumption has shown elasticity to all indicators studied, while the residential power consumption was more sensible to environmental and income indicators still  being  sensitive  to  the  other  indicators;  the  low-income hasn’t had any sensitivity to the minimum wage and the temperature changes. Macroeconomic and environmental indicators have worked as good predictors of power consumption changes. Keywords: Inflation, gdp, linear, interest, electricity

## Files

The main files are:

- The preliminary study ([estudo_preliminar.md](estudo_preliminar.md)) worked as an exploratory study of the power consumption dataset and the macroeconomic impacts. This file has a broader scope, there was no in-depth study only the search for viable macroeconomic indicators that could be used in the final project.
- Residential power consumption impact ([residential_power_consumption_impact.md](residential_power_consumption_impact.md)) – This file is the codebase used for the project's final study. After studying the macroeconomic indicators and finding the sectors in the energy consumption that would feel a bigger impact of the indicators, the in-depth study was done in this file, being the residential and low-income residential sectors chosen and the indicators being:
  - Inflation
  - GDP (PIB)
  - Interest Rate (SELIC)
  - Temperature
  - Credit Availability (Recursos Livres)
  - Minimum Wage
- [Final paper](impact_of_macroeconomic_indicators_on_electricity_consumption.pdf) – The final paper delivered as result of the project (written in Portuguese)

### Additional files

- [libs.R](libs.R) – The file that was used to load all the libraries necessary for the project, it was used to keep all the libraries used in the same place to set universal variables and to make sure all files needed were downloaded
- [personal_library.R](personal_library.R) – A file with functions used throughout the whole project
- [temperature_data_aggregator.py](temperature_data_aggregator.py) – This script aggregates and selects the measures used in the project from all the temperature files downloaded from Brazil's National Metereologic Institute ([INMET](https://portal.inmet.gov.br/dadoshistoricos))