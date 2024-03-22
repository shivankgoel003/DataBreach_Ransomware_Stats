# Respiratory - Related Mortality Rates Show A Positive Correlation With Increasing Air pollution

## Overview
This repository contains the data analysis for our research conducted on the impact of air pollution on respiratory and cardiac related mortality rates in Alberta. Our study investigates the relationship between Air Quality Health Index (AQHI), particulate matter (PM2.5) concentrations, and the occurrence of diseases such as Chronic Obstructive Pulmonary Disease, Ischemic Heart Disease, Acute Myocardial Infarction, and Lung Cancer. We provide a detailed examination of the potential correlations between air quality and mortality through modeling  during the study period from 2012 to 2022.


## Data and Methodology
The study focuses on the use of negative binomial regression to model the count data of mortality rates against the air quality indicators in Alberta. The datasets for AQHI and mortality rates have been sourced from the provincial open data portals and official resources of Alberta. The raw data can be found under `data/raw_data`

## Authors and Acknowledgements
The paper is a collaborative effort by Vanshika Vanshika, Shivank Goel, and Navya Hooda. We extend our gratitude to the academic community and the data providers for their contributions to this research.


## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from provincial open data portal of Alberta.
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains fitted models. 
-   `other` contains relevant literature, details about LLM chat interactions, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download and clean data.


## Statement on LLM usage

No auto-complete tools such as co-pilot were used in the course of this project, however, Language Learning Model ChatGPT was  used while writing this paper. It was used for the purpose of code debugging, understanding models, and knowledge of certain topics, which we were not aware of. The chat with the AI bot is also attached as a reference under `inputs\llm`..

