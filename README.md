# Analyzing How Organization Size, Sector, and Cybersecurity Strategies Influence Impacts on Cyberattacks


## Overview

In our world, where everything is online, the danger of cyberattacks—like hacking—is going up. 
This repository is part of a statistical analysis project that investigates data breaches and ransomware attacks. The project discusses the influence of organizational size, sector, and cybersecurity strategies on cyberattack impacts. We wanted to know whether being a big company or working in fields like healthcare or schools made it harder to keep safe from these attacks. Using data breach dataset available at the University of Queensland, we looked at a lot of cyberattacks from 2004 to 2019. We learned some big things: big companies often have more cyber problems, and hackers really go after hospitals and schools. But, if a company has a good plan to fight cyberattacks, it can help a lot.


## Data and Methodology

We looked closely at information on cyberattacks that the University of Queensland collected over 15 years. This data helped us understand how different companies are affected by these online threats. It showed us who gets hit the most and what can help protect them. The raw data can be found under `data/raw_data`

## Authors and Acknowledgements
The paper is an  We extend our gratitude to the academic community and the data providers for their contributions to this research.


## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from provincial open data portal of Alberta.
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains fitted models. 
-   `other` contains relevant literature, details about LLM chat interactions, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download and clean data.


## Statement on LLM usage

No auto-complete tools such as co-pilot were used in the course of this project, however, Language Learning Model ChatGPT was  used while writing this paper. It was used for the purpose of latex coding to draw tables, code debugging, understanding models, and knowledge of certain topics, which I was not aware of. The chat with the AI bot is also attached as a reference under `inputs\llm`..

