# MBFBigDataRandomForest

## Authors
* Tim Graf
* Kevin Joerg
* Moritz Daendliker


## Background

This repository is part of a project from the course "Big Data Analytics" at the University of St. Gallen which was held in the fall semester 2021 as part of the Master in Banking & Finance curriculum. 

## About the project

The aim of the following project was to project US voting outcomes on a county level with the use of data on ~3 million used-cars that were listed in the month prior to the elections. The study is able to forecast 300-400 counties depending on the computational method in use.

## How to execute this code

1. Download the datasets from: 

* US Used cars dataset: https://www.kaggle.com/ananaymital/us-used-cars-dataset
* MIT Election Lab Voting Data: https://github.com/MEDSL/2020-elections-official
* State Level Election Outcome: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX

2. Run code files sequentially

3. Check presentation for an overview on the methodology and results. 

### Note on executing this code 

* Linear regression uses GPU acceleration method. This may only work on NVIDIA GPUs which are CUDA enabled. 
* XGBoost takes up a lot of memory. If you do not have access to a powerful computer (on-premise or cloud), you may want to go ahead with 05_XGB_OOM (out-of-memory)
* XGBoost takes a long time to compute. 


## Disclaimer

This code was used for research-purposes only. 