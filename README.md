# Prediction of the 2025 January Loblaws' No Name Groceries Price Strategy

## Overview

This repo contains the project which focuses on predicting the price of the Loblaws' No Name groceries price in January 2025. The model used in this project is a gamma model and focuses only on grocery data observations which have records before September 2024.

## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data for 2024 Canadian Grocery Prices as obtained from Project Hammer.
-   `data/analysis_data` contains the cleaned datasets that were constructed which is used for analysing and prediction.
-   `model` contains fitted models. 
-   `other` contains relevant literature, details about LLM chat interactions, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, test, clean data, model data and prediction data.


## Statement on LLM usage
Aspects of the code and some bibliography were written with the help of ChatGPT, and the entire chat history is available in other/llm_usage/usage.txt.