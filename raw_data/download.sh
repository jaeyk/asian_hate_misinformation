#!/bin/bash

# Download file; The data source: https://zenodo.org/record/5335900#.YS68iXVKgkJ 
wget https://zenodo.org/record/5335900/files/full_dataset_clean.tsv.gz?download=1

# Unzip file 
gunzip full_dataset_clean.tsv.gz
