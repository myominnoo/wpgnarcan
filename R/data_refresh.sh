#!/bin/bash

echo $1

echo "Updating the total dataset"

## make the repo writable
git config --global --add safe.directory /__w/wpgnarcan/wpgnarcan

Rscript -e "source('data_refresh.R', echo = TRUE)"

if [[ "$(git status --porcelain)" != "" ]]; then
    git config --global user.name 'myominnoo'
    git config --global user.email 'dr.myominnoo@gmail.com'
    git add data/*
    git commit -m "Update data"
    git push
fi