#!/bin/bash

for year in `seq 2000 2001` ; do
    Rscript 01_daily_temp_weighting.R $year
done
