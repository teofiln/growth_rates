#!/bin/bash

rm W****.csv

ln -s ../Cwaller_2015-01-15/raw_data/TEMP.csv WTEMP.csv
ln -s ../Cwaller_2014-11-22/raw_data/MAIN.csv WSALT.csv
ln -s ../Cwaller_2014-11-22/raw_data/FLASKS.csv WFLAS.csv
ln -s ../Cwaller_2014-11-22/raw_data/FAMI.csv WFAMI.csv
