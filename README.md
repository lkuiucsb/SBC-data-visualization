# SBC-data-visualization
Data visualization for SBC long-term time series datasets. 

The vw_time series_entities.csv file is the list of long-term time series datasets in SBCLTER. 

The read_sbc_data.r is a function that read in the data index csv file and locate/download the data table (in csv or txt) for a given dataset name.  

figure_long_term_studies.R is R script that plot all the time series figures shown on SBC website: https://sbclter.msi.ucsb.edu/data/visuals/tsvisual/. 

We want to provide this code to students/researchers who would like to reproduce the graphs and modify them to fit research purposes. 

To implement, please open the figure_long_term_studies.R script and run the section that you need for producing the figures. 
