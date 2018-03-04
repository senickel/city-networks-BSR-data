# Introduction
This folder holds the necessary files to replicate the findings in Bussmann&amp;Nickel 2018. Please note that the data itself is derived by scrapping wikipedia pages in late 2013. Hence, the files that created this dataset are out-dated and not provided here.

# Data
The data is provided in the "data" folder. The file is *semicolon* separatedand has the following variables:  
"City1", "Country1", "City2", "Country2", "Year", "k50", "k100", "k150", "k200", "H1", "H2", "Hboth"

* k50 - k200 are dichotomous and indicate whether both cities of this dyad are lying within a 50-200 km buffer zone of the Baltic Sea.  
* H1: City1 was a member of the Hansa
* H2: City2 was a member of the Hansa
* Hboth: sum of H1 and H2

# Figures
Figure 1 and 2 can be created by executing "data/figures.R". The results will be saved in "output". Figure 3 and 4 were made in the program vizone and thus no script file to recreate them exist.

