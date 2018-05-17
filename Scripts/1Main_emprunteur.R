#####################################################################################################################################
#   TO PRODUCE THE 4 DESIRED RDATA TABLES, JUST LAUNCH THIS SCRIPT PRECISING HERE THE CRAWLING_FILE AND THE LAST PERIOD,            # 
#   IE THE PERIOD PREVIOUS THE NEW CRAWLING.                                                                                        #
#####################################################################################################################################



crawling_file = "./Crawling/ASSURLAND_LOAN_prices_March.csv" 

lp = "Y17W46"    # lastperiod, from where we want to update, Y--W-- format.
weekormonth = 46 # to adapt according to lp
year = 2017      # to adapt according to lp


Source = "MONTHLY"  # always for loan process

Report = "Assurland_Loan" # loan process



source("./Scripts/updateTable_emprunteur.R")




source("./Scripts/11Cumul_Evolution_emprunteur.R")



source("./Scripts/10MR_global_ranking_Generic_emprunteur.R")

source("./Scripts/Price_Gap-Market_Intensity.R")

source("./Scripts/PCA_Crawling.R")

source("./Scripts/Benchmark_Analysis.R")


#####################################################################################################################################
# WHEN IT'S DONE, YOU HAVE THE RDATA TABLES YOU NEED FOR THE PRODUCTION DATABASE IN "./output_MR_all/Assurland_Loan" :              #
#                                                                                                                                   #
# - data_Assurland_emp.RData    ->    crawling_all variable                                                                         #
# - logevolfinal2_emp.RData    ->    logevolfinal2 variable                                                                         #
# - New_Table_Assurland_emp.RData    ->    New_Table variable                                                                       #
# - summaryTab_emp.RData    ->    SummaryTable variable                                                                             #
#####################################################################################################################################

# Dimitri IORDANOVITCH