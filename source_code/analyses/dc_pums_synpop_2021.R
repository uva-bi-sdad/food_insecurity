#program to generate the synthetic data for Fairfax County, VA  
options(scipen=999)
library(tidyverse)
library(dplyr)
library(data.table)

#read in data and harmonize the variables
 ipf <- read.csv("https://github.com/uva-bi-sdad/food_insecurity/raw/main/documents/products/derived_variables/dc_acs_ipf_estimates_with_pums_2021.csv")
#create a tract variable from, geoid
  ipf$tract <- substr(ipf$geoid, 6, 11); length(unique(ipf$tract)) 
   ipf$puma <- paste0("00",ipf$puma)
puma <- read.csv("https://github.com/uva-bi-sdad/food_insecurity/raw/main/documents/products/derived_variables/dc_acs_pums_derived_variables_2021.csv")
           puma <- puma[, -1]; str(puma) 
#change serialno and hh_comb to factors 
      puma$puma <- as.character(puma$puma); puma$puma <- paste0("00",puma$puma)
  puma$serialno <- as.factor(puma$serialno)
   puma$hh_comb <- as.factor(puma$hh_comb) 
View(data.frame(table(puma$hh_comb, useNA="always")))
#insert the leading 0 in the hh_comb with only 4-digits or 5-digits
    levels(puma$hh_comb)[levels(puma$hh_comb)=="100"] <- "000100"
   levels(puma$hh_comb)[levels(puma$hh_comb)=="1000"] <- "001000"
  levels(puma$hh_comb)[levels(puma$hh_comb)=="10000"] <- "010000"
View(data.frame(table(puma$hh_comb, useNA="always"))); str(puma)
  
#number of observations in each pumas using 2010 geographies  
#00101 00102 00103 00104 00105  
# 2985  2933  3610  2989  5069 

#number of tracts in each puma using 2020 geographies 
#00101 00102 00103 00104 00105 00106
#   29    31    34    44    32    36

#data wrangling
             ipf_long <- ipf[with(ipf, order(puma, tract, income_recode, size_recode)), ]
dim(ipf_long); names(ipf_long)
             ipf_long <- ipf_long[ipf_long$tract<980000, ]
dim(ipf_long); names(ipf_long); View(ipf_long); length(unique(ipf_long$tract)) 

#check number of tracts and pumas
length(unique(ipf_long$tract)); length(unique(ipf_long$puma))

############################################################################
#00101 - NO CHANGES FROM 2010 to 2020 geographies
#put ipf_long into a wide format to use in generating the synpop keep only puma 00101
#00101 census tracts have not changed from 2010 to 2020

      ipf_long_00101 <- ipf_long[ipf_long$puma=="00101",] 
#dim(ipf_long_00101); names(ipf_long_00101); View(ipf_long_00101); length(unique(ipf_long_00101$tract)) 
#create tract column names
   ipf_long_00101$CT <- paste0("CT", ipf_long_00101$tract) 
                temp <- ipf_long_00101[, c(2,3,4,7)]; dim(temp)
      ipf_wide_00101 <- temp %>% pivot_wider(names_from=CT, values_from=ipf); rm(temp) 
#create a variable to use as the index
ipf_wide_00101$index <- c(1:45)
      ipf_wide_00101 <- ipf_wide_00101[, order(names(ipf_wide_00101))]
#dim(ipf_wide_00101); names(ipf_wide_00101); View(ipf_wide_00101)
    
#pull out 00101 from the puma data 
   puma00101 <- puma[puma$puma=="00101", ] 
#dim(puma00101); names(puma00101); View(puma00101)
    syn00101 <- cbind(tract=NA, puma00101[1,])
#get the number of tracts within puma 00101   
   no_tracts <- length(unique(ipf_long_00101$tract))
#check if the puma has a household in each cell and record the missing cells
missing_cell <- which(apply(ipf_wide_00101[1:no_tracts],1,sum)==0)
       cells <- if(length(missing_cell)==0) c(1:45) else c(1:45)[-c(missing_cell)]
   
#synthetic populations for census tracts in puma 00101
sum(ipf_wide_00101[,c(1:no_tracts)]) #number of households in puma 00101
for(j in 1:no_tracts){
  for(i in 1:length(cells)){
        I <- cells[i]; print(I)
        J <- unname(unlist(ipf_wide_00101[I,j])); print(J)
    temp1 <- na.omit(puma00101[puma00101$index==I, ])  
     prob <- temp1$hh_wt/sum(temp1$hh_wt)
    temp2 <- temp1[sample(1:nrow(temp1), size=J, replace=TRUE, prob=prob), ];  
    temp2 <- cbind(tract=rep(names(ipf_wide_00101)[j], J), temp2)
 syn00101 <- rbind(syn00101, temp2)
rm(I, J, temp1, prob, temp2)
}}
 
#cleanup
      syn00101 <- syn00101[-1,] 
syn00101$tract <- substring(syn00101$tract, 3, 8) 
dim(syn00101)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_00101[,1:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn00101$index))[,2]-test

rm(no_tracts, cells, missing_cell)


############################################################################
#00104 - NO CHANGES FROM 2010 to 2020 geographies
#put ipf_long into a wide format to use in generating the synpop keep only puma 00104
#00104 census tracts have not changed from 2010 to 2020

      ipf_long_00104 <- ipf_long[ipf_long$puma=="00104",] 
#dim(ipf_long_00104); names(ipf_long_00104); View(ipf_long_00104); length(unique(ipf_long_00104$tract)) 
#create tract column names
ipf_long_00104$CT <- paste0("CT", ipf_long_00104$tract) 
             temp <- ipf_long_00104[,c(2,3,4,7)]
   ipf_wide_00104 <- temp %>% pivot_wider(names_from=CT, values_from=ipf) 
#create a variable to use as the index
ipf_wide_00104$index <- c(1:45)  
      ipf_wide_00104 <- ipf_wide_00104[, order(names(ipf_wide_00104))]
#dim(ipf_wide_00104); names(ipf_wide_00104); View(ipf_wide_00104)

#pull out 00104 from the puma data 
   puma00104 <- puma[puma$puma=="00104", ]  
#dim(puma00104); names(puma00104); View(puma00104)
    syn00104 <- cbind(tract=NA, puma00104[1,])
#get the number of tracts within puma 00104   
   no_tracts <- length(unique(ipf_long_00104$tract)) 
#check if the puma has a household in each cell and record the missing cells
missing_cell <- which(apply(ipf_wide_00104[1:no_tracts],1,sum)==0)
       cells <- if(length(missing_cell)==0) c(1:45) else c(1:45)[-c(missing_cell)]
    
#synthetic populations for census tracts in puma 00104               
sum(ipf_wide_00104[,c(1:no_tracts)]) #number of households in puma 00104
for(j in 1:no_tracts){
  for(i in 1:length(cells)){
       I <- cells[i]; print(I)
       J <- unname(unlist(ipf_wide_00104[I,j])); print(J)
   temp1 <- na.omit(puma00104[puma00104$index==I, ])  
    prob <- temp1$hh_wt/sum(temp1$hh_wt)
   temp2 <- temp1[sample(1:nrow(temp1), size=J, replace=TRUE, prob=prob), ];  
   temp2 <- cbind(tract=rep(names(ipf_wide_00104)[j], J), temp2)
syn00104 <- rbind(syn00104, temp2)
rm(I, J, temp1, prob, temp2)
  }}

#cleanup
syn00104 <- syn00104[-1,] 
syn00104$tract <- substring(syn00104$tract, 3, 8) 
dim(syn00104)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_00104[,1:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn00104$index))[,2]-test

rm(no_tracts, cells, missing_cell) 


############################################################################
#00102 - FROM 2010 to 2020 geographies
#include tracts: 92.01, 93.01, 94.00, 95.03, 95.04, 95.09 from puma 00103
#put ipf_long into a wide format to use in generating the synpop keep only puma 00102
    
      ipf_long_00102 <- ipf_long[ipf_long$puma=="00102",] 
#dim(ipf_long_00102); names(ipf_long_00102); View(ipf_long_00102); length(unique(ipf_long_00102$tract)) 
#create tract column names
   ipf_long_00102$CT <- paste0("CT", ipf_long_00102$tract) 
                temp <- ipf_long_00102[,c(2,3,4,7)]
      ipf_wide_00102 <- temp %>% pivot_wider(names_from=CT, values_from=ipf); rm(temp)
#create a variable to use as the index
ipf_wide_00102$index <- c(1:45) 
      ipf_wide_00102 <- ipf_wide_00102[, order(names(ipf_wide_00102))]
View(ipf_wide_00102)
#remove tracts not in 2020 geography
      ipf_wide_102 <- ipf_wide_00102[, c(1:19,25:27,29:34)]
  ipf_wide_102_103 <- ipf_wide_00102[, c(20:24,28,32:34)]    
#dim(ipf_wide_102); names(ipf_wide_102)
#dim(ipf_wide_102_103); names(ipf_wide_102_103)

#pull out 00102 from the puma data 
   puma00102 <- puma[puma$puma=="00102", ] 
#dim(puma00102); names(puma00102); View(puma00102)
    syn00102 <- cbind(tract=NA, puma00102[1,])
#get the number of tracts within puma 00101   
   no_tracts <- length(unique(ipf_wide_102))-3
#check if the puma has a household in each cell and record the missing cells
missing_cell <- which(apply(ipf_wide_102[1:no_tracts],1,sum)==0)
       cells <- if(length(missing_cell)==0) c(1:45) else c(1:45)[-c(missing_cell)]

#synthetic population for census tracts in puma 00102               
sum(ipf_wide_102[,c(1:no_tracts)]) #number of households in puma 00102
for(j in 1:no_tracts){
  for(i in 1:length(cells)){
        I <- cells[i]; print(I)
        J <- unname(unlist(ipf_wide_102[I,j])); print(J)
    temp1 <- na.omit(puma00102[puma00102$index==I, ])  
     prob <- temp1$hh_wt/sum(temp1$hh_wt)
    temp2 <- temp1[sample(1:nrow(temp1), size=J, replace=TRUE, prob=prob), ];  
    temp2 <- cbind(tract=rep(names(ipf_wide_102)[j], J), temp2)
 syn00102 <- rbind(syn00102, temp2)
rm(I, J, temp1, prob, temp2)
}}

#cleanup
      syn00102 <- syn00102[-1,] 
syn00102$tract <- substring(syn00102$tract, 3, 8) 
dim(syn00102)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_102[,1:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn00102$index))[,2]-test

rm(no_tracts, cells, missing_cell) 

#get the 00102 tracts that are in 00103
#pull out 00103 from the puma data 
   puma00103 <- puma[puma$puma=="00103", ] 
#dim(puma00103); names(puma00103); View(puma00103)
   syn001023 <- cbind(tract=NA, puma00103[1,])
#get the number of tracts within puma 00101   
   no_tracts <- length(unique(ipf_wide_102_103))-3
#check if the puma has a household in each cell and record the missing cells
missing_cell <- which(apply(ipf_wide_102_103[1:no_tracts],1,sum)==0)
       cells <- if(length(missing_cell)==0) c(1:45) else c(1:45)[-c(missing_cell)]

#synthetic population for census tracts in puma 00102               
sum(ipf_wide_102_103[,c(1:no_tracts)]) #number of households in puma 00102
for(j in 1:no_tracts){
  for(i in 1:length(cells)){
        I <- cells[i]; print(I)
        J <- unname(unlist(ipf_wide_102_103[I,j])); print(J)
    temp1 <- na.omit(puma00103[puma00103$index==I, ])  
     prob <- temp1$hh_wt/sum(temp1$hh_wt)
    temp2 <- temp1[sample(1:nrow(temp1), size=J, replace=TRUE, prob=prob), ];  
    temp2 <- cbind(tract=rep(names(ipf_wide_102_103)[j], J), temp2)
syn001023 <- rbind(syn001023, temp2)
rm(I, J, temp1, prob, temp2)
}}

#cleanup
      syn001023 <- syn001023[-1,] 
syn001023$tract <- substring(syn001023$tract, 3, 8) 
dim(syn001023)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_102_103[,1:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn001023$index))[,2]-test

syn00102 <- data.frame(rbind(syn00102, syn001023[-1,])); dim(syn00102)

rm(no_tracts, cells, missing_cell) 


############################################################################
#00103 - FROM 2010 to 2020 geographies
#include tracts: 72.03 and 106.02 from puma 00105
#put ipf_long into a wide format to use in generating the synpop keep only puma 00102

      ipf_long_00103 <- ipf_long[ipf_long$puma=="00103",] 
#dim(ipf_long_00103); names(ipf_long_00103); View(ipf_long_00103); length(unique(ipf_long_00103$tract)) 
#create tract column names
   ipf_long_00103$CT <- paste0("CT", ipf_long_00103$tract) 
                temp <- ipf_long_00103[,c(2,3,4,7)]
      ipf_wide_00103 <- temp %>% pivot_wider(names_from=CT, values_from=ipf); rm(temp)
#create a variable to use as the index
ipf_wide_00103$index <- c(1:45) 
      ipf_wide_00103 <- ipf_wide_00103[, order(names(ipf_wide_00103))]
View(ipf_wide_00103)

#remove tracts not in 2020 geography
    ipf_wide_103 <- ipf_wide_00103[, -c(10,33)]
ipf_wide_103_105 <- ipf_wide_00103[, c(10,33,35:37)]    
#dim(ipf_wide_103); names(ipf_wide_103)
#dim(ipf_wide_103_105); names(ipf_wide_103_105)

#pull out 00103 from the puma data 
   puma00103 <- puma[puma$puma=="00103", ] 
#dim(puma00103); names(puma00103); View(puma00103)
    syn00103 <- cbind(tract=NA, puma00103[1,])
#get the number of tracts within puma 00101   
   no_tracts <- length(unique(ipf_wide_103))-3
#check if the puma has a household in each cell and record the missing cells
missing_cell <- which(apply(ipf_wide_103[1:no_tracts],1,sum)==0)
       cells <- if(length(missing_cell)==0) c(1:45) else c(1:45)[-c(missing_cell)]

#synthetic population for census tracts in puma 00102               
sum(ipf_wide_103[,c(1:no_tracts)]) #number of households in puma 00103
for(j in 1:no_tracts){
  for(i in 1:length(cells)){
       I <- cells[i]; print(I)
       J <- unname(unlist(ipf_wide_103[I,j])); print(J)
   temp1 <- na.omit(puma00103[puma00103$index==I, ])  
    prob <- temp1$hh_wt/sum(temp1$hh_wt)
   temp2 <- temp1[sample(1:nrow(temp1), size=J, replace=TRUE, prob=prob), ];  
   temp2 <- cbind(tract=rep(names(ipf_wide_103)[j], J), temp2)
syn00103 <- rbind(syn00103, temp2)
rm(I, J, temp1, prob, temp2)
}}

#cleanup
      syn00103 <- syn00103[-1,] 
syn00103$tract <- substring(syn00103$tract, 3, 8) 
dim(syn00103)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_103[,1:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn00103$index))[,2]-test

rm(no_tracts, cells, missing_cell) 

#get the 00103 tracts that are in 00105 
   puma00105 <- puma[puma$puma=="00105", ] 
#dim(puma00105); names(puma00105); View(puma00105)
   syn001035 <- cbind(tract=NA, puma00105[1,])
#get the number of tracts within puma 00101   
   no_tracts <- length(unique(ipf_wide_103_105))-3
#check if the puma has a household in each cell and record the missing cells
missing_cell <- which(apply(ipf_wide_103_105[1:no_tracts],1,sum)==0)
       cells <- if(length(missing_cell)==0) c(1:45) else c(1:45)[-c(missing_cell)]

#synthetic population for census tracts in puma 00102               
sum(ipf_wide_103_105[,c(1:no_tracts)]) #number of households in puma 00102
for(j in 1:no_tracts){
  for(i in 1:length(cells)){
        I <- cells[i]; print(I)
        J <- unname(unlist(ipf_wide_103_105[I,j])); print(J)
    temp1 <- na.omit(puma00105[puma00105$index==I, ])  
     prob <- temp1$hh_wt/sum(temp1$hh_wt)
    temp2 <- temp1[sample(1:nrow(temp1), size=J, replace=TRUE, prob=prob), ];  
    temp2 <- cbind(tract=rep(names(ipf_wide_103_105)[j], J), temp2)
syn001035 <- rbind(syn001035, temp2)
rm(I, J, temp1, prob, temp2)
}}

#cleanup
      syn001035 <- syn001035[-1,] 
syn001035$tract <- substring(syn001035$tract, 3, 8) 
dim(syn001035)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_103_105[,1:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn001035$index))[,2]-test

syn00103 <- data.frame(rbind(syn00103, syn001035[-1,])); dim(syn00103)

rm(no_tracts, cells, missing_cell) 


############################################################################
#00105 - FROM 2010 to 2020 geographies
#include tracts: 27.02, 27.03, 27.04, 28.01, 28.02, 29.00, 30.00, 31.00 from puma 00102
#put ipf_long into a wide format to use in generating the synpop keep only puma 00102

      ipf_long_00105 <- ipf_long[ipf_long$puma=="00105",] 
#dim(ipf_long_00105); names(ipf_long_00105); View(ipf_long_00105); length(unique(ipf_long_00105$tract)) 
#create tract column names
   ipf_long_00105$CT <- paste0("CT", ipf_long_00105$tract) 
                temp <- ipf_long_00105[,c(2,3,4,7)]
      ipf_wide_00105 <- temp %>% pivot_wider(names_from=CT, values_from=ipf); rm(temp)
#create a variable to use as the index
ipf_wide_00105$index <- c(1:45) 
      ipf_wide_00105 <- ipf_wide_00105[, order(names(ipf_wide_00105))]
View(ipf_wide_00105); names(ipf_wide_00105); dim(ipf_wide_00105)

#remove tracts not in 2020 geography
    ipf_wide_105 <- ipf_wide_00105[, -c(1:8)]
ipf_wide_105_102 <- ipf_wide_00105[, c(1:8,33:35)]    
#dim(ipf_wide_105); names(ipf_wide_105)
#dim(ipf_wide_105_102); names(ipf_wide_105_102)

#pull out 00102 from the puma data 
   puma00105 <- puma[puma$puma=="00105", ] 
#dim(puma00105); names(puma00105); View(puma00105)
    syn00105 <- cbind(tract=NA, puma00105[1,])
#get the number of tracts within puma 00101   
   no_tracts <- length(unique(ipf_wide_105))-3
#check if the puma has a household in each cell and record the missing cells
missing_cell <- which(apply(ipf_wide_105[1:no_tracts],1,sum)==0)
       cells <- if(length(missing_cell)==0) c(1:45) else c(1:45)[-c(missing_cell)]

#synthetic population for census tracts in puma 00105               
sum(ipf_wide_105[,c(1:no_tracts)]) #number of households in puma 00105
for(j in 1:no_tracts){
  for(i in 1:length(cells)){
       I <- cells[i]; print(I)
       J <- unname(unlist(ipf_wide_105[I,j])); print(J)
   temp1 <- na.omit(puma00105[puma00105$index==I, ])  
    prob <- temp1$hh_wt/sum(temp1$hh_wt)
   temp2 <- temp1[sample(1:nrow(temp1), size=J, replace=TRUE, prob=prob), ]  
   temp2 <- cbind(tract=rep(names(ipf_wide_105)[j], J), temp2)
syn00105 <- rbind(syn00105, temp2)
rm(I, J, temp1, prob, temp2)
}}

#cleanup
      syn00105 <- syn00105[-1,] 
syn00105$tract <- substring(syn00105$tract, 3, 8) 
dim(syn00105)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_105[,1:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn00105$index))[,2]-test

rm(no_tracts, cells, missing_cell) 

#get the 00105 tracts that are in 00102 
   puma00102 <- puma[puma$puma=="00102", ] 
#dim(puma00102); names(puma00102); View(puma00102)
   syn001052 <- cbind(tract=NA, puma00105[1,])
#get the number of tracts within puma 00102   
   no_tracts <- length(unique(ipf_wide_105_102))-3
#check if the puma has a household in each cell and record the missing cells
missing_cell <- which(apply(ipf_wide_105_102[1:no_tracts],1,sum)==0)
       cells <- if(length(missing_cell)==0) c(1:45) else c(1:45)[-c(missing_cell)]

#synthetic population for census tracts in puma 00102               
sum(ipf_wide_105_102[,c(1:no_tracts)]) #number of households in puma 00102
for(j in 1:no_tracts){
  for(i in 1:length(cells)){
        I <- cells[i]; print(I)
        J <- unname(unlist(ipf_wide_105_102[I,j])); print(J)
    temp1 <- na.omit(puma00102[puma00102$index==I, ])  
     prob <- temp1$hh_wt/sum(temp1$hh_wt)
    temp2 <- temp1[sample(1:nrow(temp1), size=J, replace=TRUE, prob=prob), ];  
    temp2 <- cbind(tract=rep(names(ipf_wide_105_102)[j], J), temp2)
syn001052 <- rbind(syn001052, temp2)
rm(I, J, temp1, prob, temp2)
}}

#cleanup
      syn001052 <- syn001052[-1,] 
syn001052$tract <- substring(syn001052$tract, 3, 8) 
dim(syn001052)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_105_102[,1:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn001052$index))[,2]-test

syn00105 <- data.frame(rbind(syn00105, syn001052[-1,])); dim(syn00105)

rm(no_tracts, cells, missing_cell) 


############################################################################
#Create the new puma 00106 
#most of the tracts come from puma 00105
#include tracts: 1.01 from puma 00101
#include tracts: 73.01, 104.00. and 109.00 and  from puma 00104
#put ipf_long into a wide format to use in generating the synpop keep only puma 00102

      ipf_long_00106 <- ipf_long[ipf_long$puma=="00106",] 
#dim(ipf_long_00106); names(ipf_long_00106); View(ipf_long_00106); length(unique(ipf_long_00106$tract)) 
#create tract column names
   ipf_long_00106$CT <- paste0("CT", ipf_long_00106$tract) 
                temp <- ipf_long_00106[,c(2,3,4,7)]
      ipf_wide_00106 <- temp %>% pivot_wider(names_from=CT, values_from=ipf); rm(temp)
#create a variable to use as the index
ipf_wide_00106$index <- c(1:45) 
      ipf_wide_00106 <- ipf_wide_00106[, order(names(ipf_wide_00106))]
View(ipf_wide_00106); names(ipf_wide_00106); dim(ipf_wide_00106)

#remove tracts not in 2020 geography
    ipf_wide_106 <- ipf_wide_00106[, -c(1,23,27,33)]
ipf_wide_106_101 <- ipf_wide_00106[, c(1,36:38)]  
ipf_wide_106_104 <- ipf_wide_00106[, c(23,27,33,36:38)]    

#dim(ipf_wide_106); names(ipf_wide_106)
#dim(ipf_wide_106_101); names(ipf_wide_106_101)
#dim(ipf_wide_106_104); names(ipf_wide_106_104)

#pull out 00105 from the puma data 
   puma00105 <- puma[puma$puma=="00105", ] 
#dim(puma00105); names(puma00105); View(puma00105)
    syn00106 <- cbind(tract=NA, puma00105[1,])
#get the number of tracts within puma 00101   
   no_tracts <- length(unique(ipf_wide_106))-3
#check if the puma has a household in each cell and record the missing cells
missing_cell <- which(apply(ipf_wide_106[1:no_tracts],1,sum)==0)
       cells <- if(length(missing_cell)==0) c(1:45) else c(1:45)[-c(missing_cell)]

#synthetic population for census tracts in puma 00105               
sum(ipf_wide_106[,c(1:no_tracts)]) #number of households from puma 00105
for(j in 1:no_tracts){
  for(i in 1:length(cells)){
       I <- cells[i]; print(I)
       J <- unname(unlist(ipf_wide_106[I,j])); print(J)
   temp1 <- na.omit(puma00105[puma00105$index==I, ])  
    prob <- temp1$hh_wt/sum(temp1$hh_wt)
   temp2 <- temp1[sample(1:nrow(temp1), size=J, replace=TRUE, prob=prob), ]  
   temp2 <- cbind(tract=rep(names(ipf_wide_106)[j], J), temp2)
syn00106 <- rbind(syn00106, temp2)
rm(I, J, temp1, prob, temp2)
}}

#cleanup
      syn00106 <- syn00106[-1,] 
syn00106$tract <- substring(syn00106$tract, 3, 8) 
dim(syn00106)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_106[,1:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn00106$index))[,2]-test

rm(no_tracts, cells, missing_cell) 


#get the 00106 tracts that are in 00101 
   puma00101 <- puma[puma$puma=="00101", ] 
#dim(puma00101); names(puma00101); View(puma00101)
   syn001061 <- cbind(tract=NA, puma00101[1,])
#get the number of tracts within puma 00102   
   no_tracts <- length(unique(ipf_wide_106_101))-3
#check if the puma has a household in each cell and record the missing cells
missing_cell <- which(apply(ipf_wide_106_101[1:no_tracts],1,sum)==0)
       cells <- if(length(missing_cell)==0) c(1:45) else c(1:45)[-c(missing_cell)]

#synthetic population for census tracts in puma 00102               
sum(ipf_wide_106_101[,c(1:no_tracts)]) #number of households in puma 00102
for(j in 1:no_tracts){
  for(i in 1:length(cells)){
        I <- cells[i]; print(I)
        J <- unname(unlist(ipf_wide_106_101[I,j])); print(J)
    temp1 <- na.omit(puma00101[puma00101$index==I, ])  
     prob <- temp1$hh_wt/sum(temp1$hh_wt)
    temp2 <- temp1[sample(1:nrow(temp1), size=J, replace=TRUE, prob=prob), ];  
    temp2 <- cbind(tract=rep(names(ipf_wide_106_101)[j], J), temp2)
syn001061 <- rbind(syn001061, temp2)
rm(I, J, temp1, prob, temp2)
}}

#cleanup
      syn001061 <- syn001061[-1,] 
syn001061$tract <- substring(syn001061$tract, 3, 8) 
dim(syn001061)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_106_101[,1:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn001061$index))[,2]-test

rm(no_tracts, cells, missing_cell) 

#get the 00106 tracts that are in 00104 
   puma00104 <- puma[puma$puma=="00104", ] 
#dim(puma00104); names(puma00104); View(puma00104)
   syn001064 <- cbind(tract=NA, puma00104[1,])
#get the number of tracts within puma 00102   
   no_tracts <- length(unique(ipf_wide_106_104))-3
#check if the puma has a household in each cell and record the missing cells
missing_cell <- which(apply(ipf_wide_106_104[1:no_tracts],1,sum)==0)
       cells <- if(length(missing_cell)==0) c(1:45) else c(1:45)[-c(missing_cell)]

#synthetic population for census tracts in puma 00102               
sum(ipf_wide_106_104[,c(1:no_tracts)]) #number of households in puma 00102
for(j in 1:no_tracts){
  for(i in 1:length(cells)){
        I <- cells[i]; print(I)
        J <- unname(unlist(ipf_wide_106_104[I,j])); print(J)
    temp1 <- na.omit(puma00104[puma00104$index==I, ])  
     prob <- temp1$hh_wt/sum(temp1$hh_wt)
    temp2 <- temp1[sample(1:nrow(temp1), size=J, replace=TRUE, prob=prob), ];  
    temp2 <- cbind(tract=rep(names(ipf_wide_106_104)[j], J), temp2)
syn001064 <- rbind(syn001064, temp2)
rm(I, J, temp1, prob, temp2)
}}

#cleanup
      syn001064 <- syn001064[-1,] 
syn001064$tract <- substring(syn001064$tract, 3, 8) 
dim(syn001064)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_106_104[,1:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn001064$index))[,2]-test

rm(no_tracts, cells, missing_cell) 


syn00106 <- data.frame(rbind(syn00106, syn001061[-1,], syn001064[-1,]))
dim(syn00106)





############################################################################
#combine the data from the six pumas
         dc.syn <- data.frame(rbind(syn00101,syn00102,syn00103,syn00104,syn00105,syn00106))
dc.syn$serialno <- as.factor(dc.syn$serialno); summary(dc.syn$serialno)
 dc.syn$hh_comb <- as.factor(dc.syn$hh_comb); summary(dc.syn$hh_comb)
dim(dc.syn); summary(dc.syn)

#write.csv(dc.syn, "~/Documents/Documents/Food Insecurity/Data/COL_Fairfax/FINAL/dc_pums_synpop_2021.csv")


