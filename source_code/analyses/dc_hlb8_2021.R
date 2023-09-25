#this program calculates the HLB components for taxes and healthcare 
#for every household in the synthetic population
options(scipen=999)
library(archive)
library(readr)
library(dplyr)
library(knitr)
 

#read in hlb6
syn <- read.csv("https://github.com/uva-bi-sdad/food_insecurity/raw/main/documents/products/derived_variables/dc_hlb6_2021.csv")
names(syn); syn <- syn[,-1]
syn <- syn[with(syn, order(puma, tract, hh_comb, serialno)), ]
dim(syn);  str(syn)
syn$hh_comb <- as.factor(syn$hh_comb); View(data.frame(table(syn$hh_comb, useNA="always")))
#insert the leading 0 in the hh_comb with only 5-digits
  levels(syn$hh_comb)[levels(syn$hh_comb)=="10000"] <- "010000"
  levels(syn$hh_comb)[levels(syn$hh_comb)=="11000"] <- "011000"
  levels(syn$hh_comb)[levels(syn$hh_comb)=="20000"] <- "020000"
  View(data.frame(table(syn$hh_comb, useNA="always"))) 
syn$serialno <- as.factor(syn$serialno); View(data.frame(table(syn$serialno, useNA="always"))) 


#calculate the healthcare component threshold for the HLB based on NASEM (2023)
#assume healthcare insurance is through an employer
#employer pays 76% of premium (National Compensation Survey (2022) northeast mid-Atlantic) 
#second-lowest-cost Silver Plan from DC Health Benefit Exchange
#https://disb.dc.gov/sites/default/files/dc/sites/disb/page_content/attachments/2022%20QHP%20Rate%20Submission%20Data%20-%20Individual.pdf
#assume all adults are 40
 adult <- 337.00
#infant, toddler, preschool, schoolers 0-11 
 child <- 201.63
#teenager 12-18
  teen <- 240.75 
#MOOP_individual
   ind <- 4000.00 
#MOOP_family
family <- 8000.00 

syn$hh_size <- (syn$no_adult + syn$no_infant + syn$no_toddler +
                syn$no_schooler + syn$no_schooler + syn$no_teenager) 
    premium <- (syn$no_adult*adult + 
                (syn$no_infant + syn$no_toddler + syn$no_schooler)*child +
                (syn$no_schooler + syn$no_teenager)*teen) 
       MOOP <- ifelse(syn$hh_size==1, ind, family)

#total monthly healthcare cost
syn$healthcare_cost_month <- premium*0.24 + MOOP/12
summary(syn$healthcare_cost_month, include.na=TRUE)

temp <- syn[,c(2,1,5,3,16)]; dim(temp); names(temp)
temp <- temp[with(temp, order(puma, tract, hh_comb, serialno)), ]; str(temp)

#calculate the tax component threshold for the HLB using TAXSIM
#https://taxsim.nber.org/taxsim35/  
t
#create the HLB budget for each household in the synthetic population
syn$hlb_no_tax_yr <- (syn$healthcare_cost_month+syn$broadband_cost_month+
                      syn$food_cost_month+syn$childcare_cost_month+
                      syn$housing_cost_month+syn$transp_cost_month+
                      syn$other_costs_month)*12

#need to create a marriage status vector
#assume if there are two adults in the household they are married single otherwise
mstat <- ifelse(syn$no_adult==2, "married, jointly", "single")

#create a column for the age of the spouse and three columns for the age of the youngest three children
#assume the head of the household is 40
no_children <- c(syn$no_teenager+syn$no_schooler+syn$no_preschooler+
                 syn$no_toddler+syn$no_infant)
      index <- which(no_children!=0)
        AGE <- syn[, c(7:11)]
            AGE$no_teenager[AGE$no_teenager>=1] <- 13 
            AGE$no_schooler[AGE$no_schooler>=1] <- 7 
      AGE$no_preschooler[AGE$no_preschooler>=1] <- 4 
              AGE$no_toddler[AGE$no_toddler>=1] <- 2 
                AGE$no_infant[AGE$no_infant>=1] <- 1 
                                    AGE[AGE==0] <- 500       
 
       age <- data.frame(matrix(0, nrow=dim(AGE)[1], ncol = 3))
 for(i in 1:length(index)){
   age[index[i],] <- sort(unname(unlist(AGE[index[i],])), decreasing=FALSE, na.last=NA)
}   
 
age[age==500] <- 0
summary(age[,1], includeNA=TRUE) 
summary(age[,2], includeNA=TRUE)  
summary(age[,3], includeNA=TRUE) 

#create a unique tax ID for each household

      ID <- c(1:dim(syn)[1]) 
DC_TAXES <- data.frame(taxsimid=ID, 
                       state=rep("VA", dim(syn)[1]),
                       year=rep(2022, dim(syn)[1]), 
                       mstat=mstat,
                       pwages=syn$hlb_no_tax_yr,
                       page=rep(40, dim(syn)[1]), 
                       age1=age[,1],
                       age2=age[,2],
                       age3=age[,3])
View(DC_TAXES)

                     DC_taxes <- taxsim_calculate_taxes(DC_TAXES)
View(DC_taxes); dim(DC_taxes)                     
              syn$total_taxes <- DC_taxes$fiitax+DC_taxes$siitax+DC_taxes$tfica
                 syn$hlb_year <- round((syn$hlb_no_tax_yr +
                                        syn$total_taxes), 2)
                  syn$eco_vul <- ifelse(syn$hh_income<syn$hlb_year, 1, 0)
dim(syn); str(syn)

#write.csv(syn, "~/Documents/Documents/Food Insecurity/Data/COL_Fairfax/FINAL/dc_hlb_2021.csv")



#code used to test the TAXSIM software
family_income <- data.frame(
  taxsimid = c(1, 2, 3),
     state = c("DC", "MD", "VA"),
      year = c(2022, 2022, 2022),
     mstat = c("single", "single", "single"),
    pwages = c(10000, 10000, 10000), # primary wages
      page = c(0, 0, 0)) # primary age

family_taxes <- taxsim_calculate_taxes(COL1)

 

