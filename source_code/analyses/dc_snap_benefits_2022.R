#this program calculates the monthly amount of the household snap benefit
#based on the A Quick Guide to SNAP Eligibility and Benefits (March 2023)
#https://www.cbpp.org/sites/default/files/11-18-08fa.pdf
#Center for Budget Priorities and Policies

syn <- read_csv(archive_read("https://github.com/uva-bi-sdad/food_insecurity/raw/main/documents/products/derived_variables/dc_hlb_2021.csv.zip"))     
syn <- syn[,-1]; dim(syn); str(syn) 
dim(syn) #310100x28

#change the variables hh_comb and serialno to factors
#insert the leading 0 in the hh_comb with only 5-digits
                                      syn$hh_comb <- as.factor(syn$hh_comb)
levels(syn$hh_comb)[levels(syn$hh_comb)=="10000"] <- "010000"
levels(syn$hh_comb)[levels(syn$hh_comb)=="11000"] <- "011000"
levels(syn$hh_comb)[levels(syn$hh_comb)=="20000"] <- "020000"
View(data.frame(table(syn$hh_comb, useNA="always"))) 
syn$serialno <- as.factor(syn$serialno); View(data.frame(table(syn$serialno, useNA="always"))) 
   syn$tract <- as.factor(syn$tract); View(data.frame(table(syn$tract, useNA="always"))) 

#calculate the maximum snap benefit  
                  snap <- syn[, c(1:3,5,23,15,17:19,27,28)]
snap$hh_monthly_income <- syn$hh_income/12

#SNAP eligibility: Gross Monthly Income Limit (130% of Federal Poverty Level) 
#https://fns-prod.azureedge.us/sites/default/files/media/file/FY22-Income-Eligibility-Standards%20reviewed.pdf           
snap$snap_fpl_gross <- ifelse(snap$hh_size==1, 1396, 
                       ifelse(snap$hh_size==2, 1888,
                       ifelse(snap$hh_size==3, 2379,
                       ifelse(snap$hh_size==4, 2871,
                       ifelse(snap$hh_size==5, 3363,
                       ifelse(snap$hh_size==6, 3855,
                       ifelse(snap$hh_size==7, 4347,
                       ifelse(snap$hh_size==8, 4839, 4839+(snap$hh_size-8)*492))))))))
#SNAP eligibility: Net Monthly Income Limit (100% of Federal Poverty Level) 
#https://fns-prod.azureedge.us/sites/default/files/media/file/FY22-Income-Eligibility-Standards%20reviewed.pdf           
  snap$snap_fpl_net <- ifelse(snap$hh_size==1, 1074, 
                       ifelse(snap$hh_size==2, 1452,
                       ifelse(snap$hh_size==3, 1830,
                       ifelse(snap$hh_size==4, 2209,
                       ifelse(snap$hh_size==5, 2587,
                       ifelse(snap$hh_size==6, 2965,
                       ifelse(snap$hh_size==7, 3344,
                       ifelse(snap$hh_size==8, 3722, 3722+(snap$hh_size-8)*379))))))))

#maximum SNAP benefits
#https://fns-prod.azureedge.us/sites/default/files/media/file/FY22-Maximum-Allotments-Deductions.pdf  
snap$snap_max_benefit <- ifelse(snap$hh_size==1, 250, 
                         ifelse(snap$hh_size==2, 459,
                         ifelse(snap$hh_size==3, 658,
                         ifelse(snap$hh_size==4, 835,
                         ifelse(snap$hh_size==5, 992,
                         ifelse(snap$hh_size==6, 1190,
                         ifelse(snap$hh_size==7, 1316,
                         ifelse(snap$hh_size==8, 1504, 1504+(snap$hh_size-8)*188))))))))


#calculate the snap countable income
#https://fns-prod.azureedge.us/sites/default/files/media/file/FY22-Maximum-Allotments-Deductions.pdf
#1. calculate the standard deduction
     snap$std_deduc <- ifelse(snap$hh_size<=3, 177, 
                       ifelse(snap$hh_size==4, 184, 
                       ifelse(snap$hh_size==5, 215, 246))) 
#2. calculate the childcare deduction           
   snap$ccare_deduc <- round(snap$childcare_cost_month, 2)  
#3. calculate the earnings deduction 20% of gross monthly income 
  snap$income_deduc <- snap$hh_monthly_income*0.20  
#calculate the initial countable income
       snap$snap_ci <- snap$hh_monthly_income-snap$std_deduc-
                       snap$ccare_deduc-snap$income_deduc
       
#calculate snap housing metric & shelter deduction: housing cost minus 50% of the countable income          
snap$snap_housing_metric <- snap$housing_cost_month-(snap$snap_ci*0.50) 
   snap$snap_shelt_deduc <- ifelse(snap$snap_housing_metric>597, 597, 
                            ifelse(snap$snap_housing_metric<0, 0, snap$snap_housing_metric)) 

#calculate the net income: countable income minus shelter deduction
                snap$snap_ni <- snap$snap_ci-snap$snap_shelt_deduc  
snap$snap_ni[snap$snap_ni<0] <- 0
#calculate the household's expected contribution toward food 30% of net income
          snap$snap_ectf <- snap$snap_ni*0.30
#calculate the amount of the household SNAP benefit          
   snap$snap_food_metric <- snap$snap_max_benefit-snap$snap_ectf
     
       snap$snap_benefit <- round(ifelse(snap_food_metric<0, 0, snap$snap_food_metric), 2)
dim(snap); View(snap)

#write.csv(snap, "~/Documents/Documents/Food Insecurity/Data/COL_Fairfax/FINAL/dc_snap_benefits_2022.csv")

            