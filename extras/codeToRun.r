library(SevEchoLctm)
library(rmarkdown)
library(dplyr)

#### analysis
# sample<-DataAccess(csvFilePath = "./data/Preprocessed_sex_birth_death_uft8.csv")
# 
# sample<-PreProcessing(sample)
# 
# sample<-Sampling(sample)

#sample<-RandomSampling(sample)

#### Raw data was modified. Run codes below. ####

as  <-  read.csv("./data/AS_rawdata_patients_1223.csv", stringsAsFactors = F)
as_obs <- read.csv("./data/AS_rawdata_echo_1223.csv", stringsAsFactors = F)
 ## as: 311/165 ; asobs_1308/138

as<-subset(as,select = -c(group2, group3, group4))
as_obs<-subset(as_obs,select = -c(group2, group3, group4))

sample<-as_obs


#### end ####

# LCTMLinear(seed_num=100,sample)

LCTMLinearAddCV(seed_num=100,sample)

# LCTMQuadratic(seed_num=100,sample)

# LCTMCubic(seed_num=100,sample)

## plotting model 2 ~ 4 only
LCTMPlot(sample,fileName="LCTM_plot_model") # save plots in 'plot' folder
