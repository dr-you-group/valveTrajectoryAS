# 1. RHD 환자 제외.
# - 에코 코멘트 (initial 및 f/u 모두) 에서 “rhd” “RHD” “Rheumatic” “Rheumatic heart disease” 등 포함될 경우 제외.
# 

as_obs$avRegurgitation ## AR
as_obs$mvRegurgitation ## MR
as_obs$tvRegurgitation ## TR
as_obs$mvStenosis  ### MS

as_obs_new <- as_obs

rhd_list <- as_obs %>% 
  filter(grepl("rhd", commentForFixedLines, ignore.case = TRUE) |
             grepl("rheumatic", commentForFixedLines, ignore.case = TRUE)) %>%
  distinct(ptno)

as_obs_new <- as_obs_new[!(as_obs_new$ptno %in% rhd_list$ptno),]

as_obs_new %>%
  distinct(ptno) %>%
  nrow()

## 1729 obs / 395 명

# 2. 첫 에코에서 다른 판막 질환 있는 경우 제외. 
# - MS: moderate 이상 제외 (grade: GII)
# - AR or MR or TR: severe 이상 제외 (grade: GIII)

########################## AR

as_obs_new <- as_obs_new %>% mutate(avRegur = case_when(avRegurgitation %in% c("No","-","") ~ "no",
                                            avRegurgitation %in% c("Trivial") ~ "trivial",
                                            avRegurgitation %in% c("G I") ~ "G I",
                                            avRegurgitation %in% c("G I-II","G I~II") ~ "G I-II",
                                            avRegurgitation %in% c("G II") ~ "G II",
                                            avRegurgitation %in% c("G II-III","G II~III") ~ "G II-III",
                                            avRegurgitation %in% c("G III") ~ "G III",
                                            avRegurgitation %in% c("G III-IV") ~ "G III-IV",
                                            avRegurgitation %in% c("G IV") ~ "G IV"
                                            #TRUE ~ NA
)
)

as_obs_new$avRegur <- factor(as_obs_new$avRegur, levels = c("no", "trivial", "G I", "G I-II", "G II", "G II-III", "G III", "G III-IV", "G IV"))

as_obs_new$avRegurNum <- as.numeric(as_obs_new$avRegur)

########################## MR

as_obs_new <- as_obs_new %>% mutate(mvRegur = case_when(mvRegurgitation %in% c("No","-","") ~ "no",
                                                        mvRegurgitation %in% c("Trivial") ~ "trivial",
                                                        mvRegurgitation %in% c("G I") ~ "G I",
                                                        mvRegurgitation %in% c("G I-II","G I~II") ~ "G I-II",
                                                        mvRegurgitation %in% c("G II") ~ "G II",
                                                        mvRegurgitation %in% c("G II-III","G II~III") ~ "G II-III",
                                                        mvRegurgitation %in% c("G III") ~ "G III",
                                                        mvRegurgitation %in% c("G III-IV") ~ "G III-IV",
                                                        mvRegurgitation %in% c("G IV") ~ "G IV"
                                                        #TRUE ~ NA
)
)

as_obs_new$mvRegur <- factor(as_obs_new$mvRegur, levels = c("no", "trivial", "G I", "G I-II", "G II", "G II-III", "G III", "G III-IV", "G IV"))

as_obs_new$mvRegurNum <- as.numeric(as_obs_new$mvRegur)

########################## TR

as_obs_new <- as_obs_new %>% mutate(tvRegur = case_when(tvRegurgitation %in% c("No","-","") ~ "no",
                                            tvRegurgitation %in% c("Trivial") ~ "trivial",
                                            tvRegurgitation %in% c("G I","GI", "I", "l") ~ "G I",
                                            tvRegurgitation %in% c("CI-II","G I-II","G I- II","G I_II","G I~II","G I-ll","GI- II","GI-II", "I-II", "G I-OO") ~ "G I-II",
                                            tvRegurgitation %in% c("G II", "GII", "II", "G II-II") ~ "G II",
                                            tvRegurgitation %in% c("G II-III","G II~III","G II->III", "GII-III", "II-III") ~ "G II-III",
                                            tvRegurgitation %in% c("G III","GIII","G III\\") ~ "G III",
                                            tvRegurgitation %in% c("G III-IV","GIII-IV", "III-IV") ~ "G III-IV",
                                            tvRegurgitation %in% c("G IV", "IV", "G IIII") ~ "G IV"
                                            #TRUE ~ NA
)
)


as_obs_new$tvRegur <- factor(as_obs_new$tvRegur, levels = c("no", "trivial", "G I", "G I-II", "G II", "G II-III", "G III", "G III-IV", "G IV"))

as_obs_new$tvRegurNum <- as.numeric(as_obs_new$tvRegur)

########################## MS

as_obs_new <- as_obs_new %>% mutate(mvSte = case_when(grepl("[Nn]o",mvStenosis) ~ "no",
                                          grepl("[Mm]ild$",mvStenosis) ~ "mild",
                                          grepl("[Mm]ild.+[Mm]od*.+",mvStenosis) ~ "mild to moderate",
                                          grepl("^[Mm]oderate$",mvStenosis) ~ "moderate",
                                          grepl("[Mm]od.+[Ss]ev.*",mvStenosis) ~ "moderate to severe",
                                          mvStenosis %in% c("Severe", "severe","R/O Severe","R/O severe","Severe to severe", "R/O severe AS",  "r/o Severe","Very severe") ~ "severe",
                                          mvStenosis %in% c("No","-","--","","") ~ "no",  
                                          is.na(.$mvStenosis) ~ "no",
                                          mvStenosis %in% c("Mlid", "Mild??") ~ "mild",
                                          mvStenosis %in% c("Mild to mdorate", "Mild to oderate") ~ "mild to moderate",
                                          mvStenosis %in% c("Moderate to severe") ~ "moderate to severe",
                                          TRUE ~ "no"
)
)

##Transforming class of AV Stenosis into factor
as_obs_new$mvSte <- factor(as_obs_new$mvSte, levels = c("no", "mild", "mild to moderate", "moderate", "moderate to severe", "severe"))

##add numeric column for AV Stenosis
as_obs_new$mvSteNum <- as.numeric(as_obs_new$mvSte)



# unique(as_obs_new$mvStenosis)

unique(as_obs_new$avRegur)
unique(as_obs_new$avRegurNum)
unique(as_obs_new$mvRegur)
unique(as_obs_new$mvRegurNum)
unique(as_obs_new$tvRegur)
unique(as_obs_new$tvRegurNum)

unique(as_obs_new$mvSte)
unique(as_obs_new$mvSteNum)

# 2. 다른 판막 질환 제외. 첫 에코에서.

# - AR or MR or TR: severe 이상 제외 (grade: GIII) 7
# - MS: moderate 이상 제외 (grade: GII) 4

other_hvd_list<-as_obs_new %>%
  group_by(ptno) %>%
  filter(row_number()==1) %>%
  filter((avRegurNum>=7) | (mvRegurNum>=7) |(tvRegurNum>=7) |(mvSteNum>=3)) %>%
  distinct(ptno)
  
as_obs_new <- as_obs_new[!(as_obs_new$ptno %in% other_hvd_list$ptno),]
  
as_obs_new %>%
  distinct(ptno) %>%
  nrow()

## 1619 obs / 372 명

# 3. Bicuspid 환자 제외

bicuspid_list <- as %>%
  filter(Bicuspid.AV==1) %>%
  distinct(ptno)

as_obs_new <- as_obs_new[!(as_obs_new$ptno %in% bicuspid_list$ptno),]

write.csv(as_obs_new, "./AS_observation_1223.csv")

as_new <- as_obs_new %>%
  group_by(ptno) %>%
  filter(row_number()==1)

write.csv(as_new, "./AS_patients_1223.csv")
