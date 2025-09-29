################################################################################
### REDCap data logic checks + cleaning
################################################################################

# the purpose of this file

# do some logic checks of the REDCap data and provide clean datasets for different metrics
# for Lis/Apolline to use late on
# 
# before running the code: update the CCH redcap file using the getREDCap.R file


library(tidyverse)

# load raw redcap data
redcap = read_csv("/Volumes/FS/_ISPM/CCH/Actual_Project/data/App_Personal_Data_Screening/redcap_all.csv")

## Subset different types of redcap data
#----
# consent
redcap_consent <- redcap |>
  select(uid, starts_with("con")) |>
  filter(if_any(-uid, ~ !is.na(.))) |>
  filter(str_starts(uid, "ACT"))

# screening
redcap_screening <- redcap |>
  select(uid, starts_with("scr")) |>
  filter(if_any(-uid, ~ !is.na(.))) |>
  filter(str_starts(uid, "ACT"))


# baseline
redcap_baseline <- redcap |>
  select(uid, starts_with("bsl"), starts_with("base")) |>
  filter(if_any(c(-uid, -baseline_complete), ~ !is.na(.))) |>
  filter(str_starts(uid, "ACT"))

# daily
redcap_daily <- redcap |>
  select(uid, redcap_event_name, starts_with("dly"), ) |>
  filter(if_any(c(-uid, -redcap_event_name), ~ !is.na(.))) |>
  filter(str_starts(uid, "ACT"))

# participant visit log
redcap_pvl <- redcap |>
  select(uid, redcap_event_name, starts_with("pvl"), starts_with("partic")) |>
  filter(if_any(c(-uid, -redcap_event_name), ~ !is.na(.))) |>
  filter(str_starts(uid, "ACT"))

# work adress log
redcap_work <- redcap |>
  select(uid, redcap_event_name, starts_with("wrk"), starts_with("work")) |>
  filter(if_any(c(-uid, -redcap_event_name), ~ !is.na(.))) |>
  filter(str_starts(uid, "ACT"))

# home adress log
redcap_home <- redcap |>
  select(uid, redcap_event_name, starts_with("hom")) |>
  filter(if_any(c(-uid, -redcap_event_name), ~ !is.na(.))) |>
  filter(str_starts(uid, "ACT"))

# withdrawal
redcap_withdrawal <- redcap |>
  select(uid, redcap_event_name, starts_with("wid")) |>
  filter(if_any(c(-uid, -redcap_event_name), ~ !is.na(.))) |>
  filter(str_starts(uid, "ACT"))


#----


## Manual Cleaning of "smaller" REDCap subsets: BASLINE, CONSENT, SCREENING, HOME, WORK
#----

# consent
redcap_consent_clean <- redcap_consent |>
  
  # filter out withdrawn participants and empty participants
  filter(!(uid %in% unique(redcap_withdrawal$uid) | is.na(con_dob))) |>
  
  mutate(
    # correct wrong village code
    con_villcode = ifelse(con_villcode == 8005, 80050, con_villcode),
    # correct village code and name mismatch
    con_villname = ifelse(con_villcode == 80050, "Kabakama", con_villname),
    con_villname = ifelse(con_villcode == 80014, "Basse", con_villname)    
         )

# screening
redcap_screening_clean <- redcap_screening |>
  
  # filter out withdrawn participants and empty participants
  filter(!(uid %in% unique(redcap_withdrawal$uid) | is.na(scr_vdate))) |>
  
  mutate(
    # correct 18o65 for participant 30
    scr_18to65 = ifelse(uid == "ACT030M", 1, scr_18to65),
    # flag outliers of the heart/weight tests
    scr_hr_outlier = ifelse(scr_hr >= 45 & scr_hr <= 80, NA, 1),
    scr_sbp_outlier = ifelse(scr_sbp >= 120 & scr_sbp <= 140, NA, 1),
    scr_dbp_outlier = ifelse(scr_dbp >= 80 & scr_dbp <= 90, NA, 1),
    scr_height_outlier = ifelse(scr_height >= 135 & scr_height <= 185, NA, 1),
    scr_weight_outlier = ifelse(scr_weight >= 30 & scr_weight <= 100, NA, 1)
    )

# baseline
redcap_baseline_clean <- redcap_baseline |>
  
  # filter out withdrawn participants and empty participants
  filter(!(uid %in% unique(redcap_withdrawal$uid) | is.na(bsl_vdate))) |>
  
  mutate(
    # if a person has wives, he cannot be a a number of wive
    bsl_cowives = ifelse(is.numeric(bsl_wives), NA, bsl_cowives),
    # homogenizing other secondary work activities
    bsl_work2_1 = ifelse(bsl_work2_1 == "Gardener", "Gardening", bsl_work2_1),
    bsl_work2_1 = ifelse(bsl_work2_1 == "Driving taxi", "Driver", bsl_work2_1),
    # homogenizing the specified physical activity of th eprimary job
    bsl_work5 = ifelse(bsl_work5 %in% c("Lifting watering can.", "Walking to Marking and lifting heavy material" , "Lifting heavy material", "Lifting cooking pots" ,"Lifting", "Lifting heavy materials" ,  "Lifting pans", "Lifting heavy metals", "Lifting watering can" , "Lifting and moulding of jar pots", "Lifting heavy loads", "Lifting of watering can" , "Lifting television",  "Lifting watering cans" , "Lifting heavy luggages" , "Lifting watering can,  weeding.",  "Lifting bags"  , "Lifting pans at the market" ),
                       "Lifting heavy things", bsl_work5),
    bsl_work5 = ifelse(bsl_work5 %in% c("Too much talking" , "MrDcc8cOWvlSrnHtqLZLD73J7n3Bj0qHC6FhnxkJeCA3sGvCr775LT2IsR3SW8+N"),
                       NA, bsl_work5),
    # collapse the bsl_house8 columns
    bsl_house8 = max.col(across(starts_with("bsl_house8__")), ties.method = "first"),
    # homogenizing the methods by which they adjust their temperature at home
    bsl_house10_1 = ifelse(bsl_house10_1 == "Stand fan", "Local fan", bsl_house10_1),
    # Set NA, to the question that should specify the type of Malignant Tumor which is answered with headaches
    bsl_mh7_1 = ifelse(bsl_mh7_1 %in% c("Extreme headache", "Severe headache", "Heavy headache"), 
                       NA, bsl_mh7_1),
    # Set NA, to the question that should specify the type of Repiratory disease which is answered with headaches
    bsl_mh8_1 = ifelse(bsl_mh8_1 %in% c("Severe headache"), NA, bsl_mh8_1),
    # Set NA, to the question that should specify when treatement for heat related illnesses happened but which were answered with symptoms
    bsl_mh30_1 = ifelse(bsl_mh30_1 %in% c("Dehydration", "Dehydration and dizziness"), NA, bsl_mh30_1),
    # Set NA, to the question that should specify which non-listed measures they take against heat if it was in the available answeres
    bsl_adapthome_1 = ifelse(bsl_adapthome_1 %in% c("Local fan"), NA, bsl_adapthome_1),
    # homogenizing the sources of other noise
    bsl_na1_5_1 = ifelse(bsl_na1_5_1 %in% c("Shout of children", "Noise of children"), "Children", bsl_na1_5_1),
    bsl_na1_5_1 = ifelse(bsl_na1_5_1 %in% c("When people are shouting"), "Shouting", bsl_na1_5_1),
    bsl_na1_5_1 = ifelse(bsl_na1_5_1 %in% c("Road  music"), "Music", bsl_na1_5_1),
    # set NA, to the other specified measures against noise for which you could have been checked with box previously
    bsl_na4_3_1 = ifelse(bsl_na4_3_1 %in% c("people chatting outside.", "Shouting of the children", "1", "Close doors, windows.",  "Close doors and windows", "Close windors", "Close doors", "Close windows"), 
                         NA, bsl_na4_3_1),
  ) |>
  
  # filter out rows that have only NA in them
  # dplyr::select(where(~ any(!is.na(.)))) |>
  # filter out columns that have been collapsed
  dplyr::select(-starts_with("bsl_house8__"))
  
# work adress log
redcap_work_clean <- redcap_work |>
  
  # filter out withdrawn participants and empty participants
  filter(!(uid %in% unique(redcap_withdrawal$uid) | is.na(wrk_fieldworker1)))

# home adress log
redcap_home_clean <- redcap_home |>
  
  # filter out withdrawn participants and empty participants
  filter(!(uid %in% unique(redcap_withdrawal$uid) | is.na(hom_fieldworker)))


#----


## Logic checks of "bigger" REDCap subsets: DAILY, PVL
#----


# pvl
redcap_pvl_clean <- redcap_pvl |>
  mutate(
    # flag pvl's where the time is nonsensical -> start after end, start and end at the same time is allowed
    time_flag = ifelse(pvl_start > pvl_end, 1, 0)
  ) |>
  group_by(uid, redcap_event_name) |>
  mutate(
    # check if first and last action of actigraph is set up and take down in every week
    pvl_actigraph_setup_flag = ifelse(first(pvl_actigraphact) == 1, 0, 1),
    pvl_actigraph_takedown_flag = ifelse(last(pvl_actigraphact) == 2, 0, 1),
    # check if all actigraph device id are the same in each week
    pvl_actigraph_id_flag = ifelse(n_distinct(pvl_actigraphid) == 1, 0, 1),
    
    # check if first and last action of ibutton taped is set up and take down in every week
    pvl_ibuttaped_setup_flag = ifelse(first(pvl_ibuttapedact) == 1, 0, 1),
    pvl_ibutaped_takedown_flag = ifelse(last(pvl_ibuttapedact) == 2, 0, 1),
    # check if all ibutton taped device id are the same in each week
    pvl_ibutaped_id_flag = ifelse(n_distinct(pvl_ibuttapedid, na.rm = T) == 1, 0, 1),
    
    # check if first and last action of ibutton worn is set up and take down in every week
    pvl_ibutworn_setup_flag = ifelse(first(pvl_ibutwornact) == 1, 0, 1),
    pvl_ibuworn_takedown_flag = ifelse(last(pvl_ibutwornact) == 2, 0, 1),
    # check if all ibutton worn device id are the same in each week
    pvl_ibuworn_id_flag = ifelse(n_distinct(pvl_ibutwornid, na.rm = T) == 1, 0, 1),

    # check if first and last action of ibutton house is set up and take down in every week
    pvl_ibuthouse_setup_flag = ifelse(first(pvl_ibuthouseact) == 1, 0, 1),
    pvl_ibuhouse_takedown_flag = ifelse(last(pvl_ibuthouseact) == 2, 0, 1),
    # check if all ibutton house device id are the same in each week
    pvl_ibuhouse_id_flag = ifelse(n_distinct(pvl_ibuthouseid, na.rm = T) == 1, 0, 1),
    
    # check if first and last action of noisen sentry is set up and take down in every week
    pvl_noisen_setup_flag = ifelse(first(pvl_noisenact) == 1, 0, 1),
    pvl_noisen_takedown_flag = ifelse(last(pvl_noisenact) == 2, 0, 1),
    # check if all noisen device id are the same in each week
    pvl_noisen_id_flag = ifelse(n_distinct(pvl_noisenid, na.rm = T) == 1, 0, 1),
    
    # check if first and last action of sck is set up and take down in every week
    pvl_sck_setup_flag = ifelse(first(pvl_sckact) == 1, 0, 1),
    pvl_sck_takedown_flag = ifelse(last(pvl_sckact) == 2, 0, 1),
    # check if all sck device id are the same in each week
    pvl_sck_id_flag = ifelse(n_distinct(pvl_sckid, na.rm = T) == 1, 0, 1),
  )
  
summary(redcap_pvl_clean[,48:66])





redcap_daily_instance <- redcap_daily |>
  
  # filter out withdrawn participants and empty participants
  filter(!(uid %in% unique(redcap_withdrawal$uid))) |>
  
  # find out the number of instances per observation week and participant
  group_by(uid, redcap_event_name) |>
  filter(is.numeric(dly_visitday)) |>
  summarise(n = n())

hist(redcap_daily_instance$n[redcap_daily_instance$n != 6])

redcap_daily_instance[redcap_daily_instance$n != 6, 1:2]

#----


## Save the merged and cleaned data
#----

# baseline, screening, consent
redcap_con_scr_bsl <- redcap_screening_clean |>
  left_join(redcap_screening_clean, by = "uid") |>
  left_join(redcap_baseline_clean, by = "uid") 

write_csv(redcap_con_scr_bsl, "/Volumes/FS/_ISPM/CCH/Actual_Project/data/REDCap/consent_screening_baseline_clean.csv")  

# pvl
write_csv(redcap_pvl_clean, "/Volumes/FS/_ISPM/CCH/Actual_Project/data/REDCap/pvl_clean.csv")

# daily
# write_csv(redcap_daily_clean, "/Volumes/FS/_ISPM/CCH/Actual_Project/data/REDCap/daily.csv")

#----