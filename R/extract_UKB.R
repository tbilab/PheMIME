#================================binary data table=============================#
# extract UKBB data
cat = prepare_get_category() # Get all Category and CategoryID
field = prepare_get_field_ids() # Get all Field and FieldID
## id
## Baseline characteristics (date of birth): field 33
## icd10 diagnosis table: field 41270
## icd10 diagnosis table first date: field 41262

# load_ukbdata will create variable ukb_data_list and ukb_dic in the environment.
##category 2002
load_ukbdata(c("Summary Diagnoses"))
##diagnosis- ICD10
##this field is a summary of the distinct diagnosis codes a participant has had recorded across all the hospital inpatient records in either the primary or secondary position
icd10_diagnosis = get_field_data(c("41270"))$`Summary Diagnoses`
##the corresponding date each diagnosis was first recorded across all their episodes in hospital
##the date given is the episode start date (EPISTART) or if this was missing the admission date
icd10_diagnosis_fdate = get_field_data(c("41280"))$`Summary Diagnoses`
##baseline characteristics table
load_ukbdata(c("Baseline characteristics"))
# mob = get_field_data(keyorid=c("52"))$`Baseline characteristics`
# yob = get_field_data(c("34"))$`Baseline characteristics`
demographics = ukb_data_list$`Baseline characteristics`
##ethnicity
load_ukbdata(c("Ethnicity"))
ethnicity = ukb_data_list$Ethnicity %>%
    dplyr::rename(ethnicity_cat = `21000-0.0`) %>%
    mutate(ethnicity_cat =case_when(ethnicity_cat == -3 ~ "unknown",
                               ethnicity_cat == -1 ~ "unknown",
                               ethnicity_cat == 1 ~ "White",
                               ethnicity_cat == 2 ~ "Mixed",
                               ethnicity_cat == 3 ~ "Asian",
                               ethnicity_cat == 4 ~ "Black",
                               ethnicity_cat == 5 ~ "Chinese",
                               ethnicity_cat == 6 ~ "Other",
                               ethnicity_cat == 1001 ~ "British",
                               ethnicity_cat == 1002 ~ "Irish",
                               ethnicity_cat == 1003 ~ "any other White",
                               ethnicity_cat == 2001 ~ "White and Black Caribbean",
                               ethnicity_cat == 2002 ~ "White and Black African",
                               ethnicity_cat == 2003 ~ "White and Asian",
                               ethnicity_cat == 2004 ~ "Any other mixed background",
                               ethnicity_cat == 3001 ~ "Indian",
                               ethnicity_cat == 3002 ~ "Pakistani",
                               ethnicity_cat == 3003 ~ "Bangladeshi",
                               ethnicity_cat == 3004 ~ "Any other Asian background",
                               ethnicity_cat == 4001 ~ "Caribbean",
                               ethnicity_cat == 4002 ~ "African",
                               ethnicity_cat == 4003 ~ "Any other Black background",
                               TRUE ~ as.character(NA))) %>%
                               dplyr::select(eid,ethnicity_cat)


#=================================convert ICD to phecode table====================#
##convert UKBB ICD10 to ICD10 CM
coding_dat = read_delim(here::here("data/coding19.tsv"),delim = "\t") %>%
  tidyr::separate(., meaning,into = c("ICD10","ICD10 meaning"),sep = "\\s",
                       extra = "merge")

icd10_diagnosis = bind_cols(eid = icd10_diagnosis$eid,purrr::map_dfc(2:ncol(icd10_diagnosis), function(i) {

  icd10_diagnosis %>%
    left_join(.,coding_dat %>%
                dplyr::select(coding,ICD10) %>%
                rename(!!colnames(icd10_diagnosis)[i]:=coding), by=colnames(icd10_diagnosis)[i]) %>%
    dplyr::select(-!!colnames(icd10_diagnosis[i])) %>%
    rename(!!colnames(icd10_diagnosis)[i]:=ICD10) %>%
    dplyr::select(!!colnames(icd10_diagnosis)[i])

}))

##map ICD10-CM to Phecode
phecode_icd10_map = read_delim(here::here("data/Phecode_map_v1_2_icd10cm_beta.csv"),delim = ",")
phecode_diagnosis = bind_cols(eid = icd10_diagnosis$eid,purrr::map_dfc(2:ncol(icd10_diagnosis), function(i) {

  test=icd10_diagnosis %>%
    left_join(.,phecode_icd10_map %>%
                dplyr::select(icd10cm,phecode) %>%
                distinct(icd10cm,.keep_all=TRUE) %>%
                rename(!!colnames(icd10_diagnosis)[i]:=icd10cm), by=colnames(icd10_diagnosis)[i]) %>%
    dplyr::select(-!!colnames(icd10_diagnosis[i])) %>%
    rename(!!colnames(icd10_diagnosis)[i]:=phecode) %>%
    dplyr::select(!!colnames(icd10_diagnosis)[i])

}))

##phecode wide
phecode_diagnosis_long = phecode_diagnosis %>%
    pivot_longer(!eid, names_to = "UKBB_cat", values_to="phecode") %>%
    dplyr::select(-UKBB_cat) %>%
    filter(!is.na(phecode)) %>%
    mutate(phecode=normalize_phecodes(phecode)) %>%
    mutate(freq=TRUE)

phecode_diagnosis_wide = phecode_diagnosis_long %>%
    distinct(eid,phecode,.keep_all=TRUE) %>%
    spread(phecode,freq) %>%
    data.frame()
phecode_diagnosis_wide[is.na(phecode_diagnosis_wide)] = FALSE
rownames(phecode_diagnosis_wide) = phecode_diagnosis_wide$eid
phecode_diagnosis_wide = phecode_diagnosis_wide[,-1]
colnames(phecode_diagnosis_wide) = substring(colnames(phecode_diagnosis_wide),2)
phecode_diagnosis_wide = as(as.matrix(phecode_diagnosis_wide),"sparseMatrix")

##icd10 wide
icd10_diagnosis_long = icd10_diagnosis %>%
    pivot_longer(!eid, names_to = "UKBB_cat", values_to="icd10") %>%
    dplyr::select(-UKBB_cat) %>%
    filter(!is.na(icd10)) %>%
    mutate(freq=TRUE)

icd10_diagnosis_long = icd10_diagnosis_long[icd10_diagnosis_long$eid %in% rownames(phecode_diagnosis_wide),]

icd10_diagnosis_wide = icd10_diagnosis_long %>%
    dplyr::distinct(eid,icd10,.keep_all=TRUE) %>%
    tidyr::spread(icd10,freq) %>%
    data.frame()
icd10_diagnosis_wide[is.na(icd10_diagnosis_wide)] = FALSE
rownames(icd10_diagnosis_wide) = icd10_diagnosis_wide$eid
icd10_diagnosis_wide = icd10_diagnosis_wide[,-1]
colnames(icd10_diagnosis_wide) = substring(colnames(icd10_diagnosis_wide),2)
icd10_diagnosis_wide = as(as.matrix(icd10_diagnosis_wide),"sparseMatrix")

##icd10 date long
icd10_diagnosis_fdate_long = icd10_diagnosis_fdate %>%
    pivot_longer(!eid, names_to = "UKBB_cat", values_to="date") %>%
    dplyr::select(-UKBB_cat) %>%
    filter(!is.na(date))

icd10_diagnosis_fdate_long = icd10_diagnosis_fdate_long[icd10_diagnosis_fdate_long$eid %in% rownames(phecode_diagnosis_wide),]

icd10_diagnosis_fdate_long_maxdate = icd10_diagnosis_fdate_long %>%
    group_by(eid) %>%
    arrange(date) %>%
    slice(n()) %>%
    ungroup()

aws.s3::s3saveRDS(x=icd10_diagnosis_fdate_long,bucket="ukb.tbilab",
object = "phenome/UKBB_ICD10CM_first_date_long.rds",multipart = TRUE)
aws.s3::s3saveRDS(x=phecode_diagnosis_long,bucket="ukb.tbilab",
object = "phenome/UKBB_ICD10CMtoPhecode_long_08122022.rds",multipart = TRUE)
aws.s3::s3saveRDS(x=icd10_diagnosis_long,bucket="ukb.tbilab",object = "phenome/UKBB_ICD10CM_long_08122022.rds",multipart = TRUE)

#================================prepare data for regression=============================#
##phenotype files
phecode_diagnosis_long = aws.s3::s3readRDS(object="phenome/UKBB_ICD10CMtoPhecode_long_08122022.rds",
bucket = "ukb.tbilab")
phecode_diagnosis_wide = aws.s3::s3readRDS(object="phenome/UKBB_ICD10CMtoPhecode_wide_08122022.rds",
bucket = "ukb.tbilab")
##nested phenotype file: each row of phecode with index of patients who have this phecode
phecode_nested = phecode_diagnosis_long %>%
            dplyr::select(-freq) %>%
            arrange(eid) %>%
            group_by(eid) %>%
            mutate(int_id = cur_group_id()) %>%
            ungroup() %>%
            dplyr::select(-eid) %>%
            group_by(phecode) %>%
            nest()
aws.s3::s3saveRDS(x=phecode_nested,bucket="ukb.tbilab",object = "phenome/UKBB_phecode_nested.rds",multipart = TRUE)

##cov file
### EHRAge
### SEX
### Age
### RACE (ethinicity: 21000)
### log phenome burden

icd10_diagnosis_long = aws.s3::s3readRDS(bucket="ukb.tbilab",
object = "phenome/UKBB_ICD10CM_long_08122022.rds",multipart = TRUE)

icd10_diagnosis_long_log_B = icd10_diagnosis_long %>%
                                dplyr::select(-freq) %>%
                                group_by(eid) %>%
                                count(eid) %>%
                                dplyr::rename(B=n) %>%
                                mutate(log_B=log(B))

demographics = ukb_data_list$`Baseline characteristics`
demographics = demographics[demographics$eid %in% rownames(phecode_diagnosis_wide),]
demographics = demographics %>%
                 dplyr::rename(sex=`31-0.0`,yob=`34-0.0`,mob=`52-0.0`,index=`189-0.0`,init_age=`21022-0.0`) %>%
                 mutate(mob = ifelse(floor(log10(mob))+1==1,paste0(0,mob),mob)) %>%
                 mutate(dob=as.Date(paste0(yob,"-",mob,"-01"))) %>%
                 mutate(age = round(as.numeric(difftime(Sys.Date(),dob,units="days"))/365.25,2)) %>%
                 mutate(sex=ifelse(sex==0,"F","M")) %>%
                 left_join(.,icd10_diagnosis_fdate_long_maxdate,by="eid") %>%
                 mutate(ehr_age = round(as.numeric(difftime(date,dob,units="days"))/365.25,2)) %>%
                 left_join(.,ethnicity,by="eid") %>%
                 dplyr::select(-date) %>%
                 left_join(.,icd10_diagnosis_long_log_B,by="eid")
aws.s3::s3saveRDS(x=demographics,bucket="ukb.tbilab",object = "phenome/UKBB_id_to_demographics.rds",multipart = TRUE)
# demographics = aws.s3::s3readRDS(bucket="ukb.tbilab",object = "phenome/UKBB_id_to_demographics.rds")
# demographics = demographics %>% arrange(eid)





#================================longitudinal data=======================================#
##i need a diagnosis table with repeated diagnosis records for each patient (not necessily need to know the date)




#=================================prepare data for network (relative risk) approach============================#








