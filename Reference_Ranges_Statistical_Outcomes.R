#this script intakes data and generally outputs reference ranges per group
#output excel file 1: Data Entry Component data frame - for inputting the reference ranges into the Study Data Browser using the custom-built data entry component 
#output excel file 2: Comprehensive data frame - include additional information requested by the customers like confidence intervals around the reference ranges and summary statistics for groups where reference range calculation is not appropriate

#load libraries
load.lib = c('dplyr', 'tidyr', 'MASS', 'car', 'referenceIntervals', 'outliers', 'ggplot2', 'purrr', 'DBI', 'odbc', 'openxlsx', 'magrittr')
install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

#establish connection with database and clean data (process to clean is also happening on the backend but use this for now)
connection = dbConnect(odbc::odbc(), "database_server")
data_measurement_dev = dbGetQuery(connection, "SELECT * from [Reference_Range_Baseline]")

#create test set: FOR PROTOTYPE ONLY
#data_measurement_dev = subset(data_measurement_dev, species_strain == 'RHESUS' & sex == 'Male' & age_category == '3.2+', select = analyte & assay)

#clean NAs to standardize with what is in SQL, replace non-standardized primate countries of origin
data_measurement_dev$primate_country_of_origin = data_measurement_dev$primate_country_of_origin %>% 
  replace_na('Not_Applicable')
data_measurement_dev$primate_country_of_origin = case_match(
  data_measurement_dev$primate_country_of_origin,
  'St Kitts' ~ 'St. Kitts',
  'U.S.A.-Morgan Island' ~ 'USA Morgan Island',
  'Indonesian' ~ 'Indonesia',
  'Chinese' ~ 'China',
  'Cambodian' ~ 'Cambodia',
  'Indian' ~ 'India'
)

#drop rows associated with analytes not being analyzed 
analytes_QC = c('HEM', 'ICT', 'LIP')
data_measurement_dev = data_measurement_dev %>% 
  filter(!analyte %in% analytes_QC)

#remove all data points marked 'Yes' in remove_YN column
data_measurement_dev = data_measurement_dev[data_measurement_dev$remove_YN == 'No',]

#further filtering and refining
data_dev = data_measurement_dev %>% 
  group_by(subject_name, project_name, age_category, species_strain, sex, collection_date, timepoint_day_number, specimen_name, analyte, units, primate_country_of_origin) %>% 
  mutate(mean_rsv=mean(result_value_number))
data_dev_2 = data_dev %>% 
  group_by(subject_name, project_name, age_category, species_strain, sex, collection_date, timepoint_day_number, analyte, units, primate_country_of_origin) %>% 
  mutate(percent_diff_rvm = abs((result_value_number - mean_rsv)/result_value_number)*100)
data_high_diffmean = data_dev_2[data_dev_2$percent_diff_rvm >= 5,]
data_dev_clean = anti_join(data_dev_2, data_high_diffmean, by=c('subject_name', 'project_name', 'age_category', 'species_strain', 'sex', 'collection_date', 'timepoint_day_number', 'analyte', 'units', 'primate_country_of_origin'))
cols_to_drop = c('mean_rsv', 'percent_diff_rvm')
data_dev_clean = data_dev_clean[, !(names(data_dev_clean) %in% cols_to_drop)]
data_dev_clean = data_dev_clean %>% 
  mutate(assay = case_when(assay %in% c('CHEM_PICCOLO_CHEM13', 'CHEM_PICCOLO_RFP', 'CHEM_PICCOLO_CMP') ~ 'CHEM_PICCOLO', TRUE ~ assay)) %>% 
  group_by(subject_name, project_name, age_category, species_strain, sex, collection_date, timepoint_day_number, specimen_name, analyte, units, assay, primate_country_of_origin) %>% 
  summarise(result_value_number = mean(result_value_number)) %>% 
  ungroup() 

#combine primate groups into single country of origin for separate statistical analysis and add back to data_dev_clean; remove primate groups where country of origin is Not_Applicable
primate_group = c('CYNO', 'RHESUS', 'AFRICAN_GREEN', 'PATAS')
primate_combined_subset = data_dev_clean[data_dev_clean$species_strain %in% primate_group,]
primate_combined_subset$primate_country_of_origin = 'combined_primate_country_of_origin'
data_dev_clean_combined = rbind(data_dev_clean, primate_combined_subset)
data_dev_clean_combined = data_dev_clean_combined[!(data_dev_clean_combined$species_strain %in% primate_group & data_dev_clean_combined$primate_country_of_origin =='Not_Applicable'),]
data_dev_clean_combined = data_dev_clean_combined %>% 
  group_by(analyte, assay, species_strain, sex, age_category, primate_country_of_origin) %>% 
  mutate(group_name = paste(assay, analyte, species_strain, sex, age_category, primate_country_of_origin, sep = ' '), total_count=n()) %>% 
  ungroup
country_of_origin_order = c('Chinese', 'Indian', 'Not_Applicable', 'Cambodian', 'USA Morgan Island', 'Indonesian', 'St. Kitts', 'combined_primate_country_of_origin')
data_dev_clean_combined = data_dev_clean_combined %>% 
  arrange(match(primate_country_of_origin, country_of_origin_order)) %>% 
  distinct(subject_name, project_name, age_category, species_strain, sex, collection_date, timepoint_day_number, specimen_name, analyte, units, assay, result_value_number, total_count, .keep_all = TRUE)

#separate groups out by containment of zeros (only calculating summary stats on ones with zeros)
analytes_zero = c('PDW', 'BASO#', 'BASO%', 'EO#', 'EO%', 'NH3', 'URIC')
data_dev_zero = data_dev_clean_combined[data_dev_clean_combined$analyte %in% analytes_zero,]
data_dev_clean_combined = data_dev_clean_combined[!data_dev_clean_combined$analyte %in% analytes_zero,]

#separate groups by n<|>20
above_20 = data_dev_clean_combined[data_dev_clean_combined$total_count >=20,]
below_20 = data_dev_clean_combined[data_dev_clean_combined$total_count <20,]

#separate groups not calculating reference ranges for in this prototype: n_distinct<5 OR [5<=n_distinct<10 AND proportion of dominant group<50%]
above_20 = above_20 %>% 
  group_by(group_name) %>% 
  mutate(distinct_count = n_distinct(result_value_number), max_count=max(table(result_value_number)))
above_20$relative_prop = above_20$max_count / above_20$total_count

non_distinct = above_20[above_20$distinct_count < 5,]
distinct_dom = above_20[((above_20$distinct_count >=5 & above_20$distinct_count <10) & above_20$relative_prop > 0.5),]
total_non_distinct_dom = rbind(non_distinct, distinct_dom)
above_20_clean = anti_join(above_20, total_non_distinct_dom)

#add all groups together that are being pushed to summary stats: total_non_distinct_dom, below_20, data_dev_zero
cols_to_drop = c('distinct_count', 'max_count', 'relative_prop')
total_non_distinct_dom = total_non_distinct_dom[, !(names(total_non_distinct_dom) %in% cols_to_drop)]
summary_stats_all = rbind(total_non_distinct_dom, data_dev_zero, below_20)

#create summary statistics function
summary_stats = function(df) {
  df$mean_stats = mean(df$result_value_number)
  df$median_stats = median(df$result_value_number)
  df$stan_dev = sd(df$result_value_number)
  return(df)
}

#run summary stats on all applicable dataframes
summary_stats_above_20_clean = above_20_clean %>% 
  group_by(group_name) %>% 
  do(summary_stats(.))
summary_cols_to_drop = c('distinct_count', 'relative_prop', 'max_count')
summary_stats_above_20_clean = summary_stats_above_20_clean[, !(names(summary_stats_above_20_clean) %in% summary_cols_to_drop)]
summary_stats_combined = summary_stats_all %>% 
  group_by(group_name) %>% 
  do(summary_stats(.))
summary_stats_all = rbind(summary_stats_above_20_clean, summary_stats_combined)

#set up list 
test_subset_list = split(above_20_clean$result_value_number, above_20_clean$group_name)

#______________Statistical Pipeline with appropriate groups__________________________

#build functions

#likelihood ratio function: 
likelihood_ratio_fcn = function(x) {
  model_norm = lm(x~1)
  bc_car = powerTransform(model_norm, family = 'bcnPower')
  sum_bc = summary(bc_car)
  sum_bc_p_value = sum_bc$tests #this shows you the outcomes of the tests in dataframe format; rows are named and so are columns.
  p_value = as.numeric(sum_bc_p_value['LR test, lambda = (1)', 'pval'])
  return(p_value)
}  

#shapiro wilk function: 
shapiro_wilk_fcn = function(x) {
  model_norm = lm(x~1)
  bc_car = powerTransform(model_norm, family = 'bcnPower')
  bc_car_transform = bcnPower(x, lambda = bc_car$lambda, gamma = 0) 
  shapiro_test_p_value = shapiro.test(bc_car_transform)$p.value
  return(shapiro_test_p_value)
}

#loop through all groups where n>20, create dataframe with results of likelihood ratio test and shapiro wilk
df_stats = data.frame()
for (item in names(test_subset_list)) {
  name = item
  group_count = length(test_subset_list[[item]])
  likelihood_ratio = likelihood_ratio_fcn(test_subset_list[[item]])
  shapiro_wilk = shapiro_wilk_fcn(test_subset_list[[item]])
  df_stats_interim = data.frame(name, group_count, likelihood_ratio, shapiro_wilk)
  df_stats = rbind(df_stats, df_stats_interim)
}

#set up function to calculate RRs on df_stats with conditional statements
#NOTE: we are using dixon's outliers but not using this - this does not trigger a BC transformation of the data which will FAIL if the bc-transformed data is negative

BC_function_1 = function(dt, var) {
  var_name = eval(substitute(var),eval(dt))
  ref_lim = refLimit(var_name, out.method = "cook", out.rm = FALSE, RI = "p", CI = "p", refConf = 0.95, limitConf = 0.90)
  ref_int = data.frame(as.list(ref_lim$Ref_Int))
  conf_int = data.frame(as.list(ref_lim$Conf_Int))
  RI_calc = cbind(ref_int, conf_int)
  return(RI_calc)
}

BC_function_2 = function(dt, var) {
  var_name = eval(substitute(var),eval(dt))
  model_norm = lm(var_name~1)
  bc_car = powerTransform(model_norm, family = 'bcnPower')
  bc_car_transform = bcnPower(var_name, lambda = bc_car$lambda, gamma = 0) 
  ref_lim = refLimit(bc_car_transform, out.method = "cook", out.rm = FALSE, RI = "n", CI = "n", refConf = 0.95, limitConf = 0.90)
  ref_int = data.frame(as.list(ref_lim$Ref_Int))
  conf_int = data.frame(as.list(ref_lim$Conf_Int))
  RI_calc = cbind(ref_int, conf_int)
  RI_calc_bt = bcnPowerInverse(RI_calc, lambda = bc_car$lambda, gamma=0)
  return(RI_calc_bt)
}

BC_function_3 = function(dt, var) {
  var_name = eval(substitute(var),eval(dt))
  model_norm = lm(var_name~1)
  bc_car = powerTransform(model_norm, family = 'bcnPower')
  bc_car_transform = bcnPower(var_name, lambda = bc_car$lambda, gamma = 0) 
  ref_lim = refLimit(bc_car_transform, out.method = "cook", out.rm = FALSE, RI = "r", CI = "boot", refConf = 0.95, limitConf = 0.90, bootStat = "perc")
  ref_int = data.frame(as.list(ref_lim$Ref_Int))
  conf_int = data.frame(as.list(ref_lim$Conf_Int))
  RI_calc = cbind(ref_int, conf_int)
  RI_calc_bt = bcnPowerInverse(RI_calc, lambda = bc_car$lambda, gamma = 0)
  return(RI_calc_bt)
}

BC_function_4 = function(dt, var) {
  var_name = eval(substitute(var),eval(dt))
  ref_lim = refLimit(var_name, out.method = "cook", out.rm = FALSE, RI = "r", CI = "boot", refConf = 0.95, limitConf = 0.90, bootStat = "perc")
  ref_int = data.frame(as.list(ref_lim$Ref_Int))
  conf_int = data.frame(as.list(ref_lim$Conf_Int))
  RI_calc = cbind(ref_int, conf_int)
  return(RI_calc)
}

#create column in df_stats that says which outcome the data needs to be piped through
df_stats_col = df_stats %>% mutate(stats_direction = case_when(likelihood_ratio >= 0.05 ~ 'fcn_1',
                                                               likelihood_ratio < 0.05 & group_count >= 120 ~ 'fcn_2',
                                                               likelihood_ratio < 0.05 & group_count < 120 & shapiro_wilk >= 0.05 ~ 'fcn_3',
                                                               likelihood_ratio < 0.05 & group_count < 120 & shapiro_wilk < 0.05 ~ 'fcn_4'))

#merge df_stats_col with above_20_clean
data_dev_clean_stats = merge(above_20_clean, df_stats_col, by.x = 'group_name', by.y = 'name')

#separate groups by function input and run each through appropriate function
fcn_1 = data_dev_clean_stats[data_dev_clean_stats$stats_direction == 'fcn_1',]
fcn_2 = data_dev_clean_stats[data_dev_clean_stats$stats_direction == 'fcn_2',]
fcn_3 = data_dev_clean_stats[data_dev_clean_stats$stats_direction == 'fcn_3',]
fcn_4 = data_dev_clean_stats[data_dev_clean_stats$stats_direction == 'fcn_4',]

col_names_stats = c('group_name', 'Range Low', 'Range High', 'Low CI Low Lim', 'Low CI High Lim', 'High CI Low Lim', 'High CI High Lim')

fcn_1_statistical_outcomes = if (nrow(fcn_1) > 0) {
  fcn_1 %>% 
    group_by(group_name) %>% 
    do(BC_function_1(., result_value_number)) %>% 
    set_colnames(col_names_stats)
} else if (nrow(fcn_1) == 0) {
  fcn_1_statistical_outcomes = setNames(data.frame(matrix(ncol = 7, nrow = 1)), col_names_stats)
}

fcn_2_statistical_outcomes = if (nrow(fcn_2) > 0) {
  fcn_2 %>% 
    group_by(group_name) %>% 
    do(BC_function_2(., result_value_number)) %>% 
    set_colnames(col_names_stats)
} else if (nrow(fcn_2) == 0) {
  fcn_2_statistical_outcomes = setNames(data.frame(matrix(ncol = 7, nrow = 1)), col_names_stats)
}

fcn_3_statistical_outcomes = if (nrow(fcn_3) > 0) {
  fcn_3 %>% 
    group_by(group_name) %>% 
    do(BC_function_3(., result_value_number)) %>% 
    set_colnames(col_names_stats)
} else if (nrow(fcn_3) == 0) {
  fcn_3_statistical_outcomes = setNames(data.frame(matrix(ncol = 7, nrow = 1)), col_names_stats)
}

fcn_4_statistical_outcomes = if (nrow(fcn_4) > 0) {
  fcn_4 %>% 
    group_by(group_name) %>% 
    do(BC_function_4(., result_value_number)) %>% 
    set_colnames(col_names_stats)
} else if (nrow(fcn_4) == 0) {
  fcn_4_statistical_outcomes = setNames(data.frame(matrix(ncol = 7, nrow = 1)), col_names_stats)
}

#put all statistical outputs together in masterdataframe; remove any NA rows
master_statistical_outputs = rbind(fcn_1_statistical_outcomes, fcn_2_statistical_outcomes, fcn_3_statistical_outcomes, fcn_4_statistical_outcomes) %>% 
  filter_all(any_vars(!is.na(.)))

#_____________________create final outcome dataframes: data_entry_component_master dataframe and comprehensive dataframe for vetinarians____________________

#per Dan and Jeremy, if the lower lim is less than 0, set to 0, if the upper limit is a percentage and is above 100, set to 100, and if either of those are removed, take out the CIs
merged_stats_above_20_clean = merge(above_20_clean, master_statistical_outputs, by='group_name')
#change the reference ranges
merged_stats_above_20_clean$`Range Low`[merged_stats_above_20_clean$`Range Low`<0] = 0
merged_stats_above_20_clean$`Range High`[merged_stats_above_20_clean$`Range High`>100 & merged_stats_above_20_clean$units=='PERCENT'] = 100
#update the CIs for the ones that were changed
merged_stats_above_20_clean$`Low CI Low Lim`[merged_stats_above_20_clean$`Range Low`==0] = 'NA'
merged_stats_above_20_clean$`Low CI High Lim`[merged_stats_above_20_clean$`Range Low`==0] = 'NA'
merged_stats_above_20_clean$`High CI Low Lim`[merged_stats_above_20_clean$`Range Low`==100 & merged_stats_above_20_clean$units=='PERCENT'] = 'NA'
merged_stats_above_20_clean$`High CI High Lim`[merged_stats_above_20_clean$`Range Low`==100 & merged_stats_above_20_clean$units=='PERCENT'] = 'NA'

#Data Entry Component DataFrame
merged_stats_above_20_clean[,c('Species', 'Stock', 'Instrument', 'Specimen Class')] = NA
merged_stats_above_20_clean$'Version Date' = Sys.Date()
data_entry_component_df = merged_stats_above_20_clean %>% 
  rename('Country of Origin' = primate_country_of_origin, 'Strain' = species_strain, 'Age' = age_category, 'Sex' = sex, 'Assay' = assay, 'Analyte' = analyte)
data_entry_component_master = distinct(data_entry_component_df[,c("Species", "Country of Origin", "Strain", "Stock", "Age", "Sex", "Assay", "Instrument", "Specimen Class", "Analyte", "Range Low", "Range High", "Version Date")])
data_entry_component_master$ID = seq.int(nrow(data_entry_component_master)) 
data_entry_component_master = data_entry_component_master %>% 
  relocate(ID)
sheets_list_dec = list('Sheet1' = data_entry_component_master)
write.xlsx(sheets_list_dec, "PROD_Data_Entry_Component_Reference_Ranges.xlsx")

#Comprehensive DataFrame: 
#sheet 1: reference ranges from merged_stats_above_20_clean
#sheet 2: summary statistics from summary_stats_all

comprehensive_df = distinct(merged_stats_above_20_clean[,c("group_name", "age_category", "analyte", "assay", "units", "species_strain", "primate_country_of_origin", "sex", "total_count", "Range Low", "Range High", "Low CI Low Lim", "Low CI High Lim", "High CI Low Lim", "High CI High Lim")])
summary_stats_all_filtered = distinct(summary_stats_all[,c("group_name", "age_category", "analyte", "assay", "units", "species_strain", "primate_country_of_origin", "sex", "total_count", "mean_stats", "median_stats", "stan_dev")])

sheets_list = list('Reference Ranges' = comprehensive_df, 'Summary Statistics' = summary_stats_all_filtered)
write.xlsx(sheets_list, file = "PROD_Comprehensive_Data_Reference_Ranges.xlsx")











