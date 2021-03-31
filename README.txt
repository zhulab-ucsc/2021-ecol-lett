README 


Data sourcesThis project was made possible by the many studies that researchers have conducted and published. We encourage future studies using these data to cite the original articles. 

At the start of the supplemental information we provide citations to original data sources for forest inventory tree plot data and for range shift data (Table S1).Below we provide brief explanations of column headings for Dataset S1 (data table of summarized range shift data) and Dataset S3 (data table of summarized tree plot data)


Information for Dataset S1
Each row contains information for one community Key columns are as follows:study_spatial_scale = “local” or “regional”, see main text for definitionduration = number of years elapsed between historic and modern surveystemp_change = estimated temperature change in degrees Celsius between historic and modern surveys, see also “temp_change_and_exp_shift_notes” expected_shift_given_in_study = “yes” or “no”, depending on whether study calculated an expected upslope shift given estimates of local warminglapse_rate = information on the adiabatic lapse rate for the site; we either used the lapse rate reported by the authors of the study or an empirically measured lapse rate from a nearby study within this datasetexpected_shift = expected shift given estimates of local warming and estimates of lapse rate. Unit is meters.species_level_data = “yes” or “no”, depending on whether data for individual species is presented in papersp_n = number of species for which data is reportedsp_n_mean = number of species for which data on shifts at mean/optimum range shifts are reportedshift_mean  = mean observed shift at species’ mean/optimum range shift_mean_se  = standard error of observed shift at species’ mean/optimum range responsiveness_mean = temperature tracking score for shifts at species’ mean/optimum range (= observed shift / expected shift)sp_n_lower_limit = number of species for which data on shifts at lower elevation range shifts are reportedshift_lower_limit  = mean observed shift at species’ lower elevation range limitshift_lower_limit_se  = standard error of observed shift at species’ lower elevation range limitresponsiveness_lower = temperature tracking score for shifts at species’ lower elevation range limit (= observed shift / expected shift)sp_n_upper = number of species for which data on shifts at upper elevation range shifts are reportedshift_upper_limit  = mean observed shift at species’ upper elevation range limitshift_upper_limit_se  = standard error of observed shift at species’ upper elevation range limitresponsiveness_upper = temperature tracking score for shifts at species’ upper elevation range limit (= observed shift / expected shift)weighted_shift = shift at species’ (1) mean/optimum; (2) lower elevation range limit; and (3) upper elevation range limit, weighted by the number of species for each shift typeweighted_se = standard error of the weighted shift (see “weighted_shift”)responsiveness = overall temperature tracking score for each community (= weighted_shift / expected_shift)extraction_notes = notes on how data was extracted from source publicationstemp_change_and_exp_shift_notes = notes for how estimated temperature change was calculated and how expected shift was calculatedInformation for Dataset S3

Each row contains information for one tree plot Key columns are as follows:

mat = mean annual temperature

cti_roc = community temperature index rate of change

mat_roc = mean annual temperature rate of change

t_tracking_ratio = temperature tracking, calculated as a ratio 
