 metadata <- read.csv(file.choose(), na.strings = c("", "NA"))
 
   # Part 1: Preprocessing of the sample metadata
   
   # Step 1: Remove variables that are mostly empty
   #
   # CMV serology, anti-CCP, rheumatoid factor and eGFR are missing in 83 to 89% of the samples, because these tests were only done at some visits. chem.protein is missing in 29%. We remove them. For CMV we use cmv_status, which is complete.
   # First we look at the percentage missing of these variables:
   #
   remove_vars <- c("cmv.igg_serology", "cmv.igg_serology_interpretation",
                                       "infl.anti_ccp3", "infl.anti_ccp31",
                                       "infl.rf_iga_interpretation", "infl.rf_iga_result",
                                       "infl.rf_igm_interpretation", "infl.rf_igm_result",
                                       "chem.egfr_aa", "chem.egfr_non_aa",
                                       "chem.protein")
   
     round(colMeans(is.na(metadata[, remove_vars])) * 100)   # % missing
     
     # Then we remove them. %in% checks if a name is in the list, and ! means "not", so we keep all columns that are not in remove_vars.
     #
     metadata <- metadata[, !(colnames(metadata) %in% remove_vars)]
   
     ncol(metadata)   # 76 - 11 = 65 variables
 
     #
     # Step 2: Weight or BMI of 0 is impossible
     #
     # Nobody weighs 0 kg, so these values are errors. We change them into NA (missing).
     #
     sum(metadata$am.weight == 0, na.rm = TRUE)   # 6 samples with weight 0

   sum(metadata$am.bmi == 0,    na.rm = TRUE)   # 2 samples with BMI 0
 
   
     metadata$am.weight[metadata$am.weight == 0] <- NA
   metadata$am.bmi[metadata$am.bmi == 0]       <- NA
   
     #
     # Step 3: Fill in missing height from another visit of the same person
     #
     # An adult's height does not change between visits. So if height is missing at one visit, we use the median height of that same person from their other visits.
     #
     # ave() calculates something per group (here: the median height per person) and gives the result back for every row.
     #
     sum(is.na(metadata$am.height))   # missing height before
 
   
     person_height <- ave(metadata$am.height, metadata$subject.subjectGuid,
                                                 FUN = function(h) median(h, na.rm = TRUE))
   
     missing_height <- is.na(metadata$am.height)
   metadata$am.height[missing_height] <- person_height[missing_height]
   
     # people who never had their height measured get NaN; we make that NA
     metadata$am.height[is.nan(metadata$am.height)] <- NA
   
     sum(is.na(metadata$am.height))   # missing height after

   
     #
     # Step 4: Calculate the missing BMI
     #
     # Now that more heights are known, we calculate BMI where it is missing:
     # BMI = weight (kg) / height (m)². Height is in cm, so we divide by 100 first.
     #
     sum(is.na(metadata$am.bmi))      # missing BMI before

   
     missing_bmi <- is.na(metadata$am.bmi)
   metadata$am.bmi[missing_bmi] <- metadata$am.weight[missing_bmi] /
         (metadata$am.height[missing_bmi] / 100)^2
   
     sum(is.na(metadata$am.bmi))      # missing BMI after

   
     #
     # The BMI that is still missing belongs to samples where weight is missing, or to people whose height was never measured.
     #
     # The 10 samples without routine lab values are left as they are.
     #
     # Step 5: Save the cleaned metadata
     #
     write.csv(metadata, "sample_metadata_clean.csv", row.names = FALSE)
   
     dim(metadata)   # 867 samples, 65 variables
   
     View(metadata)
   View(metadata)
   write.csv(metadata, "sample_metadata_clean.csv", row.names = FALSE)