# POMELO METEC ANALYSIS 2019
# Copyright Thomas Barchyn 2019-2020
# This file is part of POMELO METEC ANALYSIS 2019

# POMELO METEC ANALYSIS 2019 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# POMELO METEC ANALYSIS 2019 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with POMELO METEC ANALYSIS 2019.  If not, see <https://www.gnu.org/licenses/>.

# Purpose: this file conducts main analysis and aggregation for base analysis

# NOTES:
# 1) All file paths will require adjustment to work on your computer.
# 2) Please get in touch with Tom Barchyn (tbarchyn@ucalgary.ca) should
#    further clarifications be required.
# 3) Raw events files are not included on dataverse to limit the number of files
#    uploaded. This does not reduce the reproducibility of these analyses as the
#    aggregated events file included (q_sources_aggregate) includes all columns
#    to reproduce the initial aggregation.
# 4) Non-essential data confidential to METEC have been scrubbed.
# 5) Reported rates have been modified to maintain quantification confidentiality.

source ('D:\\dev\\R_utilities\\unit_conversions.R')
source ('D:\\dev\\R_utilities\\sd_quadrature.R')
source ('D:\\dev\\METEC_PoMELO\\METEC_equipment_groups.R')
source ('D:\\dev\\METEC_PoMELO\\METEC_comparator.R')
source ('D:\\dev\\METEC_PoMELO\\METEC_replication_repairs.R')

############################################################################################
# SETUP
# Set up filenames
base_dir <- 'D:\\data\\2019_METEC\\results\\Results\\'
events_filename_18 <- paste (base_dir, '2019-11-18_08-06-09\\Events.csv', sep = '\\')
events_filename_19 <- paste (base_dir, '2019-11-19_07-41-50\\Events.csv', sep = '\\')
events_filename_20 <- paste (base_dir, '2019-11-20_07-50-54\\Events.csv', sep = '\\')
events_filename_21 <- paste (base_dir, '2019-11-21_08-10-27\\Events.csv', sep = '\\')
events_filename_22 <- paste (base_dir, '2019-11-22_07-55-37\\Events.csv', sep = '\\')
events_filenames <- c(events_filename_18, events_filename_19, events_filename_20,
                      events_filename_21, events_filename_22)

post_dir <- 'D:\\data\\2019_METEC\\results\\post'
events_enhanced_filename_18 <- paste (post_dir, 'events_18_nov_2019.csv', sep = '\\')
events_enhanced_filename_19 <- paste (post_dir, 'events_19_nov_2019.csv', sep = '\\')
events_enhanced_filename_20 <- paste (post_dir, 'events_20_nov_2019.csv', sep = '\\')
events_enhanced_filename_21 <- paste (post_dir, 'events_21_nov_2019.csv', sep = '\\')
events_enhanced_filename_22 <- paste (post_dir, 'events_22_nov_2019.csv', sep = '\\')
events_enhanced_filenames <- c(events_enhanced_filename_18, events_enhanced_filename_19,
                               events_enhanced_filename_20, events_enhanced_filename_21,
                               events_enhanced_filename_22)

q_sources_filename_18 <- paste (post_dir, 'settled_18_nov_2019.csv', sep = '\\')
q_sources_filename_19 <- paste (post_dir, 'settled_19_nov_2019.csv', sep = '\\')
q_sources_filename_20 <- paste (post_dir, 'settled_20_nov_2019.csv', sep = '\\')
q_sources_filename_21 <- paste (post_dir, 'settled_21_nov_2019.csv', sep = '\\')
q_sources_filename_22 <- paste (post_dir, 'settled_22_nov_2019.csv', sep = '\\')
q_sources_aggregate_filename <- paste (post_dir, 'settled_aggregate.csv', sep = '\\')
q_sources_filenames <- c(q_sources_filename_18, q_sources_filename_19, q_sources_filename_20,
                         q_sources_filename_21, q_sources_filename_22)

# site stats filenames
site_stats_filename <- 'D:\\data\\2019_METEC\\results\\site_stats.csv'
site_stats_enhanced_filename <- 'D:\\data\\2019_METEC\\results\\site_stats_enhanced.csv'

# field reported data
reported_filename <- 'D:\\data\\2019_METEC\\results\\METECBlindResults_reported.csv'

# aggregated emissions data
eqs_filename <- 'D:\\data\\2019_METEC\\results\\eqs.csv'
groups_filename <- 'D:\\data\\2019_METEC\\results\\groups.csv'

# Test 99 Notes
# Test 99 had an issue where the flow meter was changed over, but the test was run
# on the old flow meter. The work-around is to use calibrated flow rates as the experiment
# settled flow rate is clearly incorrect.
test99_eventID <- 69
test99_filename <- events_filename_22

# Write survey stats into padmapper folders
write_survey_sources <- TRUE
padmapper_db_folder <- 'D:\\data\\2019_METEC\\padmapper\\padmapper_db'

# read in the reported data, this is needed to clean up sources
# and un-calibrated data that was measured Thursday 21 November morning
reported <- read.csv (reported_filename, stringsAsFactors = FALSE)
reported$Equipment_g_per_s <- as.numeric (reported$Equipment_g_per_s)


############################################################################################
# FIX UP EVENT FILES
# create aggregate filename
q_sources_aggregate <- data.frame ()

# loop through each aggregate file
for (f in 1:length (events_filenames)) {
    
    # 1) Read in Events file and do any cleanup
    # set specific filenames
    events_filename <- events_filenames[f]
    events_enhanced_filename <- events_enhanced_filenames[f]
    q_sources_filename <- q_sources_filenames[f]
    
    # read in events file
    events <- read.csv (events_filename, na.strings = c('nan', 'NA'), stringsAsFactors = FALSE)
    
    # date / time metrics
    events$elapsed <- events$EndTS - events$StartTS
    events$elapsed_settled <- events$EndTS - events$SettledTS
    events$StartTS_MST <- as.POSIXct (events$StartTS, origin = '1970-01-01 00:00.00 UTC', tz = 'MST')
    events$SettledTS_MST <- as.POSIXct (events$SettledTS, origin = '1970-01-01 00:00.00 UTC', tz = 'MST')
    events$EndTS_MST <- as.POSIXct (events$EndTS, origin = '1970-01-01 00:00.00 UTC', tz = 'MST')
    
    # group metrics for emissions points
    events$EP_pad <- NA
    events$EP_group <-NA                
    events$EP_eqnum <- NA
    events$EP_eqname <- NA
    EP_name_split <- strsplit (events$EPName, '-')
    for (i in 1:length(EP_name_split)) {
        events$EP_group[i] <- EP_name_split [[i]][1]
        events$EP_pad[i] <- substr(EP_name_split [[i]][1], 1, 1)
        events$EP_eqnum[i] <- substr(EP_name_split [[i]][2], 1, 1)
        events$EP_eqname[i] <- substr(events$EPName [i], 1, 4)
    }
    # run repairs with the METEC equipment groups translater
    events$EP_group <- get_group_id (events$EP_group)
    
    # 2) Subset to settled flow rates
    q_sources <- events[events$EventType == 'EmissionProfile.Settled', ]
    q_sources$EP_flow_fraction <- NA
    q_sources$calsum_diff <- NA

    # 3) Calculate fraction of total settled flow is going to each emissions point
    flow_fraction_method <- TRUE        # set to true to use flow fraction method (by default)
    calfallback_ids <- NULL             # log event ids that should fallback
    
    # examine event output flow rates
    for (id in unique (q_sources$EventID)) {
        # subset to the flow event with the unique ID
        flow_event <- q_sources[q_sources$EventID == id, ]
        
        # check number of fed soures matches what we expect
        if (nrow (flow_event) != flow_event$ActiveEPCount[1]) {
            print (paste ('ERROR: mismatch between number off active emissions points with flow event', id))
        }
        
        # calc fraction for each emissions point, this should correct for sum of cal period flow rates
        # and be easily multiplied by the settled flow rate flowing through the meter for the experiment
        EP_flow_fractions <- flow_event$CalSettled / sum (flow_event$CalSettled)

        # special instructions and fallbacks
        if (events_filenames[f] == test99_filename & id == test99_eventID) {
            print ('NOTE: test 99 special instructions were executed')
            fallback <- TRUE
        } else if (any (is.na (flow_event$CalSettled)) & any (!is.na (flow_event$CalSettled))) {
            fallback <- TRUE
        } else {
            fallback <- FALSE
        }
        
        # execute fallbacks
        if (fallback) {
            calfallback_ids <- c(calfallback_ids, id)
        }
        
        # assign back into the q_sources dataframe
        q_sources$EP_flow_fraction[q_sources$EventID == id] <- EP_flow_fractions
        
        # calculate metric of difference between the sum of calibrated flow (the mean should just catch NAs)
        q_sources$calsum_diff[q_sources$EventID == id] <- sum (flow_event$CalSettled) - mean (flow_event$SettledFlow)
    }

    # 4) Calculate bulk gas flow for each individual release point
    if (flow_fraction_method) {
        # calculate bulk gas flow rates scaled by EP_flow_fraction
        q_sources$bulk_flow <- q_sources$SettledFlow * q_sources$EP_flow_fraction
        q_sources$bulk_flow_sd <- df_sd_mult (df = q_sources, vals_cols = c('SettledFlow'),
                                              sds_cols = c('SettledSTDev'), end_result_col = c('bulk_flow'))

        # deal with fallback to using the calibrated flows in situations where one of the cals had an NA and
        # flow fractions could not be calculated properly as we didn't have a full cal suite
        for (id in calfallback_ids) {
            id_rows <- q_sources$EventID == id
            q_sources$bulk_flow[id_rows] <- q_sources$CalSettled[id_rows]
            q_sources$bulk_flow_sd[id_rows] <- q_sources$bulk_flow[id_rows] * 
                                               (q_sources$SettledSTDev[id_rows] / q_sources$SettledFlow[id_rows])
        }
        
    } else {
        # calculate the bulk flow by just bringing through the calibrated flow rate
        q_sources$bulk_flow <- q_sources$CalSettled
        
        # simulate the standard deviation as the corresponding fraction of the settled standard deviation 
        q_sources$bulk_flow_sd <- q_sources$bulk_flow * (q_sources$SettledSTDev / q_sources$SettledFlow)
    }

    # 5) check pressure and temp differences to trigger a QC fail for further follow up
    # this shows the difference between pressure and temperature between calibration and experiment
    max_p_diff <- 0.01
    max_t_diff <- 0.01      
    q_sources$tp_QC <- (abs (q_sources$pRatioCurToCal) < max_p_diff &
                        abs (q_sources$tRatioCurToCal) < max_t_diff)
    
    # 6) Calculate grams per second methane, and sd
    q_sources$g_per_s_CH4 <- bulk_slpm_METEC_to_g_per_s_CH4 (slpm_METEC = q_sources$bulk_flow,
                                                             CH4_mol_fraction = q_sources$C1_mol_frac)
    q_sources$g_per_s_CH4_sd <- df_sd_mult (df = q_sources, vals_cols = c('C1_mol_frac', 'bulk_flow'),
                                            sds_cols = c('C1_mol_frac_std', 'bulk_flow_sd'),
                                            end_result_col = c('g_per_s_CH4'))
    
    # 7) convert to other units for EDA of data
    q_sources$scfh_CH4 <- g_per_s_to_scfh (q_sources$g_per_s_CH4)
    q_sources$scfh_CH4_sd <- g_per_s_to_scfh (q_sources$g_per_s_CH4_sd)
    q_sources$m3_per_day_CH4 <- g_per_s_to_m3_per_day (q_sources$g_per_s_CH4)
    q_sources$m3_per_day_CH4_sd <- g_per_s_to_m3_per_day (q_sources$g_per_s_CH4_sd)
        
    # 8) write files and aggregate
    write.csv (events, events_enhanced_filename, row.names = FALSE)
    write.csv (q_sources, q_sources_filename, row.names = FALSE)
    q_sources_aggregate <- rbind (q_sources_aggregate, q_sources)
    print (paste ('completed', q_sources_filename))
}

# make a new ID column, known as the full tour ID for further indexing, the other ids start at 1 for each day
q_sources_aggregate$TourID <- 1:nrow (q_sources_aggregate)

# write out an aggregate dataframe
write.csv (q_sources_aggregate, q_sources_aggregate_filename, row.names = FALSE)



############################################################################################
# LINK SITES TO SOURCES
# The site stats dataframe out of padmapper records the starting time (when record was hit)
# and the ending time (when stop was hit). This can be used to see what sources were 'on'
# during the experiment and link the sources to the pad experiment.
site_stats <- read.csv (site_stats_filename, na.strings = 'nan', stringsAsFactors = FALSE)
site_stats$survey_start <- as.POSIXct (site_stats$df.computer_time_sample_start, tz = 'MST')
site_stats$survey_end <- as.POSIXct (site_stats$df.computer_time_sample_end, tz = 'MST')

# make a linkable experiment label
ex_nums <- gsub ('Test ', '', site_stats$Name)
site_stats$Test <- as.numeric (ex_nums)

# pull out a column with pad ID
site_stats$pad_id <- NA
for (i in 1:nrow (site_stats)) {
    pad_split <- strsplit (site_stats$Notes[i], ' ')[[1]]
    if (length(pad_split) == 2) {
        site_stats$pad_id[i] <- as.numeric (pad_split[2])
    }
}

# events talleys (from the settled events datafield)
site_stats$event1_n <- NA
site_stats$event2_n <- NA
site_stats$event3_n <- NA
site_stats$event4_n <- NA
site_stats$pad_sources_n <- NA
site_stats$precal_estimates <- FALSE

# rates, listed in terms of groups, equipments, etc.
site_stats$EP_tour_ids <- NA
site_stats$EP_names <- NA
site_stats$EP_rates <- NA
site_stats$EP_rates_sds <- NA
site_stats$eqs <- NA
site_stats$eqs_rates <- NA
site_stats$eqs_rates_sds <- NA
site_stats$groups <- NA
site_stats$groups_rates <- NA
site_stats$groups_rates_sds <- NA

# add post-calculated estimated flow rates columns to q_sources_aggregate
q_sources_aggregate$est_g_per_s <- NA
q_sources_aggregate$est_g_per_s_sd <- NA

# loop through the different sites
for (s in 1:nrow (site_stats)) {
    # subset events from the settled sources
    # event type 1 is a spanned event, where it started before survey, and ended after
    event1 <- (q_sources_aggregate$SettledTS_MST < site_stats$survey_start[s] & 
               q_sources_aggregate$EndTS_MST > site_stats$survey_end[s])
    
    # event type 2 is a early event where it started before the survey, but ended midway
    # through the survey.
    event2 <- (q_sources_aggregate$SettledTS_MST < site_stats$survey_start[s] & 
               q_sources_aggregate$EndTS_MST < site_stats$survey_end[s] &
               q_sources_aggregate$EndTS_MST > site_stats$survey_start[s])
    
    # event type 3 is a late event where it started after the survey, but ended after
    # the end of the survey.
    event3 <- (q_sources_aggregate$SettledTS_MST > site_stats$survey_start[s] & 
               q_sources_aggregate$SettledTS_MST < site_stats$survey_end[s] &
               q_sources_aggregate$EndTS_MST > site_stats$survey_end[s])
    
    # event type 4 is a small event where it started after the survey, but ended midway
    # through the survey.
    event4 <- (q_sources_aggregate$SettledTS_MST > site_stats$survey_start[s] & 
               q_sources_aggregate$EndTS_MST < site_stats$survey_end[s])
    
    # aggregate_sources to a global mask that pulls out all sources active
    survey_mask <- event1 | event2 | event3 | event4

    # fill in statistics on matches
    site_stats$event1_n[s] <- sum (event1)
    site_stats$event2_n[s] <- sum (event2)
    site_stats$event3_n[s] <- sum (event3)
    site_stats$event4_n[s] <- sum (event4)
    
    # chop out survey sources
    survey_sources <- q_sources_aggregate[survey_mask, ]

    if (!is.na (site_stats$pad_id[s])) {
        # further chop out matching sources on the specific pad
        pad_mask <- survey_sources$EP_pad == site_stats$pad_id[s]
        
        # allow sources from pad 4 to be included when pad 5 is surveyed
        # the precise inclusion or exclusion is done with the METEC comparator so if
        # the 4W or 4S equipment groups are included here it isn't an issue
        if (site_stats$pad_id[s] == 5) {
            pad_mask <- pad_mask | survey_sources$EP_pad == 4
        }
    
        # use the pad mask to pull out the sources that are relevant for the target pad
        site_stats$pad_sources_n[s] <- sum (pad_mask)
        survey_sources <- survey_sources[pad_mask, ]
    
        # perform repairs if we have a clear pad link, this will not repair all sources as replicate
        # matches are only performed on a pad basis (and spurious settled sources can't be repaired)
        rep_repair_list <- replication_repairs (survey_sources = survey_sources, q_sources_aggregate = q_sources_aggregate,
                                                site_stats = site_stats, reported = reported, s)
        survey_sources <- rep_repair_list$survey_sources
        q_sources_aggregate <- rep_repair_list$q_sources_aggregate

        # Populate emissions points fields
        # 1) Populate all present emissions points, including individual TourIDs for replication
        site_stats$EP_tour_ids[s] <- paste (survey_sources$TourID, collapse = ' ')
        site_stats$EP_names[s] <- paste (survey_sources$EPName, collapse = ' ')
        
        # flop over to working with estimates if we are forced to use the estimated data
        # or the
        reported_rows <- which (reported$Test == site_stats$Test[s])
        if (any (reported$Replicate_cal [reported_rows])) {
            site_stats$precal_estimates[s] <- TRUE
            survey_sources$g_per_s_CH4 <- survey_sources$est_g_per_s
            survey_sources$g_per_s_CH4_sd <- survey_sources$est_g_per_s_sd
        }
        
        # assign into the site stats    
        site_stats$EP_rates[s] <- paste (survey_sources$g_per_s_CH4, collapse = ' ')
        site_stats$EP_rates_sds[s] <- paste (survey_sources$g_per_s_CH4_sd, collapse = ' ')
        
        # 2) Populate all equipment fields
        eqs <- NULL
        eqs_rates <- NULL
        eqs_rates_sds <- NULL
        for (i in unique (survey_sources$EP_eqname)) {
            eq_sources <- survey_sources[survey_sources$EP_eqname == i, ]
            eqs <- c(eqs, i)
            eqs_rates <- c(eqs_rates, sum (eq_sources$g_per_s_CH4))
            eqs_rates_sds <- c(eqs_rates_sds, sd_add (eq_sources$g_per_s_CH4_sd))
        }
        site_stats$eqs[s] <- paste (eqs, collapse = ' ')
        site_stats$eqs_rates[s] <- paste (eqs_rates, collapse = ' ')
        site_stats$eqs_rates_sds[s] <- paste (eqs_rates_sds, collapse = ' ')
        
        # 3) Populate all equipment groups fields
        groups <- NULL
        groups_rates <- NULL
        groups_rates_sds <- NULL
        for (i in unique (survey_sources$EP_group)) {
            group_sources <- survey_sources[survey_sources$EP_group == i, ]
            groups <- c(groups, i)
            groups_rates <- c(groups_rates, sum (group_sources$g_per_s_CH4))
            groups_rates_sds <- c(groups_rates_sds, sd_add (group_sources$g_per_s_CH4_sd))
        }
        site_stats$groups[s] <- paste (groups, collapse = ' ')
        site_stats$groups_rates[s] <- paste (groups_rates, collapse = ' ')
        site_stats$groups_rates_sds[s] <- paste (groups_rates_sds, collapse = ' ')
    
        # 4) Write survey sources into the padmapper folder
        if (write_survey_sources) {
            # get site folder and construct new path name

            site_folder <- strsplit (site_stats$Directory[s], '\\\\')[[1]][2]
            print (site_folder)
            survey_sources_filename <- paste (padmapper_db_folder, site_folder, 'settled_sources.csv', sep = '\\')
            write.csv (survey_sources, survey_sources_filename, row.names = F)
        }
    }
    print (paste ('completed', site_stats$Name[s]))
}

write.csv (q_sources_aggregate, q_sources_aggregate_filename, row.names = FALSE)
write.csv (site_stats, site_stats_enhanced_filename, row.names = FALSE)


############################################################################################
# LINK REAL SOURCES TO REPORTED DATA
# the reported data are given with validated equipment codes, these are similar to 5S-1, where
# the equipment group is 5S, the equipment is separator 1. These should be run through the comparator
# function.

# get the group IDs
reported$Group <- NA
EP_name_split <- strsplit (reported$Equipment_ID, '-')
for (i in 1:length(EP_name_split)) {
    len_EP_name_split <- length (EP_name_split[[i]])
    if (len_EP_name_split > 1) {
        reported$Group[i] <- EP_name_split [[i]][1]
    } else if (len_EP_name_split == 0) {
        reported$Group[i] <- ''
    } else {
        print ('ERROR: problem with site', i)
    }
}
# run repairs with the METEC equipment groups translater
reported$Group <- get_group_id (reported$Group)

# analyze each test individually by appending data into the site stats dataframe
site_stats$rep_no_emissions <- NA               # reported as clean

# equipment specific reporting elements
site_stats$rep_eqs <- NA                        # reported equipments
site_stats$rep_emissions_eqs <- NA              # reported emissions rates (g/s)
site_stats$n_available_eqs <- NA                # number of available equipments on pad
site_stats$n_emitting_eqs <- NA                 # number emitting equipments on pad
site_stats$n_reported_eqs <- NA                 # number of reported equipments on pad
site_stats$n_true_clean_eqs <- NA               # true clean equipments
site_stats$n_false_clean_eqs <- NA              # false clean equipments
site_stats$n_true_emitting_eqs <- NA            # true emitting equipments
site_stats$n_false_emitting_eqs <- NA           # false emitting equipments
site_stats$emissions_true_emitting_eqs <- NA    # talley emissions from true emitting (found)
site_stats$emissions_false_clean_eqs <- NA      # talley emissions from false clean (missed)
site_stats$emissions_true_emitting_eqs_sds <- NA # talley emissions from true emitting (found)
site_stats$emissions_false_clean_eqs_sds <- NA   # talley emissions from false clean (missed)

# group specific reporting elements
site_stats$rep_groups <- NA                     # reported groups
site_stats$rep_emissions_groups <- NA           # reported emissions rates by group (g/s)
site_stats$n_available_groups <- NA             # number of available groups
site_stats$n_emitting_groups <- NA              # number of emitting groups
site_stats$n_reported_groups <- NA              # number of reported groups
site_stats$n_true_clean_groups <- NA            # true clean groups
site_stats$n_false_clean_groups <- NA           # false clean groups
site_stats$n_true_emitting_groups <- NA         # true emitting groups
site_stats$n_false_emitting_groups <- NA        # false emitting groups
site_stats$emissions_true_emitting_groups <- NA # talley emissions from true emitting (found)
site_stats$emissions_false_clean_groups <- NA   # tallen emissions from false clean (missed)
site_stats$emissions_true_emitting_groups_sds <- NA # talley emissions sds (found)
site_stats$emissions_false_clean_groups_sds <- NA   # talley emissions sds (missed)

# make an equipment quantifications base dataframe, and a groups base quantifications dataframe
eqs <- data.frame ()
groups <- data.frame ()

# loop through each test and address the reporting spreadsheet
for (i in 1:nrow (site_stats)) {
    
    # check to make sure we have a defined test
    if (!is.na (site_stats$Test[i])) {
    
        # get reported test data from the reported spreadsheet
        test_data <- reported[reported$Test == site_stats$Test[i], ]
    
        # collapse reported groups and reported emissions
        # check for any dual reported equipment ids
        if (nrow(test_data) != length (unique (test_data$Equipment_ID))) {
            print (paste ('ERROR with site stats row', i))
        }
        site_stats$rep_eqs[i] <- paste (unique (test_data$Equipment_ID), collapse = ' ')
        site_stats$rep_groups[i] <- paste (unique (test_data$Group), collapse = ' ')
        site_stats$rep_emissions_eqs[i] <- paste (test_data$Equipment_g_per_s, collapse = ' ')
        
        # sum up group emissions
        group_rates <- NULL
        for (g in unique (test_data$Group)) {
            group_rate <- sum (test_data$Equipment_g_per_s[test_data$Group == g], na.rm = T)
            group_rates <- c(group_rates, group_rate)
        }
        
        site_stats$rep_emissions_groups[i] <- paste (group_rates, collapse = ' ')
        
        # check if no emissions points were reported and fill in appropriate column
        if (any (test_data$No_emissions)) {
            site_stats$rep_no_emissions[i] <- TRUE
        } else {
            site_stats$rep_no_emissions[i] <- FALSE
        }
        
        # run the equipment comparator
        eq_data <- equipment_comparator (reported = site_stats$rep_eqs[i],
                                         reported_rates = site_stats$rep_emissions_eqs[i],
                                         emitting = site_stats$eqs[i],
                                         emitting_rates = site_stats$eqs_rates[i],
                                         emitting_rates_sd = site_stats$eqs_rates_sds[i],
                                         EP_pad = site_stats$pad_id[i])
        site_stats$n_available_eqs[i] <- eq_data$n_available
        site_stats$n_emitting_eqs[i] <- eq_data$n_emitting
        site_stats$n_reported_eqs[i] <- eq_data$n_reported
        site_stats$n_true_clean_eqs[i] <- eq_data$n_true_clean
        site_stats$n_false_clean_eqs[i] <- eq_data$n_false_clean
        site_stats$n_true_emitting_eqs[i] <- eq_data$n_true_emitting
        site_stats$n_false_emitting_eqs[i] <- eq_data$n_false_emitting
        site_stats$emissions_true_emitting_eqs[i] <- eq_data$emissions_true_emitting
        site_stats$emissions_false_clean_eqs[i] <- eq_data$emissions_false_clean
        site_stats$emissions_true_emitting_eqs_sds[i] <- eq_data$emissions_true_emitting_sd
        site_stats$emissions_false_clean_eqs_sds[i] <- eq_data$emissions_false_clean_sd
            
        # append new data to equipment listing with all relevant site stats to context the error
        if (nrow (eq_data$matched_emissions) > 0) {
            eq_data$matched_emissions <- cbind (eq_data$matched_emissions, site_stats[i, ])
            eqs <- rbind (eqs, eq_data$matched_emissions)
        }
        
        # run the group comparator
        gr_data <- group_comparator (reported = site_stats$rep_groups[i],
                                     reported_rates = site_stats$rep_emissions_groups[i],
                                     emitting = site_stats$groups[i],
                                     emitting_rates = site_stats$groups_rates[i],
                                     emitting_rates_sd = site_stats$groups_rates_sds[i],
                                     EP_pad = site_stats$pad_id[i])
        site_stats$n_available_groups[i] <- gr_data$n_available
        site_stats$n_emitting_groups[i] <- gr_data$n_emitting
        site_stats$n_reported_groups[i] <- gr_data$n_reported
        site_stats$n_true_clean_groups[i] <- gr_data$n_true_clean
        site_stats$n_false_clean_groups[i] <- gr_data$n_false_clean
        site_stats$n_true_emitting_groups[i] <- gr_data$n_true_emitting
        site_stats$n_false_emitting_groups[i] <- gr_data$n_false_emitting
        site_stats$emissions_true_emitting_groups[i] <- gr_data$emissions_true_emitting
        site_stats$emissions_false_clean_groups[i] <- gr_data$emissions_false_clean
        site_stats$emissions_true_emitting_groups_sds[i] <- gr_data$emissions_true_emitting_sd
        site_stats$emissions_false_clean_groups_sds[i] <- gr_data$emissions_false_clean_sd
        
        # append new data to group listing with all relevant site stats to context the error
        if (nrow (gr_data$matched_emissions) > 0) {
            gr_data$matched_emissions <- cbind (gr_data$matched_emissions, site_stats[i, ])
            groups <- rbind (groups, gr_data$matched_emissions)
        }
    }
}

# calculate all emissions
site_stats$all_emissions <- site_stats$emissions_false_clean_eqs + site_stats$emissions_true_emitting_eqs

# convert from g per s to scfh and m3 per day to help compare against METEC logged data
base_site_stats_emissions_cols <- c(
    'emissions_true_emitting_eqs', 'emissions_false_clean_eqs',
    'emissions_true_emitting_eqs_sds', 'emissions_false_clean_eqs_sds',
    'emissions_true_emitting_groups', 'emissions_false_clean_groups',
    'emissions_true_emitting_groups_sds', 'emissions_false_clean_groups_sds'
)
site_stats_emissions_cols <- c(base_site_stats_emissions_cols, 'all_emissions')
for (i in site_stats_emissions_cols) {
    site_stats[, paste (i, 'scfh', sep = '_')] <- g_per_s_to_scfh (site_stats[, i])
    site_stats[, paste (i, 'cubes', sep = '_')] <- g_per_s_to_m3_per_day (site_stats[, i])
}

eqs_emissions_cols <- c(base_site_stats_emissions_cols,
    'reported_lr', 'real_lr', 'real_lr_sd'
)
for (i in eqs_emissions_cols) {
    eqs[, paste (i, 'scfh', sep = '_')] <- g_per_s_to_scfh (eqs[, i])
    eqs[, paste (i, 'cubes', sep = '_')] <- g_per_s_to_m3_per_day (eqs[, i])
}

groups_emissions_cols <- c(base_site_stats_emissions_cols,
    'reported_lr', 'real_lr', 'real_lr_sd'
)
for (i in groups_emissions_cols) {
    groups[, paste (i, 'scfh', sep = '_')] <- g_per_s_to_scfh (groups[, i])
    groups[, paste (i, 'cubes', sep = '_')] <- g_per_s_to_m3_per_day (groups[, i])
}

# calculate the number of emissions points
site_stats$n_available_EPs <- NA
for (i in 1:nrow (site_stats)) {
    EPs <- strsplit (site_stats$EP_names[i], ' ')[[1]]
    site_stats$n_available_EPs[i] <- length (EPs)
}
    
# write out data
write.csv (site_stats, site_stats_enhanced_filename, row.names = FALSE)
write.csv (eqs, eqs_filename, row.names = FALSE)
write.csv (groups, groups_filename, row.names = FALSE)


