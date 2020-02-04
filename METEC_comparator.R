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

# NOTES:
# 1) All file paths will require adjustment to work on your computer.
# 2) Please get in touch with Tom Barchyn (tbarchyn@ucalgary.ca) should
#    further clarifications be required.

# METEC comparator: this contains the function(s) and logic required to compare reported results
# to real results on either an equipment group or equipment scale.

source ('D:\\dev\\METEC_PoMELO\\METEC_equipment_groups.R')
source ('D:\\dev\\R_utilities\\sd_quadrature.R')

equipment_comparator <- function (reported, reported_rates, emitting, emitting_rates, emitting_rates_sd, EP_pad) {
    # Equipment comparator compares equipment available to emit on a site against what was reported
    # to be emitting. This function also records the sum of correctly attributed equipment and the
    # sum of incorrectly attributed emitting equipment.
    
    # reported = a space separated character list of reported equipment codes
    # reported_rates = a space separated character list of reported rates
    # emitting = a space separated character list of emitting equipment codes
    # emitting_rates = a space separated character list of emitting rates
    # emitting_rates_sd = a space separated character list of emitting rates sds
    # EP_pad = the pad used to test
    
    # convert the input text list to a list
    if (length(reported) > 1 | length(reported_rates) > 1 | length(emitting) > 1 | length(emitting_rates) > 1) {
        print ('ERROR: input to the equipment comparator has length greater than 1')
    }
    
    reported <- strsplit (reported, ' ')[[1]]
    reported_rates <- as.numeric (strsplit (reported_rates, ' ')[[1]])
    emitting <- strsplit (emitting, ' ')[[1]]
    emitting_rates <- as.numeric (strsplit (emitting_rates, ' ')[[1]])
    if (is.na (emitting_rates_sd)) {
        emitting_rates_sd <- emitting_rates * NA    # set them all to NA
    } else {
        emitting_rates_sd <- as.numeric (strsplit (emitting_rates_sd, ' ')[[1]])
    }
    
    # get available equipment
    available <- get_all_equipment (EP_pad)
    available <- strsplit (available, ' ')[[1]]
    
    # talley different lengths
    n_available <- length (available)
    n_emitting <- length (emitting)
    n_reported <- length (reported)
    
    # calculate matrix of flagging results
    n_true_clean <- 0
    n_false_clean <- 0
    n_true_emitting <- 0
    n_false_emitting <- 0
    
    # calculate sum of emissions rates
    emissions_true_emitting <- 0.0
    emissions_false_clean <- 0.0
    emissions_true_emitting_sd <- 0.0
    emissions_false_clean_sd <- 0.0
    
    # produce a matched emissions vector that will match the reported rates
    # matched to explicit pieces of equipment for extraction into a comparison
    # the unreported emissions are also included here to help make MDL curves
    matched_emissions <- data.frame (eq_name = character (), reported_lr = numeric (),
                                     real_lr = numeric (), real_lr_sd = numeric (),
                                     reported = logical ())
    
    for (i in available) {
        
        if ((i %in% emitting) & !(i %in% reported)) {
            # emitting, but we didn't report it, false clean
            n_false_clean <- n_false_clean + 1
            real_lr <- emitting_rates [which (i == emitting)]
            real_lr_sd <- emitting_rates_sd [which (i == emitting)]
            emissions_false_clean <- emissions_false_clean + real_lr
            emissions_false_clean_sd <- sd_add (c(emissions_false_clean_sd, real_lr_sd))            
            
            # create a matched emissions entry for the dataframe (this equipment was a not detect)
            reported_lr <- NA
            new_matched_emissions_row <- list (eq_name = i, reported_lr = reported_lr,
                                               real_lr = real_lr, real_lr_sd = real_lr_sd,
                                               reported = FALSE)
            matched_emissions <- rbind (matched_emissions, new_matched_emissions_row)
            matched_emissions$eq_name <- as.character (matched_emissions$eq_name)
            
        } else if ((i %in% emitting) & (i %in% reported)) {
            # true emitting, its emitting and we said so
            n_true_emitting <- n_true_emitting + 1
            real_lr <- emitting_rates [which (i == emitting)]
            real_lr_sd <- emitting_rates_sd [which (i == emitting)]
            emissions_true_emitting <- emissions_true_emitting + real_lr
            emissions_true_emitting_sd <- sd_add (c(emissions_true_emitting_sd, real_lr_sd))
            
            # create a matched emissions entry for the dataframe (this equipment was a detect)
            reported_lr <- reported_rates [which (i == reported)]
            new_matched_emissions_row <- list (eq_name = i, reported_lr = reported_lr,
                                               real_lr = real_lr, real_lr_sd = real_lr_sd,
                                               reported = TRUE)
            matched_emissions <- rbind (matched_emissions, new_matched_emissions_row)
            matched_emissions$eq_name <- as.character (matched_emissions$eq_name)
            
        } else if (!(i %in% emitting) & (i %in% reported)) {
            # false positive, the place is not actually emitting
            n_false_emitting <- n_false_emitting + 1
            
        } else if (!(i %in% emitting) & !(i %in% reported)) {
            # true negative, this site is clean and we said so
            n_true_clean <- n_true_clean + 1
            
        } else {
            print (paste ('ERROR: could not successfully sort out this emissions point', i))
        }
    }

    # QC sanity checks
    # loop through emitting sources, check exists in available
    for (i in emitting) {
        if (!(i %in% available)) {
            print (paste ('ERROR: source', i, 'was emitting, but is not in available equipment'))
        }
    }
    # loop through reported sources, check exists in available
    for (i in reported) {
        if (!is.na(i) & !(i %in% available)) {
            print (paste ('ERROR: source', i, 'was reported, but is not in available equipment'))
        }
    }
    out_data <- list (n_available = n_available, n_emitting = n_emitting, n_reported = n_reported,
                      n_true_clean = n_true_clean, n_false_clean = n_false_clean,
                      n_true_emitting = n_true_emitting, n_false_emitting = n_false_emitting,
                      emissions_true_emitting = emissions_true_emitting, emissions_false_clean = emissions_false_clean,
                      emissions_true_emitting_sd = emissions_true_emitting_sd,
                      emissions_false_clean_sd = emissions_false_clean_sd,
                      matched_emissions = matched_emissions)
    return (out_data)
}

group_comparator <- function (reported, reported_rates, emitting, emitting_rates, emitting_rates_sd, EP_pad) {
    # Group comparator compares equipment available to emit on a site against what was reported
    # to be emitting. This function also returns the sum of true reported emissions
    
    # reported = a space separated character list of reported group codes
    # reported_rates = a space separated character list of reported rates
    # emitting = a space separated character list of emitting group codes
    # emitting_rates = a space separated character list of emitting rates
    # emitting_rates_sd = a space separated character list of emitting rates sd
    # EP_pad = the pad used to test
    
    # convert the input text list to a list
    if (length(reported) > 1 | length(reported_rates) > 1 | length(emitting) > 1 | length(emitting_rates) > 1) {
        print ('ERROR: input to the equipment comparator has length greater than 1')
    }
    
    reported <- strsplit (reported, ' ')[[1]]
    reported_rates <- as.numeric (strsplit (reported_rates, ' ')[[1]])
    emitting <- strsplit (emitting, ' ')[[1]]
    emitting_rates <- as.numeric (strsplit (emitting_rates, ' ')[[1]])
    if (is.na (emitting_rates_sd)) {
        emitting_rates_sd <- emitting_rates * NA    # set them all to NA
    } else {
        emitting_rates_sd <- as.numeric (strsplit (emitting_rates_sd, ' ')[[1]])
    }
        
    # get available equipment groups
    available <- get_all_groups (EP_pad)
    available <- strsplit (available, ' ')[[1]]
    
    # talley different lengths
    n_available <- length (available)
    n_emitting <- length (emitting)
    n_reported <- length (reported)
    
    # calculate matrix of flagging results
    n_true_clean <- 0
    n_false_clean <- 0
    n_true_emitting <- 0
    n_false_emitting <- 0
    
    # calculate sum of emissions rates
    emissions_true_emitting <- 0.0
    emissions_false_clean <- 0.0
    emissions_true_emitting_sd <- 0.0
    emissions_false_clean_sd <- 0.0
    
    # produce a matched emissions vector that will match the reported rates
    # matched to explicit pieces of groups for extraction into a comparison
    # the unreported emissions are also included here to help make MDL curves
    matched_emissions <- data.frame (gr_name = character (), reported_lr = numeric (),
                                     real_lr = numeric (), real_lr_sd = numeric (),
                                     reported = logical ())
    
    for (i in available) {
        
        if ((i %in% emitting) & !(i %in% reported)) {
            # emitting, but we didn't report it, false clean
            n_false_clean <- n_false_clean + 1
            real_lr <- emitting_rates [which (i == emitting)]
            real_lr_sd <- emitting_rates_sd [which (i == emitting)]
            emissions_false_clean <- emissions_false_clean + real_lr
            emissions_false_clean_sd <- sd_add (c(emissions_false_clean_sd, real_lr_sd))
                        
            # create a matched emissions entry for the dataframe (this equipment was a not detect)
            reported_lr <- NA
            new_matched_emissions_row <- list (gr_name = i, reported_lr = reported_lr,
                                               real_lr = real_lr, real_lr_sd = real_lr_sd,
                                               reported = FALSE)
            matched_emissions <- rbind (matched_emissions, new_matched_emissions_row)
            matched_emissions$gr_name <- as.character (matched_emissions$gr_name)
            
        } else if ((i %in% emitting) & (i %in% reported)) {
            # true emitting, its emitting and we said so
            n_true_emitting <- n_true_emitting + 1
            real_lr <- emitting_rates [which (i == emitting)]
            real_lr_sd <- emitting_rates_sd [which (i == emitting)]
            emissions_true_emitting <- emissions_true_emitting + real_lr
            emissions_true_emitting_sd <- sd_add (c(emissions_true_emitting_sd, real_lr_sd))
            
            # create a matched emissions entry for the dataframe
            reported_lr <- reported_rates [which (i == reported)]
            new_matched_emissions_row <- list (gr_name = i, reported_lr = reported_lr,
                                               real_lr = real_lr, real_lr_sd = real_lr_sd,
                                               reported = TRUE)
            matched_emissions <- rbind (matched_emissions, new_matched_emissions_row)
            matched_emissions$gr_name <- as.character (matched_emissions$gr_name)
            
        } else if (!(i %in% emitting) & (i %in% reported)) {
            # false positive, the place is not actually emitting
            n_false_emitting <- n_false_emitting + 1
            
        } else if (!(i %in% emitting) & !(i %in% reported)) {
            # true negative, this site is clean and we said so
            n_true_clean <- n_true_clean + 1
            
        } else {
            print (paste ('ERROR: could not successfully sort out this emissions point', i))
        }
    }
    
    # QC sanity checks
    # loop through emitting sources, check exists in available
    for (i in emitting) {
        if (!(i %in% available)) {
            print (paste ('ERROR: source', i, 'was emitting, but is not in available groups'))
        }
    }
    # loop through reported sources, check exists in available
    for (i in reported) {
        if (!is.na(i) & !(i %in% available)) {
            print (paste ('ERROR: source', i, 'was reported, but is not in available groups'))
        }
    }
    out_data <- list (n_available = n_available, n_emitting = n_emitting, n_reported = n_reported,
                      n_true_clean = n_true_clean, n_false_clean = n_false_clean,
                      n_true_emitting = n_true_emitting, n_false_emitting = n_false_emitting,
                      emissions_true_emitting = emissions_true_emitting, emissions_false_clean = emissions_false_clean,
                      emissions_true_emitting_sd = emissions_true_emitting_sd,
                      emissions_false_clean_sd = emissions_false_clean_sd,
                      matched_emissions = matched_emissions)
    return (out_data)
}




