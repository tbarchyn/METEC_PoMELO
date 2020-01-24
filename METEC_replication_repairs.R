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

source ('D:\\dev\\R_utilities\\sd_quadrature.R')

replication_repairs <- function (survey_sources, q_sources_aggregate, site_stats, reported, s) {
    # replication repairs returns survey sources with repaired rates, and performs the required
    # repairs.
    
    # this returns the survey sources, and also updates the q_sources_aggregate for the chopped
    # survey sources.
    
    # survey_sources = input survey sources dataframe
    # q_sources_aggregate = the quality sources aggregated
    # site_stats = the site stats dataframe from padmapper
    # reported = the reported dataframe which contains the replication links
    # s = the row of site stats, which indicates which experiment we are presently on
    
    # get present experiment number
    experiment <- site_stats$Test[s]

    # figure out whether this experiment was a replicate
    reported_rows <- which (reported$Test == experiment)
    if (any (reported$Replicate_cal [reported_rows])) {
        
        # get the initial experiment number and tour ids associated with that initial experiment
        initial_exp <- reported$Initial_test [reported_rows][1]
        tour_ids <- site_stats$EP_tour_ids [which (site_stats$Test == initial_exp)]
        tour_ids <- as.numeric (strsplit (tour_ids, ' ')[[1]])
        
        # get the initial sources associated with the initial run of this pad
        q_sources_rows <- NULL
        for (ids in tour_ids) {
            q_sources_rows <- c(q_sources_rows, which(q_sources_aggregate$TourID == ids))
        }
        init_sources <- q_sources_aggregate [q_sources_rows, ]
        
        # convert temperatures to K, pressures don't need to be converted because they have a real zero
        # and are only used in a ratio in the calculations, which normalizes the unit differences
        # conversions of standard deviations just involve rescaling to the C scale (/ 1.8)
        init_sources$ControllerTAvg_K <- ((init_sources$ControllerTAvg - 32.0) / 1.8) + 273.15 
        init_sources$ControllerTStd_K <- init_sources$ControllerTStd / 1.8
        survey_sources$ControllerTAvg_K <- ((survey_sources$ControllerTAvg - 32.0) / 1.8) + 273.15 
        survey_sources$ControllerTStd_K <- survey_sources$ControllerTStd / 1.8

        # correct each emissions point individually
        for (i in 1:nrow(survey_sources)) {
            # subset to an initial source with the same name as the survey source in question
            init_source <- init_sources[which (init_sources$EPName == survey_sources$EPName[i]), ]
            
            # prepare the input data (note: there is no calSettled sd brought through)
            init_Q <- init_source$CalSettled[1]
            init_P <- init_source$ControllerPAvg[1]
            init_T <- init_source$ControllerTAvg_K[1]
            init_P_sd <- init_source$ControllerPStd[1]
            init_T_sd <- init_source$ControllerTStd_K[1]
            
            survey_T <- survey_sources$ControllerTAvg_K[i]
            survey_P <- survey_sources$ControllerPAvg[i]
            survey_T_sd <- survey_sources$ControllerTStd_K[i]
            survey_P_sd <- survey_sources$ControllerPStd[i]
            
            # run correction equation, this corrects the estimated
            # Q with a correction for the orifice plate.
            survey_Q <- init_Q * (survey_P / init_P) * sqrt (init_T) / sqrt (survey_T)

            # calculate the sd in parts
            sqrt_init_T <- sqrt (init_T)
            sqrt_init_T_sd <- 0.5 * (init_T_sd / init_T)
            sqrt_survey_T <- sqrt (survey_T)
            sqrt_survey_T_sd <- 0.5 * (survey_T_sd / survey_T)
            
            survey_Q_sd <- sd_mult (vals = c(survey_P, init_P, sqrt_init_T, sqrt_survey_T),
                                    sds = c(survey_P_sd, init_P_sd, sqrt_init_T_sd, sqrt_survey_T_sd),
                                    end_result = survey_Q)
            
            # calculate the grams per second CH4, and the estimated standard deviation
            g_per_s <- bulk_slpm_METEC_to_g_per_s_CH4 (slpm_METEC = survey_Q,
                                                       CH4_mol_fraction = survey_sources$C1_mol_frac[i])
            
            g_per_s_sd <- sd_mult (vals = c(survey_Q, survey_sources$C1_mol_frac[i]),
                                   sds = c(survey_Q_sd, survey_sources$C1_mol_frac_std[i]),
                                   end_result = g_per_s)
            
            # fill in the correct row in the survey sources dataframe
            survey_sources$est_g_per_s[i] <- g_per_s
            survey_sources$est_g_per_s_sd[i] <- g_per_s_sd

            # fill in the rows in the q sources aggregate            
            q_sources_aggregate [q_sources_aggregate$TourID == survey_sources$TourID[i], 'est_g_per_s'] <- g_per_s
            q_sources_aggregate [q_sources_aggregate$TourID == survey_sources$TourID[i], 'est_g_per_s_sd'] <- g_per_s_sd
        }
        
        # print the results to inspect
        if (FALSE) {
            print (init_sources$EPName)
            print (init_sources$g_per_s_CH4)
            print (survey_sources$EPName)
            print (survey_sources$est_g_per_s)
        }    
    }
    return (list (survey_sources = survey_sources, q_sources_aggregate = q_sources_aggregate))
}



