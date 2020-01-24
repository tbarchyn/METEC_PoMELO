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

source ('D:\\dev\\R_utilities\\unit_conversions.R')

#############################################################################################
# SETUP
site_stats_enhanced_filename <- 'D:\\data\\2019_METEC\\results\\site_stats_enhanced.csv'
eqs_filename <- 'D:\\data\\2019_METEC\\results\\eqs.csv'
groups_filename <- 'D:\\data\\2019_METEC\\results\\groups.csv'
missed_eqs_filename <- 'D:\\data\\2019_METEC\\results\\missed_eqs.csv'
missed_groups_filename <- 'D:\\data\\2019_METEC\\results\\missed_groups.csv'
mdls_filename <- 'D:\\data\\2019_METEC\\results\\mdls.csv'

site_stats <- read.csv (site_stats_enhanced_filename, stringsAsFactors = FALSE)
site_stats <- site_stats [!is.na (site_stats$pad_id), ]
eqs <- read.csv (eqs_filename, stringsAsFactors = FALSE)
groups <- read.csv (groups_filename, stringsAsFactors = FALSE)


#############################################################################################
# BASE PLOT 
# experiment plot plots the experiments along the x axis forward through time
exp_plot <- function (y, hline_seq, xlab = "", ylab="", col = 'black', sds = 0,
                      x_text_adjust = 0.8, y_text_adjust = 0.8, ...){
    # experiment plots
    # y = the data element
    # xlab = x label
    # ylab = y label
    # col = color
    # sds = standard deviations
    # x_text_adjust = x text adjustment to position better
    # y_text_adjust = y text adjustment to position better

    # set up x data
    x <- 1:length (y)
    
    # create layout
    zones <- matrix (c(2, 1), ncol = 2, byrow = TRUE)
    layout (zones, widths = c(4/5, 1/5))
    
    # barplot histogram
    yhist <- hist (y, breaks = 20, plot = FALSE)
    par (mar = c(3, 1, 1, 1))
    barplot (yhist$counts, col = col, axes = TRUE, xlim = c(0, max (yhist$counts) + 1),
             space = 0, horiz = TRUE)
    box ()

    # scatterplot
    par (mar = c(3, 3, 1, 0))
    plot (x, y, col = col, ...)
    
    # add day lines with labels
    days <- c('Mon', 'Tues', 'Wed', 'Thurs', 'Fri')
    daybreaks <- c(0.0, 30.5, 45.5, 65.5, 85.5)
    abline (v = daybreaks, col = 'grey')
    text (x = daybreaks + 4, y = min (y, na.rm = T), labels = days, col = 'grey')
    
    # add horizontal lines
    abline (h = hline_seq, col = 'lightgrey')
    
    # add points again to plot on top of grid lines
    points (x, y, col = col, ...)
    if (length (sds) > 1 & sds[1] > 0) {
        arrows (x0 = x, x1 = x, y0 = y - sds, y1 = y + sds, col = col, length = 0, code = 3)
    }
    box ()

    # marginal data
    par (oma = c(3, 3, 0, 0))
    mtext (xlab, side = 1, line = 1, outer = TRUE, adj = 0, 
           at = x_text_adjust * (mean(x, na.rm = T) - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))
    mtext (ylab, side = 2, line = 1, outer = TRUE, adj = 0, 
           at = y_text_adjust * (mean(y, na.rm = T) - min(y, na.rm = T))/(max(y, na.rm = T) - min(y, na.rm = T)))
}
# test plot to confirm plotting
exp_plot (y = site_stats$df.ch4_pressure_mean, hline_seq = seq (83.2,85, 0.2),
          xlab = 'Experiment', ylab = 'Pressure',
          col = 'red', sds = site_stats$df.ch4_pressure_std)

#############################################################################################
# PLOT TEST CONDITIONS
# histogram plots of conditions when tests were performed in
# TEMPERATURE
exp_plot (y = site_stats$df.ch4_temp_mean, hline_seq = seq (-10, 30, 5),
          xlab = 'Experiment', ylab = 'Temperature (deg. C)',
          col = 'red', sds = site_stats$df.ch4_temp_std)

# WIND SPEED
exp_plot (y = site_stats$df.true_wspd_mean, hline_seq = seq (0, 10, 1),
          xlab = 'Experiment', ylab = 'Wind speed (m/s)',
          col = 'blue', sds = site_stats$df.true_wspd_std)

# WIND DIRECTION
exp_plot (y = site_stats$pn.mean_wdir, hline_seq = seq (0, 360, 50),
          xlab = 'Experiment', ylab = 'Wind direction (deg.)',
          col = 'orange' )

#############################################################################################
# LIST OF STATISTICS
print (paste ('number of single blind tests', '105'))
print (paste ('pad distribution', summary (factor (site_stats$pad_id))))
print (paste ('number of equipment surveyed', nrow (eqs)))
print (paste ('number of equipment groups surveyed', nrow (groups)))
print (paste ('number with precalibrated estimation', length (site_stats[site_stats$precal_estimates,1])))
days <- factor (strftime (as.POSIXct (site_stats$Start), '%d'))
print (summary (days))
print (paste ('number of blanks', length (site_stats[site_stats$n_emitting_eqs == 0,1])))

#############################################################################################
# SURVEY CHARACTERISTICS
# histograms of survey data
# TIME TO COMPLETE SURVEY
exp_plot (y = site_stats$df.log_time_rng / 60.0, hline_seq = seq (0, 15, 2),
          xlab = 'Experiment', ylab = 'Survey time (minutes)',
          col = 'forestgreen')

# DRIVING SPEED
exp_plot (y = site_stats$df.spd_over_grnd_mean, hline_seq = seq (0, 10, 1),
          xlab = 'Experiment', ylab = 'Driving speed (m/s)',
          col = 'blue', sds = site_stats$df.spd_over_grnd_std)

#############################################################################################
# EMISSIONS CHARACTERISTICS
# PAD DISTRIBUTION
dev.off()
plot (site_stats$pad_id, col = 'black', ylab = 'Pad', xlab = 'Experiment')

# add day lines with labels
days <- c('Mon', 'Tues', 'Wed', 'Thurs', 'Fri')
daybreaks <- c(0.0, 30.5, 45.5, 65.5, 85.5)
abline (v = daybreaks, col = 'grey')
text (x = daybreaks + 4, y = 1.5, labels = days, col = 'grey')

# add horizontal lines
abline (h = seq (0, 5, 1), col = 'lightgrey')

dev.off ()
par (oma = c(0,0,0,0))
# TOTAL EMISSIONS RATE DISTRIBUTION (g/s)
exp_plot (y = site_stats$all_emissions, hline_seq = seq (0, 0.5, 0.05),
          xlab = 'Experiment', ylab = 'Sum pad emissions rates (g/s)',
          col = 'blue')

# TOTAL EMISSIONS RATE DISTRIBUTION (scfh CH4)
exp_plot (y = site_stats$all_emissions_scfh, hline_seq = seq (0, 60, 10),
          xlab = 'Experiment', ylab = 'Sum pad emissions rates (scfh CH4)',
          col = 'forestgreen')

# TOTAL EMISSIONS RATE DISTRIBUTION (cubic meters per day)
exp_plot (y = site_stats$all_emissions_cubes, hline_seq = seq (0, 60, 5),
          xlab = 'Experiment', ylab = expression (Sum~pad~emissions~rates~~(m^3/day)),
          col = 'orange')

# NUMBER OF EMISSIONS SOURCES
exp_plot (y = site_stats$n_available_EPs, hline_seq = seq (0, 8, 1),
          xlab = 'Experiment', ylab = 'Number of emissions points',
          col = 'red')

# NUMBER OF EMITTING EQUIPMENT
exp_plot (y = site_stats$n_emitting_eqs / site_stats$n_available_eqs, 
          hline_seq = seq (0, 1, 0.2),
          xlab = 'Experiment', ylab = expression (n[emitting]/n[available]~~(equipment)),
          col = 'black', y_text_adjust = 0.5)

# NUMBER OF EMITTING GROUPS
exp_plot (y = site_stats$n_emitting_groups / site_stats$n_available_groups,
          hline_seq = seq (0, 1, 0.2),
          xlab = 'Experiment', ylab = expression (n[emitting]/n[available]~~(equipment~groups)),
          col = 'purple', y_text_adjust = 0.3)

# HISTOGRAM OF AGGREGATED EQUIPMENT
hist (eqs$real_lr, breaks = 100, col = 'yellow', xlab = 'Emissions rate (g/s)', main = '')
box ()

# HISTOGRAM OF AGGREGATED EQUIPMENT GROUPS
hist (groups$real_lr, breaks = 50, col = 'orange', xlab = 'Emissions rate (g/s)', main = '')
box ()

#############################################################################################
# PLOT REPORTED RESULTS
# EQUIPMENT
true_eqs <- site_stats$n_true_emitting_eqs / site_stats$n_emitting_eqs
exp_plot (y = true_eqs, hline_seq = seq (0, 1, 0.2),
          xlab = 'Experiment', ylab = expression (n[detected]/n[emitting]~~(equipment)),
          col = 'purple', y_text_adjust = 0.3)
summary (factor (true_eqs))
print (paste ('proportion of correctly flagged eqs', 
              length (true_eqs[true_eqs == 1 & !is.na(true_eqs)]) /
                  length (true_eqs[!is.na(true_eqs)])))
print (paste ('n = ', length (true_eqs[!is.na(true_eqs)])))

# EQUIPMENT GROUPS
true_groups <- site_stats$n_true_emitting_groups / site_stats$n_emitting_groups
exp_plot (y = true_groups, hline_seq = seq (0, 1, 0.2),
          xlab = 'Experiment', ylab = expression (n[detected]/n[emitting]~~(equipment~groups)),
          col = 'black', y_text_adjust = 0.20)
print (paste ('proportion of correctly flagged groups', 
              length (true_groups[true_groups == 1 & !is.na(true_groups)]) /
                  length (true_groups[!is.na(true_groups)])))
print (paste ('n = ', length (true_groups[!is.na(true_groups)])))

# note: sites with every equipment or group emitting come through as NaN and don't end up on plot
# EXTRA EQUIPMENT
extra_eqs <- site_stats$n_false_emitting_eqs / (site_stats$n_available_eqs - site_stats$n_emitting_eqs)
exp_plot (y = extra_eqs, hline_seq = seq (0, 1, 0.2),
          xlab = 'Experiment', ylab = expression (n[detected]/n[not~emitting]~~(equipment)),
          col = 'green', y_text_adjust = 0.4)

# EXTRA GROUPS
extra_groups <- site_stats$n_false_emitting_groups / (site_stats$n_available_groups - site_stats$n_emitting_groups)
exp_plot (y = extra_groups, hline_seq = seq (0, 1, 0.2),
          xlab = 'Experiment', ylab = expression (n[detected]/n[not~emitting]~~(equipment~groups)),
          col = 'orange', y_text_adjust = 0.5)

# statistics
print ('equipment')
print (paste ('reported and emitting', sum (site_stats$n_true_emitting_eqs, na.rm = T)))
print (paste ('not reported and not emitting', sum (site_stats$n_false_clean_eqs, na.rm = T)))
print (paste ('reported and not emitting', sum (site_stats$n_false_emitting_eqs, na.rm = T)))
print (paste ('not reported and not emitting', sum (site_stats$n_true_clean_eqs, na.rm = T)))

print ('equipment groups')
print (paste ('reported and emitting', sum (site_stats$n_true_emitting_groups, na.rm = T)))
print (paste ('not reported and not emitting', sum (site_stats$n_false_clean_groups, na.rm = T)))
print (paste ('reported and not emitting', sum (site_stats$n_false_emitting_groups, na.rm = T)))
print (paste ('not reported and not emitting', sum (site_stats$n_true_clean_groups, na.rm = T)))

#############################################################################################
# PLOT BINNED MDL CURVES
# bin equipment and equipment group data
bin_width <- 0.01
cuts <- seq (0, 0.2, bin_width)
labels <- cuts + (bin_width / 2.0)

# create mdl table 
mdls <- data.frame ()
for (i in 1:(length (cuts) - 1)) {
    eq_cut <- eqs[!is.na (eqs$real_lr) & eqs$real_lr <= cuts[i+1] & eqs$real_lr > cuts[i], ]
    eq_reported_frac <- length (eq_cut$reported [eq_cut$reported]) / nrow (eq_cut)
    gr_cut <- groups[!is.na (groups$real_lr) & groups$real_lr <= cuts[i+1] & groups$real_lr > cuts[i], ]
    gr_reported_frac <- length (gr_cut$reported [gr_cut$reported]) / nrow (gr_cut)

    # list and append
    bin_list <- list (g_per_s = labels[i], n_equipment = nrow(eq_cut), eq_reported_frac = eq_reported_frac,
                      n_groups = nrow(gr_cut), gr_reported_frac = gr_reported_frac)
    mdls <- rbind (mdls, bin_list)
}
mdls$scfh <- g_per_s_to_scfh (mdls$g_per_s)
mdls$cubes <- g_per_s_to_m3_per_day (mdls$g_per_s)

# make mdl plot
mdl_plot <- function (mdls, er_unit, xlab, vseq) {
    # make a mdl plot
    # mdls = mdl dataframe
    # er_unit = one of g_per_s, scfh, or cubes to pick unit for x axis
    # xlab = x label
    # vseq = vertical gridline seq

    # split to remove nas and plot connecting lines
    eq_mdls <- mdls[!is.na (mdls$eq_reported_frac),]
    eq_x <- eq_mdls [, er_unit]
    gr_mdls <- mdls[!is.na (mdls$gr_reported_frac),]
    gr_x <- gr_mdls [, er_unit]
    
    # make multipanel layout
    zones <- matrix (c(1, 2), ncol = 2, byrow = TRUE)
    layout (zones, widths = c(1/2, 1/2))
    
    # plot mdl bins
    plot (eq_x, eq_mdls$eq_reported_frac, ylim = c(0,1), xlab = xlab, 
          ylab = expression (n[detected]/n[emitting]), col = 'red')
    abline (v = vseq, col = 'grey')
    abline (h = seq (0, 1, 0.2), col = 'grey')
    points (eq_x, eq_mdls$eq_reported_frac, col = 'red')
    lines (eq_x, eq_mdls$eq_reported_frac, col = 'red')
    points (gr_x, gr_mdls$gr_reported_frac, col = 'blue')
    lines (gr_x, gr_mdls$gr_reported_frac, col = 'blue')
    legend (x = 'bottomright', lty = 1, col = c('red', 'blue'), c('Equipment', 'Equipment groups'))
    
    # plot ns (numbers)
    plot (mdls[, er_unit], mdls$n_equipment, xlab = xlab, ylab = 'Bin samples (n)', col = 'red')
    abline (v = vseq, col = 'grey')
    abline (h = seq (0, 100, 20), col = 'grey')
    points (mdls[, er_unit], mdls$n_equipment, col = 'red')
    lines (mdls[, er_unit], mdls$n_equipment, col = 'red')
    points (mdls[, er_unit], mdls$n_groups, col = 'blue')
    lines (mdls[, er_unit], mdls$n_groups, col = 'blue')
    legend (x = 'topright', lty = 1, col = c('red', 'blue'), c('Equipment', 'Equipment groups'))
}
# MDL PLOTS (different units)
mdl_plot (mdls, 'g_per_s', 'Emissions rate (g/s)', vseq = seq(0,0.2, 0.05))
mdl_plot (mdls, 'scfh', 'Emissions rate (scfh CH4)', vseq = seq(0, 50, 5))
mdl_plot (mdls, 'cubes', expression (Emissions~rate~~(m^3/day)), vseq = seq (0, 25, 2.5))

#############################################################################################
# TABLE of MISSED POINTS
# interrogate the missed equipments
# calculate ranks
relative_rank <- function (x, d) {
    # return relative position in data as a 'percentile'
    x <- x[!is.na (x)]
    return (100.0 * length (x[x <= d]) / length (x))
}

df_relative_rank <- function (x, d) {
    # same as relative rank, just for dataframes
    rr <- x * NA
    for (i in 1:length (x)) {
        rr[i] <- relative_rank (x, d[i])
    }
    return (rr)
}
eqs$temp_rank <- df_relative_rank (eqs$df.ch4_temp_mean, eqs$df.ch4_temp_mean)
eqs$wspd_rank <- df_relative_rank (eqs$df.true_wspd_mean, eqs$df.true_wspd_mean)
groups$temp_rank <- df_relative_rank (groups$df.ch4_temp_mean, groups$df.ch4_temp_mean)
groups$wspd_rank <- df_relative_rank (groups$df.true_wspd_mean, groups$df.true_wspd_mean)

# generic table columns
include_cols <- c('Test', 'pad_id', 'df.ch4_temp_mean', 'temp_rank', 'df.true_wspd_mean',
                  'wspd_rank', 'pn.mean_wdir', 'real_lr', 'real_lr_sd')

# missed equipment table
missed <- eqs[!eqs$reported, c(include_cols, 'eq_name')]
missed$real_lr_scfh <- g_per_s_to_scfh (missed$real_lr)
missed$real_lr_scfh_sd <- g_per_s_to_scfh (missed$real_lr_sd)
missed$real_lr_cubes <- g_per_s_to_m3_per_day (missed$real_lr)
missed$real_lr_cubes_sd <- g_per_s_to_m3_per_day (missed$real_lr_sd)

write.csv (missed, missed_eqs_filename, row.names = FALSE)

# missed equipment group table
missed <- groups[!groups$reported, c(include_cols, 'gr_name')]
missed$real_lr_scfh <- g_per_s_to_scfh (missed$real_lr)
missed$real_lr_scfh_sd <- g_per_s_to_scfh (missed$real_lr_sd)
missed$real_lr_cubes <- g_per_s_to_m3_per_day (missed$real_lr)
missed$real_lr_cubes_sd <- g_per_s_to_m3_per_day (missed$real_lr_sd)

write.csv (missed, missed_groups_filename, row.names = FALSE)

# sum data
# equipment scale
det_eqs <- sum (site_stats$emissions_true_emitting_eqs, na.rm = T)
missed_eqs <- sum (site_stats$emissions_false_clean_eqs, na.rm = T)
det_grs <- sum (site_stats$emissions_true_emitting_groups, na.rm = T)
missed_grs <- sum (site_stats$emissions_false_clean_groups, na.rm = T)

print (paste ('eq sum flagged rates ', det_eqs))
print (paste ('eq sum missed rates ', missed_eqs))
print (paste ('eq fraction detected', det_eqs / (det_eqs + missed_eqs)))

print (paste ('gr sum flagged rates ', det_grs))
print (paste ('gr sum missed rates ', missed_grs))
print (paste ('gr fraction detected', det_grs / (det_grs + missed_grs)))

# minimum equipment group scale results
min_eq <- which (groups$real_lr == min (groups$real_lr, na.rm = T))
groups$real_lr[min_eq]
groups$real_lr_sd[min_eq]
groups$real_lr_scfh[min_eq]
groups$real_lr_sd_scfh[min_eq]
groups$real_lr_cubes[min_eq]
groups$real_lr_sd_cubes[min_eq]

# write out MDL dataframe
write.csv (mdls, mdls_filename, row.names = F)




