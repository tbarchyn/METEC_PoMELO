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

# read in a map of all the equipment 
METEC_equipment_map <- read.csv ('D:\\data\\2019_METEC\\results\\METEC_equipment_map.csv',
                                 stringsAsFactors = FALSE)

get_group_id <- function (EP_group_code) {
    # EP_group_code is something like 1W or something like that. A pad number and a letter.
    # the reason for this function is Pad 1 and Pad 2 have a sep, well, and tank, but are
    # the same equipment group.
    for (i in 1:length (EP_group_code)) {
        if (!is.na (EP_group_code[i])) {
            # check for pad 1 or pad 2 and run updates
            if (EP_group_code[i] == '1S' | EP_group_code[i] == '1W' | EP_group_code[i] == '1T') {
                EP_group_code[i] <- '1'
            } else if (EP_group_code[i] == '2S' | EP_group_code[i] == '2W' | EP_group_code[i] == '2T') {
                EP_group_code[i] <- '2'
            }
        }
    }
    return (EP_group_code)
}

get_all_equipment <- function (EP_pad) {
    # function to return a vector of all the available equipment on a given pad, this can
    # be compared against what was detected to determine false positives, etc.
    # EP_pad = the pad number associated with the emissions point
    all_equipment <- vector (length = length (EP_pad), mode = 'character')
    for (i in 1:length (EP_pad)) {
        if (!is.na (EP_pad[i])) {
            # subset to pad equipment
            pad_equipment <- METEC_equipment_map[METEC_equipment_map$Pad == EP_pad[i], ]
            all_equipment[i] <- paste (pad_equipment$Equipment, collapse = ' ')
        }
    }
    return (all_equipment)
}

get_all_groups <- function (EP_pad) {
    # function to return a vector of all the available equipment groups on a given pad, this can
    # be compared against what was detected to determine false positives, etc.
    # EP_pad = the pad number associated with the emissions point
    all_groups <- vector (length = length (EP_pad), mode = 'character')
    for (i in 1:length (EP_pad)) {
        if (!is.na (EP_pad[i])) {
            # subset to pad equipment
            pad_equipment <- METEC_equipment_map[METEC_equipment_map$Pad == EP_pad[i], ]
            all_groups[i] <- paste (unique (pad_equipment$Group), collapse = ' ')
        }
    }
    return (all_groups)
}

