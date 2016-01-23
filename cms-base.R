######################################################################
## Copyright (C) 2016, Dave Straube, http://davestraube.com
##     
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
######################################################################

# This program visualizes Centers for Medicare & Medicaid Services (CMS) inpatient data for 2011-2013.

setwd("~/R/Medicaid")
rm(list=ls())

library(stringi)
library(dplyr)
library(foreign)
library(maps)
library(maptools)
library(mapproj)
library(ggplot2)

source("./utils.R", echo = TRUE)

fname <- "./drg.RData"
if ( file.exists(fname) ) {
    load(fname)
} else {
    # Read inpatient data files for 2011/2012/2013.  Files dowloaded from:
    # https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Inpatient.html
    mycolnames <- c("drg",              # Medicare Severity Diagnosis Related Group
                    "id",               # provider CMS certification number
                    "name",             # provider name
                    "street",           # provider street address
                    "city",             # provider city
                    "state",            # provider state
                    "zip",              # provider zip code
                    "hrr",              # hospital referral region
                    "discharges",       # count of discharges billed by provider
                    "avg.charge",       # provider's average charge for services covered by Medicare
                    "avg.total",        # average total payments to all providers for the MS-DRG
                    "avg.medicare")     # average amount that Medicare pays to provider for the MS-DRG
    myclasses <- c("character", "integer", "character", "character", "character", "character", "integer",
                   "character", "integer", "numeric", "numeric", "numeric")
    drg.2011 <- read.csv("./CMS/Medicare_Provider_Charge_Inpatient_DRG100_FY2011.csv",
                         header = TRUE, stringsAsFactors = FALSE, col.names = mycolnames, colClasses = myclasses)
    drg.2012 <- read.csv("./CMS/Medicare_Provider_Charge_Inpatient_DRG100_FY2012.csv",
                         header = TRUE, stringsAsFactors = FALSE, col.names = mycolnames, colClasses = myclasses)
    drg.2013 <- read.csv("./CMS/Medicare_Provider_Charge_Inpatient_DRG100_FY2013.csv",
                         header = TRUE, stringsAsFactors = FALSE, col.names = mycolnames, colClasses = myclasses)
    
    # Add year column so we can backtrack errors and inconsistencies if necessary.
    drg.2011$year <- 2011
    drg.2012$year <- 2012
    drg.2013$year <- 2013
    
    # Combine all years into one file.  Note that this will result in pseudo-duplicates like:
    #                        drg     id                    name ... discharges avg.charge avg.total avg.medicare year
    #   603 - CELLULITIS W/O MCC 110164 COLISEUM MEDICAL CENTER ...         35   20637.60  5056.914     3973.943 2011
    #   603 - CELLULITIS W/O MCC 110164 COLISEUM MEDICAL CENTER ...         26   18296.42  5297.615     3998.500 2012
    #   603 - CELLULITIS W/O MCC 110164 COLISEUM MEDICAL CENTER ...         27   21792.15  5212.185     3968.704 2013
    drg.us <- rbind(drg.2011, drg.2012, drg.2013)
    
    # Source files have many lines of just commas which result in rows of NAs.  Clean up.
    drg.us <- drg.us[complete.cases(drg.us),]
    
    # DRG values include (major) complication and comorbity suffixes as follows.
    #   "377 - G.I. HEMORRHAGE W MCC"                                               
    #   "378 - G.I. HEMORRHAGE W CC"                                                
    #   "379 - G.I. HEMORRHAGE W/O CC/MCC"
    # As per http://www.covidien.com/imageServer.aspx/doc188121.pdf?contentID=15184&contenttype=application/pdf
    # these reflect procedure complexity, not procedure nature.  We're interested in procedures only so we
    # generalize procedure classification by removing code numbers and discrimination by CC, MCC, and others.
    drg.us$drg <- sub("^[0-9]+ - *", "", drg.us$drg)
    drg.us$drg <- sub("* W CC$", "", drg.us$drg)
    drg.us$drg <- sub("* W/O CC$", "", drg.us$drg)
    drg.us$drg <- sub("* W MCC$", "", drg.us$drg)
    drg.us$drg <- sub("* W/O MCC$", "", drg.us$drg)
    drg.us$drg <- sub("* W CC/MCC$", "", drg.us$drg)
    drg.us$drg <- sub("* W/O CC/MCC$", "", drg.us$drg)
    drg.us$drg <- sub("* W MCC OR 4+ VESSELS/STENTS$", "", drg.us$drg)
    drg.us$drg <- sub("* <96 HOURS$", "", drg.us$drg)
    drg.us$drg <- sub("* 96+ HOURS$", "", drg.us$drg)
    drg.us$drg <- sub("* W MV 96+ HOURS$", "", drg.us$drg)
    drg.us$drg <- sub("* W/O MV 96\\+ HOURS$", "", drg.us$drg)
    drg.us$drg <- sub("* W CC OR TPA IN 24 HRS$", "", drg.us$drg)
    
    # Due to rbind and DRG name substitutions, drg.us is no longer unique on drg + hospital.  Remedy by combining
    # rows for each drg + hospital combination.  Can sum discharges, but need to do weighted average of avg.* values.
    myfun <- function(df) {
        if ( dim(df)[1] == 1 ) { return(df) }
        # Grab drg, id, name, street, city, state, zip, hrr, and year from first element.
        retval <- df[1,]
        retval[1, "discharges"] <- sum(df$discharges)
        retval[1, "avg.charge"] <- weighted.mean(df$avg.charge, df$discharges)
        retval[1, "avg.total"] <- weighted.mean(df$avg.total, df$discharges)
        retval[1, "avg.medicare"] <- weighted.mean(df$avg.medicare, df$discharges)
        retval
    }
    
    # There are many inconsistencies in the original CMS data.  Multiple records with the same hospital id may
    # have different name, street, city, or zip values.  (hrr and state seem to be consistent.)
    # Grouping on drg + id only reconciles this automatically with the caveat that myfun() reconciles these 
    # values to whatever was in the first row of the grouping.
    drg.us <- drg.us %>% group_by(drg, id) %>% do(myfun(.))
    drg.us <- as.data.frame(drg.us)
    
    # Year is now misleading so remove it.
    drg.us$year <- NULL
    
    # Save data.
    save(drg.us, drg.2011, drg.2012, drg.2013, file = fname)
}

# Compute most common DRGs and order descending.
freq.us <- as.data.frame(summarise(group_by(drg.us, drg),
                                   discharges = sum(discharges)))
freq.us <- freq.us[order(freq.us$discharges, decreasing = TRUE),]

source("./cms-top10.R", echo = TRUE)
source("./cms-ranges.R", echo = TRUE)
source("./cms-us.R", echo = TRUE)


