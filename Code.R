## load pacakges
library(tidyverse)
library(R.utils)
library(stringdist)

## download data if necessary
if (!file.exists("repdata%2Fdata%2FStormData.csv")) {
        if (!file.exists("repdata%2Fdata%2FStormData.csv.bz2")) {
                download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                              destfile = "repdata%2Fdata%2FStormData.csv.bz2", method = "curl")
        }
        bunzip2("repdata%2Fdata%2FStormData.csv.bz2", remove = FALSE)
}

## load data and event type list
storm <- read_csv("repdata%2Fdata%2FStormData.csv")
evtype_list <- read_table("evtype.txt", col_names = FALSE)[[1]]

## subset needed variables
storm <- select(storm, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

## clean data

### make variable names and evtype to lower case
storm <- mutate(storm, EVTYPE = tolower(EVTYPE))
evtype_list <- tolower(evtype_list)
names(storm) <- tolower(names(storm))

### rectify typos to ensure evtype is 48
storm$evtype <- evtype_list[amatch(storm$evtype, evtype_list, maxDist = 8)]
storm <- drop_na(storm, evtype)


###
storm$propdmgexp <- gsub("[Hh]", "2", storm$propdmgexp)
storm$propdmgexp <- gsub("[Kk]", "3", storm$propdmgexp)
storm$propdmgexp <- gsub("[Mm]", "6", storm$propdmgexp)
storm$propdmgexp <- gsub("[Bb]", "9", storm$propdmgexp)
storm$propdmgexp <- gsub("\\+", "1", storm$propdmgexp)
storm$propdmgexp <- gsub("\\?|\\-|\\ ", "0", storm$propdmgexp)

storm$cropdmgexp <- gsub("[Hh]", "2", storm$cropdmgexp)
storm$cropdmgexp <- gsub("[Kk]", "3", storm$cropdmgexp)
storm$cropdmgexp <- gsub("[Mm]", "6", storm$cropdmgexp)
storm$cropdmgexp <- gsub("[Bb]", "9", storm$cropdmgexp)
storm$cropdmgexp <- gsub("\\+", "1", storm$cropdmgexp)
storm$cropdmgexp <- gsub("\\?|\\-|\\ ", "0", storm$cropdmgexp)

storm$propdmgexp <- as.numeric(storm$propdmgexp)
storm$cropdmgexp <- as.numeric(storm$cropdmgexp)

storm$propdmgexp[is.na(storm$propdmgexp)] <- 0
storm$cropdmgexp[is.na(storm$cropdmgexp)] <- 0



## population health damage
storm_to_health <- storm %>%
        select(evtype, fatalities, injuries) %>%
        group_by(evtype) %>%
        summarise(toll = sum(fatalities + injuries)) %>%
        arrange(desc(toll))

## economic damage
storm_to_economy <- storm %>% 
        select(evtype, propdmg, propdmgexp, cropdmg, cropdmgexp) %>%
        mutate(propdmg_num = propdmg * (10 ^ propdmgexp), 
               cropdmg_num = cropdmg * (10 ^ cropdmgexp)) %>%
        group_by(evtype) %>%
        summarise(total_damage = sum(propdmg_num + cropdmg_num)) %>%
        arrange(desc(total_damage))


## plot
ggplot(storm_to_health[1:10,], aes(reorder(evtype, toll), toll)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme(plot.margin = unit(rep(2,4), "lines")) + 
        labs(title = "Top 10 Weather Events Causing Death and Injury") +
        labs(x = "Event Type", y = "Toll")

ggplot(storm_to_economy[1:10,], aes(reorder(evtype, total_damage), total_damage)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_y_continuous(labels = scales::dollar) +
        theme(plot.margin = unit(rep(2,4), "lines")) + 
        labs(title = "Top 10 Weather Events Causing Economic Damage") +
        labs(x = "Event Type", y = "Economic Damage")
          


        





