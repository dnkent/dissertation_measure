#----------------------------------------------------#
# Script to build a dataframe with the final measure #
# and some useful controls                           #
#----------------------------------------------------#

library(dplyr)
library(tidyr)
library(rio)
library(countrycode)
library(readr)

## import the files
centrality <- import(
    "../actual_benefits_networks/r_networks/centrality_rankings.rds"
    )
doe <- import("../expected_benefits_doe/doe_rankings/ranks_final.rds")
cinc <- import("../expected_benefits_cinc/cinc_ranks.rds")
colnames(cinc)[3] <- "cinc_rank"

# Alliance, trade, diplomacy, arms trade
# IGOs have little variation in aggregation over time
centrality$benefits <- rowMeans(
    centrality[1:nrow(centrality), c(3, 7, 8, 12)],
    na.rm = TRUE
)

# Join
full_dat <- left_join(centrality, cinc, by = c("stateabb", "year"))
full_dat <- left_join(full_dat, doe, by = c("stateabb", "year"))

# power
full_dat$expectations <- full_dat$cinc * full_dat$percent_win

# Smooth
full_dat$smooth_benefits <- NA
full_dat$smooth_benefits_sd <- NA
full_dat$smooth_expectations <- NA
full_dat$smooth_expectations_sd <- NA
smoothed_dat <- full_dat[0, ] # empty tibble with columns

# For-loop for now
# Point est for smooth
# Bootstrap se for smooth
for(t in 1815:2016){
    if(t < 1818){
        temp <- full_dat %>% filter(year <= t)
    }else{
        temp <- full_dat %>% filter(year <= t & year > t - 4)
    }
    id_year <- full_dat %>% filter(year == t) # country list changing

    # creates issues otherwise
    for(k in 1:length(unique(id_year$stateabb))){
        temp2 <- temp %>% filter(stateabb == unique(id_year$stateabb)[k])
        # Benefits
        temp2[nrow(temp2), 20] <- mean(temp2$benefits, na.rm = TRUE) # point est
        ben_ests <- rep(0, nrow((temp2))) # Prep for bootstrap

        # Expectations
        temp2[nrow(temp2), 22] <- mean(temp2$expectations, na.rm = TRUE)
        exp_ests <- rep(0, nrow((temp2))) # Prep for bootstrap

        # bootstrap uncertainty
        for(i in 1:nrow(temp2)){
            temp3 <- temp2[-i,]
            ben_ests[i] <- mean(temp3$benefits, na.rm = TRUE)
            exp_ests[i] <- mean(temp3$expectations, na.rm = TRUE)
        }
        if(length(na.omit(ben_ests) > 1)){
            sd_ben <- sqrt(
                sum(
                    (ben_ests - mean(temp2$benefits, na.rm = TRUE))^2,
                    na.rm = TRUE
                ) / 
                length(ben_ests)
            )
        }else{
            sd_ben <- 0
        }
        if(length(na.omit(exp_ests)) > 1){
            sd_exp <- sqrt(
                sum(
                    (exp_ests - mean(temp2$expectations, na.rm = TRUE))^2,
                    na.rm = TRUE
                ) / 
                length(exp_ests)
            )
        }else{
            sd_exp <- 0
        }

        temp2[nrow(temp2), 21] <- sd_ben # sd
        temp2[nrow(temp2), 23] <- sd_exp # sd

        # fill in
        smoothed_dat <- rbind(smoothed_dat, temp2[nrow(temp2),]) # only last ob
    }
}

smoothed_dat <- smoothed_dat %>%
    mutate(
        smooth_expectations_sd = ifelse(
            is.na(smooth_expectations),
            NA,
            smooth_expectations_sd
        )
    )

# Update
full_dat <- smoothed_dat

# Measure
# Log to approximate normal distributions
full_dat$dissatisfaction <- log(full_dat$smooth_expectations) -
    log(full_dat$smooth_benefits)

# For parsimony, drop network-specific columns we don't revisit
full_dat <- full_dat %>%
    select(-c(3:13)) %>%
    filter(
        year > 1815
    )

## MID onset?
mids <- read_csv("gml-midb-2.1.csv")

## Only keep cases where reciprocated (more than two members)
mids2 <- mids %>%
    group_by(dispnum) %>%
    add_tally() %>%
    filter(
        n >= 2 & orig == 1 # more than two in the mid (reciprocated)
    ) %>%
    ungroup() %>% # ungroup
    select(
        c(stabb, styear)
    ) %>%
    rename(
        stateabb = stabb,
        year = styear
    )

## Count
mids2 <- mids2 %>%
    group_by(stateabb, year) %>%
    tally()

colnames(mids2)[3] <- "number_onset"

## Any onset
mids2$onset <- 1

## Merge the datasets
final_dat <- dplyr::left_join(
    full_dat,
    mids2,
    by = c("stateabb", "year")
)

final_dat <- final_dat %>%
    mutate(onset = replace_na(onset, 0))

final_dat <- final_dat %>%
    mutate(number_onset = replace_na(number_onset, 0))

## Fatal?
fatal <- mids %>%
    filter(
        orig == 1 & fatality >= 1 ## originator and war
    ) %>%
    select(
        c(stabb, styear)
    ) %>%
    rename(
        stateabb = stabb,
        year = styear
    )

## Count
fatal$fatal_onset <- 1

## Merge the datasets
final_dat <- dplyr::left_join(
    final_dat,
    fatal,
    by = c("stateabb", "year")
)

final_dat <- final_dat %>%
    mutate(fatal_onset = replace_na(fatal_onset, 0)) %>%
    ungroup()

# Ideal point
ideal <- rio::import("control_data/idealpoints.RData")
ideal <- ideal %>%
    select(c(countryabb, year, idealpoint)) %>%
    rename(stateabb = countryabb)
final_dat <- left_join(final_dat, ideal)

# Control variables
# polity score
polity <- readRDS("control_data/polity.Rds")
polity <- polity %>%
    select(c(scode, year, polity)) %>%
    rename(stateabb = scode)
final_dat <- left_join(final_dat, polity)

# population as a model control
nmc <- read_csv(file = "../expected_benefits_cinc/NMC_5_0.csv")
nmc <- nmc %>%
    mutate(
        stateabb = countrycode(ccode, origin = "cown", destination = "cowc")
    ) %>%
    select(c(
        stateabb,
        year,
        tpop
    ))
final_dat <- left_join(final_dat, nmc)

# Prep for fixed effects: Drop repeats on ID and ungroup
final_dat <- final_dat %>%
    mutate(id = paste(stateabb, ",", year)) %>% # create id for duplicates
    filter(!duplicated(id)) %>% # remove duplicate obs
    select(-id) %>% # drop id
    ungroup() # ungroup

# Save for subsequent use
saveRDS(final_dat, file = "final_measure.rds")