# libraries
library(dplyr)
library(readr)
library(countrycode)

#------------------------------
# Kenkel and Carroll Estimates
#------------------------------

# Download the data with predicted win percentages for each dyad
doe <- readRDS(
    "../replication_extension/results/small_predictions_automl_year.Rds"
    )

# Load states
states <- read_csv("states2016.csv")

# Remove duplicate obs in state for cleaning
states_no_dup <- states %>%
    distinct(stateabb, .keep_all = TRUE)

states_temp <- states_no_dup %>% select(
    ccode, stateabb
)
colnames(states_temp) <- c("ccode_a", "stateabb_a")
doe <- left_join(doe, states_temp, by = "ccode_a")

colnames(states_temp) <- c("ccode_b", "stateabb_b")
doe <- left_join(doe, states_temp, by = "ccode_b")

# reorder
doe <- doe %>% select(
    c(
        year,
        stateabb_a,
        stateabb_b,
        VictoryA,
        VictoryB
    )
)

# Start master dataset for storing all centrality
doe_ranks <- tibble(
    stateabb = as.character(),
    year = as.integer(),
    percent_win = as.numeric()
)

years <- tibble(
    years = as.integer(c(1816:2012))
)

# Populate country-year combinations
for (i in 1:nrow(years)){
    time <- years$years[i]
    # States at that time
    states_temp <- dplyr::filter(
        # State was formed during or before year
        # and state ended after or during year
        states,
        styear <= time & endyear >= time
        ) %>%
        select(
            stateabb
        )

    # temp tibble for storing this year's countries
    temp <- tibble(
        stateabb = as.character(states_temp$stateabb),
        year = as.integer(rep(time, length(states_temp))),
        percent_win = rep(0, length(states_temp))
    )

    # now append to existing data
    doe_ranks <- rbind(doe_ranks, temp)
}

ranks_final <- tibble(
    stateabb = as.character(),
    year = as.integer(),
    percent_win = as.numeric()
)

# Aggregate probabilities of winning for each state
# This takes a few minutes
for (i in 1:nrow(years)){
    time <- years$years[i]

    # DOE scores for that year
    doe_temp <- dplyr::filter(
        doe, year == time
    )

    # States for that year
    states_temp <- dplyr::filter(
        doe_ranks, year == time
    )

    # Calculate each state's expected wins and losses
    for (j in 1:nrow(states_temp)){
        state_of_interest <- states_temp[j,1]$stateabb
        scores_temp <- doe_temp %>% filter(
            stateabb_a == state_of_interest |
            stateabb_b == state_of_interest
        )

        percents <- 0
        for(w in 1:nrow(scores_temp)){
            if(scores_temp[w, 2] == state_of_interest){
                percents <- percents + scores_temp[w, 4]
            }else{
                percents <- percents + scores_temp[w, 5]
            }
        }
        percents <- percents / nrow(scores_temp)

        states_temp[j, 3] <- percents
        # Join to ranks datasetstates_temp[j, 4]
        ranks_final <- rbind(ranks_final, states_temp[j, ])
    }
}

# Looks good?
head(ranks_final)
dim(ranks_final)

# Most powerful? Produce a ranking variable
ranks_final2 <- tibble(
    stateabb = as.character(),
    year = as.integer(),
    rank = as.integer()
)

# Rank with mutate and dense_rank
ranks_final <- ranks_final %>%
    group_by(year) %>%
    mutate(
        rank = dense_rank(desc(percent_win))
    )

# Save final rankings
export(ranks_final, "ranks_final.rds")