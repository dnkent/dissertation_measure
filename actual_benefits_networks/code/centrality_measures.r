# Calculate each state's centrality for all network-years then store the rank
# Unit of observation is the country-year, variables are for each network

# libraries
library(dplyr)
library(readr)
library(network)
library(tidygraph)
library(ggraph)
library(igraph)
library(intergraph)

# Import networks
alliance_tidy <- import("../r_networks/alliance_tidy.rds")
alliance <- import("../r_networks/alliance_network.rds")
arms_tidy <- import("../r_networks/arms_tidy.rds")
arms <- import("../r_networks/arms_network.rds")
trade_total <- import("../r_networks/trade_total.rds")
diplomacy_tidy <- import("../r_networks/dipl_tidy.rds")
diplomacy <- import("../r_networks/dipl_network.rds")

# Create empty dataset
states <- read_csv(
    "https://www.dropbox.com/s/uza5c5d7d3qoym7/states2016.csv?dl=1"
    )

# Extend to 1815 so matches with atop
states$styear <- ifelse(
    states$styear == 1816, ## If starts in 1816 (start of dataset)
    1815, ## Then recode as 1815 (start of atop)
    states$styear ## Or keep as is
)

# Start master dataset for storing all centrality
network_centrality <- tibble(
    stateabb = as.character(),
    year = as.integer(),
)

years <- tibble(
    years = as.integer(c(1815:2016))
)

# Populate country-year combinations
for (i in 1:nrow(years)){
    time <- years$years[i]
    # Nodes
    states_temp <- filter(
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
        year = as.integer(rep(time, length(states_temp)))
    )

    # now append to existing data
    network_centrality <- rbind(network_centrality, temp)
}

# Calculate pagerank centrality for each state-year and store
# Alliances
alliance_centrality <- tibble(
    stateabb = as.character(),
    alliance_centrality = as.numeric(),
    year = as.integer()
)

for (i in 1:length(alliance_tidy)){
    temp <- alliance_tidy[[i]]
    ## Calculate pagerank centrality
    temp <- temp %>%
        mutate(centrality = centrality_pagerank())
    ## Turn into tibble to access data
    temp <- as_tibble(temp)
    ## Remove unecessary column
    temp <- temp %>% select(c(name, centrality))
    ## Create year variable
    year <- get.network.attribute(alliance[[i]], "year")
    ## Store the year for merging
    temp$year <- year
    ## Make sure column names are the same
    colnames(temp) <- c("stateabb", "alliance_centrality", "year")
    ## Create ranking variable
    temp <- temp %>% mutate(
            alliance_rank = dense_rank(desc(alliance_centrality))
        ) 
    ## And append
    alliance_centrality <- rbind(alliance_centrality, temp)
}


# left join to merge
network_centrality <- left_join(
    network_centrality,
    alliance_centrality,
    by = c("stateabb", "year")
)

# A few countries I missed
network_centrality <- network_centrality %>%
    mutate(
        alliance_centrality = ifelse(
            is.na(alliance_centrality), ## if an na
            0, ## then recode as 0
            alliance_centrality ## otherwise keep as is
        )
    )

# Fix rank
network_centrality <- network_centrality %>%
    group_by(year) %>%
    mutate(
        alliance_rank = dense_rank(desc(alliance_centrality))
    )

# Trade
trade_total <- trade_total %>%
    group_by(year) %>%
    mutate(
        trade_percent = trade_agg / sum(trade_agg)
    )

network_centrality <- left_join(
    network_centrality,
    trade_total,
    by = c("stateabb", "year")
    )

# Diplomacy
diplomacy_centrality <- tibble(
    stateabb = as.character(),
    diplomacy_centrality = as.numeric(),
    year = as.integer()
)

for (i in 1:38){
    temp <- diplomacy_tidy[[i]]
    ## Calculate pagerank centrality
    temp <- temp %>%
        mutate(centrality = centrality_pagerank())
    ## Turn into tibble to access data
    temp <- as_tibble(temp)
    ## Remove unecessary column
    temp <- temp %>% select(c(name, centrality))
    ## Create year variable
    year <- get.network.attribute(diplomacy[[i]], "year")
    ## Store the year for merging
    temp$year <- year
    ## Make sure column names are the same
    colnames(temp) <- c("stateabb", "diplomacy_centrality", "year")
    ## Create ranking variable
    temp <- temp %>% mutate(
        diplomacy_rank = dense_rank(desc(diplomacy_centrality))
        )
    ## And append
    diplomacy_centrality <- rbind(diplomacy_centrality, temp)
}


for (i in 39:40){
    temp <- diplomacy_tidy[[i]]
    ## Calculate pagerank centrality
    temp <- temp %>%
        mutate(centrality = centrality_pagerank())
    ## Turn into tibble to access data
    temp <- as_tibble(temp)
    ## Remove unecessary column
    temp <- temp %>% select(c(vertex.names, centrality))
    ## Create year variable
    year <- get.network.attribute(diplomacy[[i]], "year")
    ## Store the year for merging
    temp$year <- year
    ## Make sure column names are the same
    colnames(temp) <- c("stateabb", "diplomacy_centrality", "year")
    ## Create ranking variable
    temp <- temp %>% mutate(
        diplomacy_rank = dense_rank(desc(diplomacy_centrality))
        )
    ## And append
    diplomacy_centrality <- rbind(diplomacy_centrality, temp)
}

network_centrality <- left_join(
    network_centrality,
    diplomacy_centrality,
    by = c("stateabb", "year")
    )


# Arms trade
arms_centrality <- tibble(
    stateabb = as.character(),
    arms_centrality = as.numeric(),
    year = as.integer()
)

for (i in 1:length(arms_tidy)){
    temp <- arms_tidy[[i]]
    ## Calculate pagerank centrality
    temp <- temp %>%
        mutate(centrality = centrality_pagerank())
    #temp <- temp %>%
    #    mutate(centrality = centrality_eigen())
    ## Turn into tibble to access data
    temp <- as_tibble(temp)
    ## Remove unecessary column
    temp <- temp %>% select(c(name, centrality))
    ## Create year variable
    year <- get.network.attribute(arms[[i]], "year")
    ## Store the year for merging
    temp$year <- year
    ## Make sure column names are the same
    colnames(temp) <- c("stateabb", "arms_centrality", "year")
    ## Create ranking variable
    temp <- temp %>% mutate(
        arms_rank = dense_rank(desc(arms_centrality))
        )
    ## And append
    arms_centrality <- rbind(arms_centrality, temp)
}

network_centrality <- left_join(
    network_centrality,
    arms_centrality,
    by = c("stateabb", "year")
)


## Export
export(network_centrality, "../r_networks/centrality_rankings.rds")