# Code for building interstate trade network (weighted)

# libraries
library(dplyr)
library(tidyr)
library(readr)
library(network)
library(tidygraph)
library(ggraph)
library(igraph)
library(intergraph)
library(magrittr)
library(purrr)

# Trade and gleditsch imputations
trade <- import(
    "https://www.dropbox.com/s/lplrgx52wtikgrr/Dyadic_COW_4.0.rds?dl=1"
    )

gleditsch <- import(
    "https://www.dropbox.com/s/09n0dll920n1f52/dk_trade.rds?dl=1"
    )

# Nodes
states <- read_csv("../data/states2016.csv")

#----------------
# Data clean/prep
#----------------
# Adjust so that labels are stateabb not ccode
# Remove duplicate obs in state for cleaning
states_no_dup <- states %>%
    distinct(stateabb, .keep_all = TRUE)

mem1 <- dplyr::select(trade, ccode1) %>%
    rename(
        ccode = ccode1
    ) %>%
    left_join(states_no_dup) ## join with states

trade$mem1 <- mem1$stateabb

mem2 <- dplyr::select(trade, ccode2) %>%
    rename(
        ccode = ccode2
    ) %>%
    left_join(states_no_dup) ## join with states

trade$mem2 <- mem2$stateabb

## Let's only include columns of interest
trade <- trade %>%
    select(
        c(
            mem1, # state a
            mem2, # state b
            year, # year
            smoothtotrade # total trade (Maoz smoothed)
        )
    ) %>%
    mutate( ## replace -9 with NA for total trade
        smoothtotrade = na_if(
            smoothtotrade, -9
        )
    )

## Gleditsch, only starts post-WWII
gleditsch <- gleditsch %>%
    select( # what variables to keep
        c(
            acra, # state a
            acrb, # state b
            year, # year
            expab, # exports from a to b (should equal imports b from a)
            impab # imports in a from b (should equal exports from b to a)
        )
    ) %>%
    rename(
        mem1 = acra,
        mem2 = acrb
    ) %>% 
    mutate(
        total_trade = expab + impab
    )

## Join the two datasets
final_trade <- left_join(
        trade, # trade data
        gleditsch # gleditsch imputations
    ) %>%
    mutate( # create variable to use gleditsch when available
        trade = case_when(
            is.na(total_trade) ~ smoothtotrade,
            TRUE ~ total_trade # TRUE is remaining condition
        )
    ) %>%
    select( # remove trade variables that aren't final
        -c(
            smoothtotrade,
            expab,
            impab,
            total_trade
        )
    ) %>% # because building an edgelist, let's drop the NA's 
    drop_na() %>%
    filter( ## remove ties with zero trade for edgelist
        trade > 0
    )

test <- final_trade

# The network is chaotic, so let's just aggregate the amount
# each country gets traded with
trade_summary <- tibble(
    stateabb = as.character(),
    year = as.integer(),
    trade = as.numeric(),
    trade_rank = as.integer()
)

years <- c(1870:2014)

for(i in 1:length(years)){
    time <- i + 1869
    # Nodes
    node <- dplyr::filter(
        # State was formed during or before year
        # and state ended after or during year
        states,
        styear <= time & endyear >= time
        ) %>%
        select(
            stateabb
        )
    # Year
    node$year <- time

    # Create trade count variable
    node$trade_agg <- 0

    # Edgelist
    edge <- dplyr::filter(
        final_trade,
        year == time,
    )
    # Summarize the amount of trade
    for(j in 1:nrow(node)){
        state <- node$stateabb[j]
        temp2 <- dplyr::filter(
            edge,
            mem1 == state | mem2 == state
        )
        # Fill in nodes
        node[j,3] <- sum(temp2$trade)
    }

    # Rank
    node <- node %>%
        mutate(
            trade_rank = rank(desc(trade_agg))
        )

    # Append
    trade_summary <- rbind(trade_summary, node)
}

#--------------------------
# Initialize empty networks
#--------------------------
# years of data, will be a network-level var
years <- c(1870:2014)

# List of empty networks
trade_net <- list()

trade_net <- map(seq_along(years), function(i){
    # Select year of interest
    time <- i + 1869
    # Nodes
    node <- dplyr::filter(
        # State was formed during or before year
        # and state ended after or during year
        states,
        styear <= time & endyear >= time
        ) %>%
        select(
            stateabb
        )
    # Edgelist
    edge <- dplyr::filter(
        final_trade,
        year == time,
    ) %>%
    select(-year)

    # Initialize the network with only the edgelist
    net <- tbl_graph(
        edges = edge,
        directed = FALSE
    )

    # Now let's define the isolates
    isolates <- setdiff(
        node$stateabb, # all nodes
        as_tibble(net)$name # in network
    )

    # Add the isolates if there are any
    net <- add_vertices(
        net,
        length(isolates),
        name = isolates
    )

    # add_vertices is an igraph function and turns our tidygraph into an 
    # igraph object, so let's convert back to a tidygraph
    net <- as_tbl_graph(net)

    # Set year as a graph attribute
    net %<>% # fun magrittr trick, don't need net <- net %>%
        set_graph_attr(
            "year", # name of the attr
            time # value
        )

    # tranform into igraph, easier for tidygraph
    trade_net[[i]] <- net
})

## network and igraph versions
network_trade <- list() # empty list for storage
network_trade <- map(seq_along(trade_net), function(x){
    network_trade[[x]] <- asNetwork(trade_net[[x]]) # intergraph
})

igraph_trade <- list() # empty list for storage
igraph_trade <- map(seq_along(trade_net), function(x){
    igraph_trade[[x]] <- as.igraph(trade_net[[x]]) # igraph
})

## Save network files
export(network_trade, "../r_networks/trade_network.rds")
export(trade_net, "../r_networks/trade_tidy.rds")
export(trade_summary, "../r_networks/trade_total.rds")