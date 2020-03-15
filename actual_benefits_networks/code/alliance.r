# Code for building the interstate alliance network 1815-2016

# libraries
library(dplyr)
library(readr)
library(network)
library(tidygraph)
library(ggraph)
library(igraph)
library(intergraph)
library(magrittr)
library(purrr)
library(repmis)

#------------
# Data import
#------------

# Alliances
atop <- read_csv(
    "https://www.dropbox.com/s/63xzl890mupb68g/atop4_01dy.csv?dl=1"
    ) # note changed dl=0 to dl=1 for dropbox share link

# Nodes
states <- read_csv("https://www.dropbox.com/s/uza5c5d7d3qoym7/states2016.csv?dl=1")

#----------------
# Data clean/prep
#----------------
# Fix start year for node list
# Extend to 1815 so matches with atop
states$styear <- ifelse(
    states$styear == 1816, ## If starts in 1816 (start of dataset)
    1815, ## Then recode as 1815 (start of atop)
    states$styear ## Or keep as is
)

# This code is only so that labels for atop can be
# stateabb instead of ccode
# Remove duplicate obs in state for cleaning
states_no_dup <- states %>%
    distinct(stateabb, .keep_all = TRUE)

mem1 <- dplyr::select(atop, mem1) %>%
    rename(
        ccode = mem1
    ) %>% 
    left_join(states_no_dup) ## join with states

atop$mem1 <- mem1$stateabb

mem2 <- dplyr::select(atop, mem2) %>%
    rename(
        ccode = mem2
    ) %>% 
    left_join(states_no_dup) ## join with states

atop$mem2 <- mem2$stateabb

#--------------------------
# Initialize empty networks
#--------------------------

# years of alliance data, will be a network-level var
years <- c(1815:2016)

# List of empty networks
ally_net <- list()

#-------------------
# Build the networks 
#-------------------
ally_net <- map(seq_along(years), function(i){

    # Specify the year
    time <- i + 1814

    # Nodes: which states exist during year i?
    node <- filter(
            states, 
            styear <= time & endyear >= time
        ) %>%
        select(
            stateabb
        )

    # Edgelist
    edge <- filter(
            atop, 
            year == time
        ) %>%
        select(c(
            mem1, # member 1
            mem2, # member 2
            defense, # defensive alliance?
            offense, # offensive alliance?
            neutral, # neutrality pact?
            nonagg, # nonaggression pact?
            consul # consultation
        ))

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

    # Add the isolates
    net <- add_vertices(
        net, 
        length(isolates),
        name = isolates
    )

    # add_vertices is an igraph function and turns our tidygraph into an 
    # igraph object, so let's convert back to a tidygraph
    net <- as_tbl_graph(net)

    # Set year as a graph attribute
    net$year <- time

    # store the network
    ally_net[[i]] <- net
})

## network and igraph versions
network_alliances <- list() # empty list for storage
network_alliances <- lapply(seq_along(ally_net), function(x){
    network_alliances[[x]] <- asNetwork(ally_net[[x]]) # intergraph
})

igraph_alliances <- list() # empty list for storage
igraph_alliances <- lapply(seq_along(ally_net), function(x){
    igraph_alliances[[x]] <- as.igraph(ally_net[[x]]) # igraph
})

## Save network files
export(network_alliances, "../r_networks/alliance_network.rds")
export(ally_net, "../r_networks/alliance_tidy.rds")