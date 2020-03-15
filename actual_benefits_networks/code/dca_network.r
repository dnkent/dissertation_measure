# Code for building the defense cooperation agreement network

# libraries
library(dplyr)
library(readr)
library(network)
library(tidygraph)
library(ggraph)
library(igraph)
library(intergraph)
library(purrr)
library(magrittr)

#------------
# Data import
#------------

# DCA
dca <- rio::import(
    "https://www.dropbox.com/s/pzkqvl8epa7j9ek/DyadicDataset.rds?dl=1"
    )

# Nodes
states <- read_csv(
    "https://www.dropbox.com/s/uza5c5d7d3qoym7/states2016.csv?dl=1"
)


#--------------------------
# Initialize empty networks
#--------------------------
# years of dca data, will be a network-level var
years <- c(1980:2010)

# List of empty networks
dca_net <- list()

dca_net <- map(seq_along(years), function(i){

    # Select year of interest
    time <- i + 1979

    # Nodes
    nodes <- dplyr::filter(
        # State was formed during or before year
        # and state ended after or during year
        states,
        styear <= time & endyear >= time
        ) %>%
        select(
            stateabb
        )

    # Edgelist, avoiding other dummy variables for now
    edges <- dplyr::filter(
        dca,
        year == time & dcaAnyV1 == 1 # was a DCA signed at all?
    ) %>%
    select(
        c(
            abbrev1,
            abbrev2
            )
    )

    # Initialize the network with only the edgelist
    net <- tbl_graph(
        edges = edges,
        directed = FALSE
    )


    # Now let's define the isolates
    isolates <- setdiff(
        nodes$stateabb, # all nodes
        as_tibble(net)$name # in network
    )

    # Add the isolates
    net <- add_vertices(
        net,
        length(isolates),
        name = isolates
    )

    net <- as_tbl_graph(net)

    # Set year as a graph attribute
    net$year <- time

    dca_net[[i]] <- net
})


## network and igraph versions
network_dca <- list() # empty list for storage
network_dca <- map(seq_along(dca_net), function(x){
    network_dca[[x]] <- asNetwork(dca_net[[x]]) # intergraph
})

igraph_dca <- list() # empty list for storage
igraph_dca <- map(seq_along(dca_net), function(x){
    igraph_dca[[x]] <- as.igraph(dca_net[[x]]) # igraph
})

## Save network files
export(network_dca, "../r_networks/dca_network.rds")
export(dca_net, "../r_networks/dca_tidy.rds")