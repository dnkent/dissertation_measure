## This script is only for adding 2008 and 2010 diplomacy networks

# libraries
library(dplyr)
library(readr)
library(network)
library(tidygraph)
library(ggraph)
library(igraph)
library(intergraph)

#-----------
# Nodes Load
#-----------

states <- read_csv(
    "https://www.dropbox.com/s/uza5c5d7d3qoym7/states2016.csv?dl=1"
    )

#-----------
# Edges Load
#-----------
# This is messier than others because the data is already in matrix form
# Also the files import as different classes
matrix_list <- list()

# 2008
dipcon2008 <- read.csv(
    "https://www.dropbox.com/s/6no22x754uun6t7/DIPCON2008_3.0.csv?dl=1",
    row.names = 1
    )

matrix_list[[1]] <- dipcon2008

# 2010
dipcon2010 <- read.csv(
    "https://www.dropbox.com/s/x0gsdciidog7dmy/DIPCON2010_3.0.csv?dl=1",
    row.names = 1
    )

matrix_list[[2]] <- dipcon2010

years <- c(2008, 2010)

# List of empty networks
dipl_net <- vector("list", length = length(years))

# Build the networks
dipl_net <- lapply(seq_along(years), function(i){
    # Select year of interest
    time <- years[i]
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
    # Build using adjacency, since already in adjacency form
    # Remove locations not in the states list
    sociomatrix <- matrix_list[[i]]
    cols_keep <- colnames(sociomatrix) %in% nodes$stateabb
    rows_keep <- rownames(sociomatrix) %in% nodes$stateabb
    sociomatrix <- sociomatrix[rows_keep, cols_keep]

    # replace . with 0
    for (j in 1:nrow(sociomatrix)){
        for (k in 1:ncol(sociomatrix)){
            if (sociomatrix[j, k] == "."){
                sociomatrix[j, k] <- 0
            }
        }
    }

    # Network build
    net <- as.network(
        sociomatrix,
        matrix.type = "adjacency",
        loops = FALSE,
        directed = TRUE
        )

    # Add nodes not in matrix:
    missing <- setdiff(
        nodes$stateabb, # all nodes
        network::get.vertex.attribute(net, "name") # in network
        )

    # adding more than one node at a time seems to cause issues
    # Don't run if no isolates:
    if (length(missing) > 0){
            for (j in 1:length(missing)){
            network::add.vertices(
                net,
                nv = 1,
                vattr = list(
                    list(
                        vertex.names = missing[j]
                    )
                )
            )
        }
    }

    # Set network-level attribute for year
    network::set.network.attribute(
        net,
        "year",
        time
        )

    # Store network
    dipl_net[[i]] <- net
})

## Tidy version
dipl_tidy <- vector("list", length = length(years))

dipl_tidy <- lapply(seq_along(years), function(i){
    net_igraph <- asIgraph(dipl_net[[i]])
    dipl_tidy[[i]] <- as_tbl_graph(net_igraph)
}
)

## Add to other network files
dipl_net <- import("../r_networks/dipl_tidy.rds")

## Add two more networks for 2008 and 2010
dipl_net[[39]] <- dipl_tidy[[1]]
dipl_net[[40]] <- dipl_tidy[[2]]

## network and igraph versions
network_dipl <- list() # empty list for storage
network_dipl <- purrr::map(seq_along(dipl_net), function(x){
    network_dipl[[x]] <- asNetwork(dipl_net[[x]]) # intergraph
})

## Save network files
export(network_dipl, "../r_networks/dipl_network.rds")
export(dipl_net, "../r_networks/dipl_tidy.rds")