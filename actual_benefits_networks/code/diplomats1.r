# diplomat networks 1817-2005, not every year 

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

#---------------------
# Data import and prep
#---------------------

# Diplomat tie data
dipl <- read_csv(
    "https://www.dropbox.com/s/rjktda8921buvd1/cow_diplomatic_exchange2006.csv?dl=1"
    )

# Nodes
states <- read_csv(
    "https://www.dropbox.com/s/uza5c5d7d3qoym7/states2016.csv?dl=1"
    )


# This code is only so that labels for atop can be
# stateabb instead of ccode
# Remove duplicate obs in state for cleaning
states_no_dup <- states %>%
    distinct(stateabb, .keep_all = TRUE)

ccode1 <- dplyr::select(dipl, ccode1) %>%
    rename(
        ccode = ccode1
    ) %>%
    left_join(states_no_dup) ## join with states

dipl$ccode1 <- ccode1$stateabb

ccode2 <- dplyr::select(dipl, ccode2) %>%
    rename(
        ccode = ccode2
    ) %>%
    left_join(states_no_dup) ## join with states

dipl$ccode2 <- ccode2$stateabb


## Convert to directed
el1 <- dplyr::filter(
    dipl,
    DR_at_2 > 0
    ) %>%
    select(
        c(
            "ccode1",
            "ccode2",
            "year"
            )
    ) %>%
    rename(
        mem1 = ccode1,
        mem2 = ccode2
    )


el2 <- dplyr::filter(
    dipl,
    DR_at_1 > 0
    ) %>% 
    mutate(
        mem1 = ccode2,
        mem2 = ccode1
    ) %>%
    select(
        c(
            "mem1",
            "mem2",
            "year"
        )
    )


edgelist <- rbind(el1, el2)

years <- sort(unique(edgelist$year))

# List of empty networks
dipl_net <- list()

#-------------------
# Build the networks 
#-------------------
dipl_net <- map(seq_along(years), function(i){
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
    # Edgelist
    edges <- dplyr::filter(
        edgelist,
        year == time,
    ) %>%
    select(
        -year
    ) %>%
    filter(
        mem1 %in% nodes$stateabb & mem2 %in% nodes$stateabb
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
    dipl_net[[i]] <- net
})

## network and igraph versions
network_dipl <- list() # empty list for storage
network_dipl <- map(seq_along(dipl_net), function(x){
    network_dipl[[x]] <- asNetwork(dipl_net[[x]]) # intergraph
})

igraph_dipl <- list() # empty list for storage
igraph_dipl <- map(seq_along(dipl_net), function(x){
    igraph_dipl[[x]] <- as.igraph(dipl_net[[x]]) # igraph
})

## Save network files
export(network_dipl, "../r_networks/dipl_network.rds")
export(dipl_net, "../r_networks/dipl_tidy.rds")