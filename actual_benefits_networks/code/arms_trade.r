# Code for building the defense cooperation agreement network

# libraries
library(tidyr)
library(dplyr)
library(readr)
library(network)
library(tidygraph)
library(ggraph)
library(igraph)
library(intergraph)
library(magrittr)
library(purrr)
library(countrycode)

#------------
# Data import
#------------

# Arms trade data
arms <- read_csv("https://www.dropbox.com/s/j83cr0bvc5k7pmn/sipri.csv?dl=1")

# Gather into tidy format and replace NA with 0
arms %<>%
    gather(
        key = year, # renaming columns to a year variable
        value = amount_transferred, # variable name
        3:69 # column numbers to be gathering (here all years)
    ) %>%
    drop_na() ## drop NAs, if recoded as 0, then treated as a tie

# Nodes
states <- read_csv("https://www.dropbox.com/s/uza5c5d7d3qoym7/states2016.csv?dl=1")

arms$recip <- countrycode(
    arms$recipient,
    origin = "country.name",
    destination = "iso3c"
)

arms$send <- countrycode(
    arms$sender,
    origin = "country.name",
    destination = "iso3c"
)

# Czechoslovakia
arms %<>%
    mutate(
        recip = ifelse(
            recipient == "Czechoslovakia",
            "CZE",
            recip
        )
    ) %>%
    mutate(
        send = ifelse(
            sender == "Czechoslovakia",
            "CZE",
            send
        )
    )

# East Germany (GDR)
arms %<>%
    mutate(
        recip = ifelse(
            recipient == "East Germany (GDR)",
            "GDR",
            recip
        )
    ) %>%
    mutate(
        send = ifelse(
            sender == "East Germany (GDR)",
            "GDR",
            send
        )
    )

# North Yemen -- Yemen Arab Republic
arms %<>%
    mutate(
        recip = ifelse(
            recipient == "North Yemen",
            "YAR",
            recip
        )
    ) %>%
    mutate(
        send = ifelse(
            sender == "North Yemen",
            "YAR",
            send
        )
    )

# Yugoslavia
arms %<>%
    mutate(
        recip = ifelse(
            recipient == "Yugoslavia",
            "YUG",
            recip
        )
    ) %>%
    mutate(
        send = ifelse(
            sender == "Yugoslavia",
            "YUG",
            send
        )
    )

# NATO, drop
arms %<>%
    filter(
        recipient != "NATO**" &
        sender != "NATO**"
    )

# Unknown recipient(s) -- drop
arms %<>%
    filter(
        recipient != "Unknown recipient(s)"
    )

# Unknown sender(s) -- drop
arms %<>%
    filter(
        sender != "Unknown supplier(s)"
    )

# Katanga -- part of Congo crisis, recode as Congo
arms %<>%
    mutate(
        recip = ifelse(
            recipient == "Katanga",
            "DRC",
            recip
        )
    )

# UN drop
arms %<>%
    filter(
        recipient != "United Nations**" &
        sender != "United Nations**"
    )

# Biafra, Nigeria
arms %<>%
    mutate(
        recip = ifelse(
            recipient == "Biafra",
            "NIG",
            recip
        )
    )

# South Yemen -- Yemen People's Republic
arms %<>%
    mutate(
        recip = ifelse(
            recipient == "South Yemen",
            "YPR",
            recip
        )
    ) %>%
    mutate(
        send = ifelse(
            sender == "South Yemen",
            "YPR",
            send
        )
    )

# Lebanon Palestinian rebels* -- Lebanon
arms %<>%
    mutate(
        recip = ifelse(
            recipient == "Lebanon Palestinian rebels*",
            "LEB",
            recip
        )
    )

# Provisional IRA (UK)* -- UK
arms %<>%
    mutate(
        recip = ifelse(
            recipient == "Provisional IRA (UK)*",
            "UK",
            recip
        )
    )

# Micronesia, drop
arms %<>%
    filter(
        recipient != "Micronesia"
    )

# Southern rebels (Yemen)*
arms %<>%
    mutate(
        recip = ifelse(
            recipient == "Southern rebels (Yemen)*",
            "YPR",
            recip
        )
    )

# Regional Security System**, drop
arms %<>%
    filter(
        recipient != "Regional Security System**"
    )

# African Union**, drop
arms %<>%
    filter(
        recipient != "African Union**"
    )

# Kosovo
arms %<>%
    mutate(
        recip = ifelse(
            recipient == "Kosovo",
            "KOS",
            recip
        )
    )

# OSCE**, drop
arms %<>%
    filter(
        recipient != "OSCE**"
    )

# PRC (Israel/Palestine)*, drop
arms %<>%
    filter(
        recipient != "PRC (Israel/Palestine)*"
    )

# And keep useful columns:
arms %<>%
    select(
        c(
            recip,
            send,
            year,
            amount_transferred
        )
    )

#--------------------------
# Initialize empty networks
#--------------------------
# years of dca data, will be a network-level var
years <- c(1950:2016)

# List of empty networks
arms_net <- list()

arms_net <- map(seq_along(years), function(i){

    # Select year of interest
    time <- i + 1949 ## 1 less than first year
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

    # Edgelist, include transfers -- they automatically populate next
    edges <- dplyr::filter(
        arms,
        year == time
    ) %>%
    select(
        c(
            recip,
            send,
            amount_transferred
        )
    )
    # Initialize the network with only the edgelist
    net <- tbl_graph(
        edges = edges,
        directed = TRUE
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
    # add_vertices is an igraph function and turns our tidygraph into an 
    # igraph object, so let's convert back to a tidygraph
    net <- as_tbl_graph(net)

    # Set year as a graph attribute
    net %<>% # fun magrittr trick, don't need net <- net %>%
        set_graph_attr(
            "year", # name of the attr
            time # value
        )

    # store the network
    arms_net[[i]] <- net
})

## network and igraph versions
network_arms <- list() # empty list for storage
network_arms <- map(seq_along(arms_net), function(x){
    network_arms[[x]] <- asNetwork(arms_net[[x]]) # intergraph
})

igraph_arms <- list() # empty list for storage
igraph_arms <- map(seq_along(arms_net), function(x){
    igraph_arms[[x]] <- as.igraph(arms_net[[x]]) # igraph
})

## Save network files
export(network_arms, "../r_networks/arms_network.rds")
export(arms_net, "../r_networks/arms_tidy.rds")