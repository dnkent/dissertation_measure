# World Economics and Politics Datasets
# Preferential trade agreements and bilateral investment treaties

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

#------------
# Data import
#------------

# WEP -- large because of country-years that don't actually exist
wep <- rio::import(
    "https://www.dropbox.com/s/t7nj1ntetdv2vyu/wep_dyad.rds?dl=1"
    )

# Nodes
states <- read_csv("https://www.dropbox.com/s/uza5c5d7d3qoym7/states2016.csv?dl=1")


# What should the states actually look like?
states_list <- tibble(
    stateabb = as.character(),
    ccode = as.integer(),
    statenme = as.character(),
    year = as.integer()
)

years <- c(1945:2016)

# All states 1945-2016
states_list <- map_df(seq_along(1:length(years)), function(x){
    time <- years[x] # select year
    temp <- filter(
        states, styear <= time & endyear >= time # states exist at that year
    ) %>% select( # vars to keep
        stateabb, # abbreviation
        ccode,    # country code
        statenme  # full name
    ) %>%
    mutate(
        year = time # create year variable
    )
    states_list <- bind_rows(states_list, temp) # append rows
})

# Now let's filter wep if repccode and parccode and year are all in states_list
wep %<>%
    filter(
        repccode %in% states_list$ccode & year %in% states_list$year
    ) %>% 
    filter(
        parccode %in% states_list$ccode & year %in% states_list$year
    ) %>% 
    select(-c(
        repcountry,
        parcountry,
        repifs,
        parifs,
        repgwno,
        pargwno,
        repifscode,
        parifscode,
        inforce_BIT # bit data is a mess
    ))


# Bilateral labor agreements 1946-2015
years <- c(1946:2015)

bla_net <- list()

bla_net <- map(seq_along(years), function(i){

    # Specify the year
    time <- i + 1945

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
            wep, 
            year == time
        ) %>%
        select(c(
                repgwabbrev,
                pargwabbrev,
                treatyruntotalN_BLA
        )) %>%
        rename(
            statea = "repgwabbrev",
            stateb = "pargwabbrev",
            bla = "treatyruntotalN_BLA"
        ) %>%
        na.omit() %>%
        filter(
            bla > 0
        )

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
    net %<>% # fun magrittr trick, don't need net <- net %>%
        set_graph_attr(
            "year", # name of the attr
            time # value
        )

    # store the network
    bla_net[[i]] <- net
})


## network and igraph versions
network_bla <- list() # empty list for storage
network_bla <- map(seq_along(bla_net), function(x){
    network_bla[[x]] <- asNetwork(bla_net[[x]]) # intergraph
})

igraph_bla <- list() # empty list for storage
igraph_bla <- map(seq_along(bla_net), function(x){
    igraph_bla[[x]] <- as.igraph(bla_net[[x]]) # igraph
})

## Save network files
rio::export(network_bla, "../r_networks/bla_network.rds")
rio::export(bla_net, "../r_networks/bla_tidy.rds")


# Preferential trade agreements 1948-2015
years <- c(1948:2015)

pta_net <- list()

pta_net <- map(seq_along(years), function(i){

    # Specify the year
    time <- i + 1945

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
            wep,
            year == time
        ) %>%
        select(c(
                repgwabbrev,
                pargwabbrev,
                inforce_PTA
        )) %>%
        rename(
            statea = "repgwabbrev",
            stateb = "pargwabbrev",
            pta = "inforce_PTA"
        ) %>%
        na.omit() %>%
        filter(
            pta > 0
        )

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

    # store the network
    pta_net[[i]] <- net
})


## network and igraph versions
network_pta <- list() # empty list for storage
network_pta <- map(seq_along(pta_net), function(x){
    network_pta[[x]] <- asNetwork(pta_net[[x]]) # intergraph
})

igraph_pta <- list() # empty list for storage
igraph_pta <- map(seq_along(pta_net), function(x){
    igraph_pta[[x]] <- as.igraph(pta_net[[x]]) # igraph
})

## Save network files
rio::export(network_pta, "../r_networks/pta_network.rds")
rio::export(pta_net, "../r_networks/pta_tidy.rds")