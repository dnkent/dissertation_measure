library(tidyverse)
library(tidygraph)
library(rio)
library(hrbrthemes)
library(ggraph)

alliances <- import("../actual_benefits_networks/r_networks/alliance_tidy.rds")

# Alliance network before WWUI -- 1938
ally_net <- alliances[[124]] %>%
    mutate(
        cent = centrality_pagerank(),
        ties = centrality_degree()
    ) %>%
    filter(
        ties > 0 # remove isolates
    )


# Make the graph -- start of WWII
centrality_graph <- ggraph(ally_net, layout = "kk") +
    geom_edge_link(
        edge_width = 1,
        alpha = 0.5,
        color = "#0072B2"
        ) +
    geom_node_point(
        aes(size = cent),
        alpha = 0.75
    ) +
    theme_graph() + # graph visuals theme
    labs(title = "Military Alliances, 1938") +
    theme(
        plot.title = element_text(size = 14),
        legend.position = "none"
    )

tikzDevice::tikz(file = "centrality.tex", width = 8, height = 5)
centrality_graph
dev.off()