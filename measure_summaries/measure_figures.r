## Evaluating the initial measure
library(tidyverse)
library(hrbrthemes)
library(GGally)
library(colorblindr)

# Import data
centrality <- import(
    "../actual_benefits_networks/r_networks/centrality_rankings.rds"
)
final_dat <- readRDS("final_measure.rds")

# Does a stagte's ideal point correlate with its dissatisfaction?
cor(final_dat$idealpoint, final_dat$dissatisfaction, use = "complete.obs")

id_diss_corr <- ggplot(final_dat, aes(x = dissatisfaction, y = idealpoint)) +
    geom_point() +
    stat_smooth(
        method = "glm"
    ) +
    labs(
        title = "Correlation Between \nDissatisfaction and Ideal Points",
        x = "International Dissatisfaction",
        y = "Ideal Point"
    ) +
    theme_ipsum_rc() +
    theme(
        axis.title.y = element_text(size = 13, hjust = 0.5),
        axis.title.x = element_text(size = 13, hjust = 0.5, vjust = -0.2),
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        plot.title = element_text(color = "black", size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 11)
    )

# some overlap, but quite a bit of variation
id_diss_corr
# for roboto_condensed: https://github.com/hrbrmstr/hrbrthemes/issues/18

# Save
ggsave(
    filename = "../../writing/chapter_1/ideal_diss_cor.pdf",
    plot = id_diss_corr,
    width = 6,
    height = 6,
    units = "in"
)


# Correlation between cinc score and win percentage
cor(final_dat$cinc, final_dat$percent_win, use = "complete.obs")

# some association -- which is good, but enough differences to
# represent capturing something separate for each
corr_plot <- ggplot(final_dat, aes(x = percent_win, y = cinc)) +
    geom_point() +
    theme_ipsum() +
    theme(
        axis.title.x = element_text(size = 13, hjust = 0.5, vjust = -0.2),
        axis.title.y = element_text(size = 13, hjust = 0.5),
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        plot.title = element_text(color = "black", size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 11)
    ) +
    labs(
        x = "Predicted Percent of MIDs Won",
        y = "CINC Score",
        title = "Comparison of CINC Scores and Predicted MID Outcomes"
    )

corr_plot


# Correlation across centrality measures:
centrality_data <- centrality %>%
    select(c(
        alliance_centrality,
        trade_percent,
        diplomacy_centrality,
        arms_centrality
    )) %>%
    rename(
        Alliances = alliance_centrality,
        Trade = trade_percent,
        Diplomacy = diplomacy_centrality,
        Arms = arms_centrality
    )

# Drop year var that R adds
centrality_data <- centrality_data[,-1]

# Correlation matrix plot
ggpairs(centrality_data) +
    theme_ipsum_rc() +
    labs(
        title = "Correlation Across Centrality Components"
    ) +
    theme(
        text = element_text(size = 16),
        axis.title = element_text(size = 16),
        title = element_text(size = 20)
    )

# ggsave because too many points for tikzDevice
ggsave(
    filename = "../../writing/chapter_1/centcorr.pdf",
    dpi = 650,
    units = "in",
    height = 6.5,
    width = 8
)


#----------------------------#
# Summary Network Statistics #
#----------------------------#

## Time periods

## Pre-Bismarck
pre_germany <- filter(final_dat, year < 1871)

## Post-Bismarck-WWI
pre_wwi <- filter(final_dat, year >= 1871 & year < 1914)

## WWI-WWII
pre_cw <- filter(final_dat, year >= 1914 & year <= 1945)

## Cold War
cw <- filter(final_dat, year >= 1945 & year < 1991)

## Post-Cold War
post_cw <- filter(final_dat, year >= 1991)

## Function for extracting top average rank:
extract_top_benefit <- function(x){
    ranks <- x %>%
        group_by(stateabb) %>%
        summarise(mean_benefits = mean(smooth_benefits, na.rm = TRUE))

    # Reorder by rank
    ranks <- ranks %>%
        arrange(desc(mean_benefits))

    # Add names again
    ranks <- left_join(ranks, states)
    ranks
}

pre_germ_benefit <- extract_top_benefit(pre_germany)
pre_wwi_benefit <- extract_top_benefit(pre_wwi)
pre_cw_benefit <- extract_top_benefit(pre_cw)
cw_benefit <- extract_top_benefit(cw)
post_cw_benefit <- extract_top_benefit(post_cw)

# And we are off
pre_germ_benefit
pre_wwi_benefit
pre_cw_benefit
cw_benefit
post_cw_benefit


## Summarize great powers
# Pre-German Unification
german_unification <- pre_germany %>%
    filter(
        stateabb %in% c(
            "USA",
            "UKG",
            "RUS",
            "CHN",
            "FRN",
            "GMY"
        )
    )

# Function for extracting most powerful states
extract_top_power <- function(x){
    ranks <- x %>%
        group_by(stateabb) %>%
        summarise_at(c("cinc", "percent_win", "smooth_expectations"),
            mean, na.rm = TRUE)

    # Reorder by rank
    ranks <- ranks %>%
        arrange(desc(smooth_expectations))

    # Add names again
    ranks <- left_join(ranks, states)
    ranks
}

# Pre WWI -- 1815-1914
pre_wwi <- filter(final_dat, year <= 1914)

## interwar
interwar <- filter(final_dat, year >= 1920 & year <= 1938)

## cold war
cw <- filter(final_dat, year > 1945 & year < 1991)

## Post-Cold War
post_cw <- filter(final_dat, year >= 1991)

pre_wwi_power <- extract_top_power(pre_wwi)
interwar_power <- extract_top_power(interwar)
cw_power <- extract_top_power(cw)
post_cw_power <- extract_top_power(post_cw)

# Let's take a look
pre_wwi_power[1:10,1:4]
interwar_power[1:10,1:4]
cw_power[1:10,1:4]
post_cw_power[1:10,1:4]


#---------------------
# Satisfaction Plots
#---------------------

# New data with changed West Germany name in Cold War for visual continuity
plot_data <- final_dat %>%
    mutate(
        stateabb = ifelse(stateabb == "GFR", "GMY", stateabb)
    )

# Countries
great_powers <- filter(
    plot_data,
    stateabb %in% c(
        "USA",
        "UKG",
        "RUS",
        "CHN",
        "FRN",
        "GMY",
        "JPN"
    ) &
    year %in% c(1816:2012)
)

dissatisfaction_plot <- ggplot(
        great_powers,
        aes(x = year, y = dissatisfaction, color = stateabb)
    ) +
    geom_line(size = 1, aes = 0.25) +
    geom_point(size = 2) +
    labs(
        x = "Year",
        y = "International Dissatisfaction",
        title = "International Dissatisfaction: 1816 - 2012"
    ) +
    theme_ipsum() +
    theme(
        axis.title.x = element_text(size = 18, hjust = 0.5, vjust = -0.2),
        axis.title.y = element_text(size = 18, hjust = 0.5, vjust = 2.5),
        axis.text.x = element_text(color = "black", size = 16),
        axis.text.y = element_text(color = "black", size = 16),
        plot.title = element_text(color = "black", size = 22),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = round(seq(1815, 2012, by = 15), 1)) +
    scale_color_OkabeIto(
        labels = c(
            "China",
            "France",
            "Germany",
            "Japan",
            "Russia",
            "United Kingdom",
            "United States"
        )
    )

dissatisfaction_plot

# Save plot
tikz(
    file = "../../writing/chapter_1/dissatisfaction.tex",
    width = 13,
    height = 8.5
    )
dissatisfaction_plot
dev.off()

#---------------------
# Benefits Plot
#---------------------

benefits_plot_smooth <- ggplot(
        great_powers,
        aes(x = year, y = smooth_benefits, color = stateabb)
    ) +
    geom_line(size = 1, aes = 0.25) +
    geom_point(size = 2) +
    labs(
        x = "Year",
        y = "International Benefits",
        title = "International Benefits: 1816 - 2012"
    ) +
    theme_ipsum() +
    theme(
        axis.title.x = element_text(size = 18, hjust = 0.5, vjust = -0.2),
        axis.title.y = element_text(size = 18, hjust = 0.5, vjust = 2.5),
        axis.text.x = element_text(color = "black", size = 16),
        axis.text.y = element_text(color = "black", size = 16),
        plot.title = element_text(color = "black", size = 22),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = round(seq(1815, 2012, by = 15), 1)) +
    scale_color_OkabeIto(
        labels = c(
            "China",
            "France",
            "Germany",
            "Japan",
            "Russia",
            "United Kingdom",
            "United States"
        )
    )

benefits_plot_smooth

# Save as tikz object
tikz(
    file = "../../writing/chapter_1/benefits_smooth.tex",
    width = 13,
    height = 8.5
    )
benefits_plot_smooth
dev.off()

#----------------------------
# International Expectations
#----------------------------

expectations_plot <- ggplot(
    great_powers, aes(x = year, y = smooth_expectations, color = stateabb)
    ) +
    geom_line(size = 1, aes = 0.25) +
    geom_point(size = 2) +
    labs(
        x = "Year",
        y = "Expected Benefits",
        title = "Expected Benefits: 1816 - 2012"
    ) +
    theme_ipsum() +
    theme(
        axis.title.x = element_text(size = 18, hjust = 0.5, vjust = -0.2),
        axis.title.y = element_text(size = 18, hjust = 0.5, vjust = 2.5),
        axis.text.x = element_text(color = "black", size = 16),
        axis.text.y = element_text(color = "black", size = 16),
        plot.title = element_text(color = "black", size = 22),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = round(seq(1815, 2012, by = 15), 1)) +
    scale_color_OkabeIto(
        labels = c(
            "China",
            "France",
            "Germany",
            "Japan",
            "Russia",
            "United Kingdom",
            "United States"
        )
    )

expectations_plot

tikz(
    file = "../../writing/chapter_1/expectations.tex",
    width = 13,
    height = 8.5
    )
expectations_plot
dev.off()
