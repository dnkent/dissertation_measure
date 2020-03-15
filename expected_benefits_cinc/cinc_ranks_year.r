library(dplyr)
library(tidyr)
library(readr)
library(purrr)


# Import cinc data
nmc <- read_csv(
    "https://www.dropbox.com/s/gfx7rtgauul5fl4/NMC_5_0.csv?dl=1"
    )


# Start empty data frame for storing all ranks
cinc_ranks <- tibble(
    stateabb = as.character(),
    year = as.integer(),
    rank = as.integer(),
    cinc = as.numeric()
)

# Years of interest
years <- c(1816:2012)


for(i in 1:length(years)){
    # Extract year
    time <- years[i]

    # Only obs in this year
    temp <- filter(nmc, year == time)

    # Arrange by rank
    temp <- temp %>%
        mutate(rank = dense_rank(desc(cinc)))

    # Keep useful vars
    temp <- temp %>%
        select(c(stateabb, year, rank, cinc))

    # now append to existing data
    cinc_ranks <- rbind(cinc_ranks, temp)
}

# Export CINC scores
export(cinc_ranks, "cinc_ranks.rds")