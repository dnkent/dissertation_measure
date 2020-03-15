#------------------------------------------------------# 
# Build GLMs comparing dissatisfaction to ideal points #
# as predictors of conflict initiation                 #
#------------------------------------------------------#

library(dplyr)
library(tidyr)
library(hrbrthemes)
library(dynamr)
library(lme4) # random effects
library(plm) # fixed effects
library(pscl) # count model
library(dotwhisker)

# Load Data
final_dat <- readRDS("final_measure.rds")

#-------------------
# Statistical Models
#-------------------

## Time-varying effects? Takes a couple minutes
dissatisfaction_dynam <- dynamr(
    dat = final_dat,
    time_var = "year",
    formula = onset ~ dissatisfaction + polity + tpop,
    num_models = 182,
    window_size = 25,
    start_time = 1816,
    end_time = 2012,
    change_var = "dissatisfaction",
    family = "binomial",
    N = 2500
)

dissatisfaction_dynam %>% filter(change == 1)

dynam_plot <- ggplot(dissatisfaction_dynam,
        aes(x = Time, y = Estimate, colour = factor(change))
        ) +
    geom_errorbar(
        aes(
            ymin = (Estimate - (1.96 * std.err)),
            ymax = (Estimate + (1.96 * std.err))
            ),
        width = 1.5,
        size = 1.75
        ) +
    scale_color_manual(values = c(
        "#666666",
        "#D55E00"
        )) +
    geom_point() +
    theme_ipsum() +
    theme(
        axis.text.x = element_text(
            angle = 45, hjust = 1, color = "black"
            ),
        axis.text.y = element_text(color = "black"),
        text = element_text(size = 12),
        legend.position = "none"
        ) +
    scale_x_continuous(
        breaks = round(seq(1815, 2012,
        by = 15),1)
        ) +
    labs(
        title = "MID Initiation and International Dissatisfaction",
        x = "Year",
        y = "Coefficient Estimate"
        ) +
    theme(
        axis.title.x = element_text(size = 16, hjust = 0.5, vjust = -0.2),
        axis.title.y = element_text(size = 16, hjust = 0.5),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14),
        plot.title = element_text(color = "black", size = 18)
    )


## some movement, but always positive
tikz(
    file = "../../writing/chapter_1/dynam.tex",
    width = 11,
    height = 8.5
    )
dynam_plot
dev.off()

# Naive
m1 <- glm(
    onset ~ dissatisfaction,
    data = final_dat,
    family = binomial
)
summary(m1)

## onset logit -- more satibinomialsfied means fewer mids
m2 <- glm(
    onset ~ dissatisfaction + polity + idealpoint + log(tpop),
    data = final_dat,
    family = binomial
)
summary(m2)

# fixed effects
m3 <- plm(
    onset ~ dissatisfaction,
    data = final_dat,
    index = c("stateabb", "year"),
    family = binomial,
    model = "within"
)
summary(m3)

m4 <- plm(
    onset ~ dissatisfaction + polity + idealpoint + log(tpop),
    data = final_dat,
    index = c("stateabb", "year"),
    family = binomial,
    model = "within"
)
summary(m4)

# Count Models
m5 <- glm(
    number_onset ~ dissatisfaction,
    data = final_dat,
    family = poisson
)
summary(m5)

m6 <- glm(
    number_onset ~ dissatisfaction + polity + idealpoint + log(tpop),
    data = final_dat,
    family = poisson
)
summary(m6)

m7 <- plm(
    number_onset ~ dissatisfaction,
    data = final_dat,
    index = c("stateabb", "year"),
    family = poisson,
    model = "within"
)
summary(m7)

m8 <- plm(
    number_onset ~ dissatisfaction + polity + idealpoint + log(tpop),
    data = final_dat,
    index = c("stateabb", "year"),
    family = poisson,
    model = "within"
)
summary(m8)

# Coefficient plot
# No matter what model, the relationship holds
glm_results <- dwplot(
    list(m1, m2, m3, m4, m5, m6, m7, m8),
    dot_args = list(
        size = 4
        ), # dot characteristics
    whisker_args = list(size = 3.5), # whisker length
    vline = geom_vline(# vline
        xintercept = 0,
        colour = "grey20",
        linetype = "solid",
        size = 1)) %>%
    relabel_predictors(
        c(
            dissatisfaction = "International \n Dissatisfaction",
            polity = "Polity \n Score",
            idealpoint = "Ideal \n Point",
            `log(tpop)` = "Log(Population)"
        )
    ) +
    hrbrthemes::theme_ipsum_rc() +
    xlab("Coefficient Estimate") +
    ylab("Predictor") +
    ggtitle("GLM Results") +
    guides() +
    theme(
        axis.title.x = element_text(
            size = 15, hjust = 0.5, vjust = -0.2),
        axis.title.y = element_text(
            size = 15, hjust = 0.5, vjust = 5),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14),
        plot.title = element_text(face = "bold", size = 18),
        legend.position = "bottom",
        #legend.background = element_rect(colour = "grey80"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.key.width = unit(0.3, "cm")
        ) +
    scale_color_manual(
        labels = c(
            "Logit, \n No Controls",
            "Logit, \n Controls \n",
            "FE Logit, \n No Controls",
            "FE Logit, \n Controls \n",
            "Poisson, \n No Controls",
            "Poisson, \n Controls \n",
            "FE Possion, \n No Controls",
            "FE Poisson, \n Controls \n"
            ),
        values = c(
            "#E69F00", "#56B4E9", "#009E73", "#F0E442",
            "#0072B2", "#D55E00", "#CC79A7", "#000000"
        )
    ) +
    guides(
        color = guide_legend(
            reverse = TRUE,
            ncol = 4),
        shape = FALSE
    )

glm_results

tikz(
    file = "../../writing/chapter_1/glm.tex",
    width = 11,
    height = 8.5
    )
glm_results
dev.off()


## Predicted values -- max-min to make easily interpretable
max_min <- function(x){
    (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
final_dat$diss_trans <- max_min(final_dat$dissatisfaction)
hist(final_dat$diss_trans)

predicted_probs <- ggplot(
        final_dat,
        aes(x = diss_trans, y = onset)
        ) +
    stat_smooth(
        method = "glm",
        method.args = list(family = "binomial"),
        se = TRUE,
        fullrange = TRUE
    ) +
    geom_point() +
    labs(
        x = "International Dissatisfaction (Min-Max Transformation)",
        y = "Pr(Mid Onset)",
        title = "Predicted Probability of MID Onset and 
        International Dissatisfaction"
    ) +
    hrbrthemes::theme_ipsum() +
    theme(
        axis.title.y = element_text(size = 13, hjust = 0.5),
        axis.title.x = element_text(size = 13, hjust = 0.5, vjust = -0.2),
        axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        plot.title = element_text(color = "black", size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 11)
    )

ggsave(
    filename = "../../writing/chapter_1/regression.png",
    plot = predicted_probs,
    width = 8,
    height = 5,
    units = "in",
    dpi = 750
)
