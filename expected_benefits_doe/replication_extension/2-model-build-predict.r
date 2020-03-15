### Train ensemble with all data
library("dplyr")
library("readr")
library("tidyr")
library("tibble")
library("caret")
library("h2o")

sessionInfo()

# Data prep
data <- read_csv("results/data-train-small-no-stale.csv")

set.seed(12345)

train_rows <- sample(1:nrow(data), 0.85 * nrow(data))
test_rows <- setdiff(1:nrow(data), train_rows)

data_train <- data[train_rows, ]
data_test <- data[test_rows, ]

## Create cross-validation folds
##
## Doing this in advance (1) to ensure same folds across models,
## as needed for the automl; and (2) to use caret's functionality
## which ensures class balance as much as possible

set.seed(9248)
folds <- createFolds(
    y = data_train$outcome,
    k = 5, # 5-fold instead of 10-fold
    list = FALSE
)

data_train <- data_train %>%
    add_column(fold = !! folds) 

cat("\nObservations per fold:\n")
print(with(data_train, table(fold)))
print(with(data_train, table(fold, outcome)))

## Set up h2o cluster
h2o.init(nthreads = -1, max_mem_size = "16G")
h2o.no_progress()

stopifnot(all(!is.na(data_train$outcome)))

data_train$outcome <- factor(
    data_train$outcome,
    levels = c("VictoryA", "VictoryB")
)


h2o_data_train <- as.h2o(
    data_train,
    destination_frame = "data_train"
)

# Names of features to be used in model training
x_names <- setdiff(
    names(h2o_data_train),
    c("outcome", "fold") # don't train on outcome or cv-folds
)

# Looks good?
cat("\nTraining variables:\n")
print(x_names)

# Test set
data_test$outcome <- factor(
    data_test$outcome,
    levels = c("VictoryA", "VictoryB")
)

stopifnot(all(!is.na(data_test$outcome)))
h2o_data_test <- as.h2o(
    data_test, 
    destination_frame = "data_test"
)

# Number of obs?
print(table(data_train$outcome))
print(table(data_test$outcome))
# Though low n, the automl function does a good job


## To display memory usage
print_free_mem <- function() {
    cat("\nNo. objects in h2o:", length(h2o.ls()[["key"]]), "\n\n")
    print(dim(h2o.ls()))
    x <- h2o.clusterStatus()
    used <- as.numeric(x$mem_value_size)
    cat("Memory:",
        sprintf("%.2f GB used", used * 1024^-3),
        "\n")
    invisible(used)
}

print_free_mem()

cat("\nTraining AutoML Algorithm", date(), "\n")

automl <- h2o.automl(
    y = "outcome",
    x = x_names,
    training_frame = h2o_data_train,
    fold_column = "fold",
    keep_cross_validation_predictions = TRUE,
    seed = 12345,
    balance_classes = TRUE # imbalance in outcome
)

automl@leader

## Model performance?
# training set
automl_perf_train <- h2o.performance(
    model = automl@leader,
    newdata = h2o_data_train
)
automl_perf_train

# test set
automl_perf_test <- h2o.performance(
    model = automl@leader,
    newdata = h2o_data_test
)
automl_perf_test
# looks good


# Now let's predict
# All possible dyads
non_dir_dyad <- readRDS("results/data-non-dir-dyad.Rds")
# Country labels
non_dir_dyad_labels <- non_dir_dyad %>%
    select(c(ccode_a, ccode_b))
# Features only
non_dir_dyad_features <- non_dir_dyad %>%
    select(-c(ccode_a, ccode_b))
# Convert to H2O object for predictions
h2o_non_dir_dyad <- as.h2o(non_dir_dyad_features)
# Make predictions
preds <- h2o.predict(automl, h2o_non_dir_dyad)
# Look good?
head(preds)
# Turn predictions into data frame
preds_df <- as.data.frame(preds)

# Merge predictions to dyad data frame
merged <- cbind(non_dir_dyad, preds_df)
merged <- merged %>%
    select(
        c(
            year,
            ccode_a,
            ccode_b,
            predict,
            VictoryA,
            VictoryB
        )
    ) %>%
    mutate(
        prediction = ifelse(
            VictoryA > VictoryB,
            "a",
            "b"
        )
    )

saveRDS(merged, "results/small_predictions_automl_year.Rds")