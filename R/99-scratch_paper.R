# 99-scratch_paper.R
# pacman::p_install("pak")
options(repos = c(getOption("repos"), H2O.ai = "https://h2o-release.s3.amazonaws.com/h2o/rel-yu/3/R"))
# pak::pkg_install(
#   c("tidyverse", "vroom", "tictoc", "RCurl", "jsonlite", "h2o"),
#   ask = FALSE,
#   upgrade = TRUE
# )
pacman::p_load(tidyverse, vroom, h2o)
# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()

tictoc::tic()

all_train <-
  vroom::vroom("Data/train.csv") %>%
  mutate(Embarked = as.factor(Embarked), Survived = as.factor(Survived))

my_train <- all_train %>% sample_frac(0.80)
my_test <- setdiff(all_train, my_train)

# replace lines w/ script that always gets latest h2o

# pacman::p_load(h2o)
# h2o.init()

source("R/98-get_latest_h2o.R")

train.h2o <- as.h2o(my_train)

# split.h2o <- h2o.splitFrame(as.h2o(my_train), ratios = 0.80)
# train.h2o <- split.h2o[[1]]
# valid.h2o <- split.h2o[[2]]
test.h2o <- as.h2o(my_test)

# h2o.describe(train.data)

y <- "Survived" # result -- what we're predicting

x <-
  setdiff(names(train.h2o), c(
    y,
    "PassengerId",
    "Name",
    "SibSp",
    "Parch",
    "Ticket",
    "Cabin"
  )) # predictors

# For binary classification, response should be a factor
# train.data[, y] <- as.factor(train.data[, y])

aml <- h2o.automl(
  x = x,
  y = y,
  training_frame = train.data
)

aml_lb <- aml@leaderboard


test.data_id <- test.h2o[ , "PassengerId"]

test <- test.h2o[ , x]
pred <- predict(aml, test)

rating_tab <-
  bind_cols(as_tibble(test.data_id), as_tibble(pred$predict)) %>%
  left_join(test.h2o %>%
              as_tibble() %>%
              select(PassengerId, Survived)) %>%
  mutate("match?" = predict == Survived)
pct <- rating_tab$`match?` %>% sum() / 119
elapsed <- tictoc::toc()

output <- c(
  "elapsed time is {elapsed}",
  "pct accurate is {pct}",
  "\n",
  rating_tab
  )
write_lines(output, "{lubridate::now()}_output.txt")
