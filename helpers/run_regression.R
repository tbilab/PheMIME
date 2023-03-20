library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shinyjs)
# library(network3d)
library(magrittr)
library(plotly)
library(here)
library(tidyverse)
library(phewasHelper)
library(gt)
library(glue)
library(DT)
library(associationsubgraphs)
library(aws.s3)
library(UKBBdata)
library(Matrix)
library(data.table)
library(lubridate)
library(furrr)
library(foreach)
library(doParallel)

#prepare the nested_phecodes data
# nested_phecodes <- aws.s3::s3readRDS(bucket="ukb.tbilab",object = "phenome/UKBB_phecode_nested.rds")
nested_phecodes <- aws.s3::s3readRDS(bucket="tbilab",object = "bv221/ruderfer_collaboration/nested_phecodes_updated.rds")
num_phecodes <- nrow(nested_phecodes)

#prepare the id_to_demographics data
# id_to_demographics = aws.s3::s3readRDS(bucket="ukb.tbilab",object = "phenome/UKBB_id_to_demographics.rds")
id_to_demographics = aws.s3::s3readRDS(bucket="tbilab",object = "bv221/ruderfer_collaboration/grid_metadata.rds")

options(future.globals.maxSize = 4000 * 1024 ^ 2)

expand_combinations <- function(n, repeats = FALSE){

  rep_counts <- n:1

  if(!repeats){
    rep_counts <- rep_counts - 1
  }

  tibble(
    a_index = rep(1:n, times = rep_counts),
    b_index = purrr::flatten_int( purrr::map(rep_counts, ~{tail(1:n, .x)}) )
  )
}

shuffle_df <- function(df){
  df[sample(1:nrow(df)),]
}

set.seed(42)
phecode_combos <- expand_combinations(num_phecodes) %>%
  shuffle_df() 
  # %>%
  # mutate(group=as.numeric(cut(1:nrow(.),50)))

start = Sys.time()
message("collecting nodes")
plan(multisession,workers = 30)
# myCluster <- makeCluster(50, # number of cores to use
#                          type = "PSOCK")
# registerDoParallel(myCluster)
results <- furrr::future_pmap(
                   list(nested_phecodes$data[phecode_combos$a_index],
                   nested_phecodes$data[phecode_combos$b_index],
                   nested_phecodes$phecode[phecode_combos$a_index],
                   nested_phecodes$phecode[phecode_combos$b_index]),
                  #  .options = furrr_options(globals = c("id_to_demographics")),
                   purrr::safely(function(phecode_a_ids, phecode_b_ids,phecode_a,phecode_b) {

                    id_to_demographics$phecode_a <- 0L
                    id_to_demographics$phecode_a[phecode_a_ids$int_id] <- 1L
                     
                    id_to_demographics$phecode_b <- 0L
                    id_to_demographics$phecode_b[phecode_b_ids$int_id] <- 1L

                     
                        a_to_b <-
                       speedglm::speedglm(
                         phecode_b ~ phecode_a + age + sex + race + ehr_age + log_B,
                         family = binomial(),
                         data = id_to_demographics
                       )

                       b_to_a <-
                       speedglm::speedglm(
                         phecode_a ~ phecode_b + age + sex + race + ehr_age + log_B,
                         family = binomial(),
                         data = id_to_demographics
                       )

                      bind_cols(phecode_a=phecode_a,phecode_b=phecode_b,dplyr::bind_cols(
                       dplyr::select(
                         broom::tidy(a_to_b)[2,],
                         beta_a = estimate,
                         std_err_a = std.error,
                         z_a = statistic,
                         p_val_a = p.value
                       ),
                       dplyr::select(
                         broom::tidy(b_to_a)[2,],
                         beta_b = estimate,
                         std_err_b = std.error,
                         z_b = statistic,
                         p_val_b = p.value
                       )
                     ) %>%
                       dplyr::mutate_if(is.factor, function(x){as.numeric(as.character(x))}) %>%
                       dplyr::mutate(
                         n_a = nrow(phecode_a_ids),
                         n_b = nrow(phecode_b_ids),
                         overlap = sum(id_to_demographics$phecode_a == 1 & id_to_demographics$phecode_b == 1)
                       ))
                   })
                   )

message("finish")
# write_rds(results, "/home/siwei/cseToolkit/data/UKBB_regression.rds")
aws.s3::s3saveRDS(x=results,bucket="tbilab",
object = "bv221/ruderfer_collaboration/Vandy_regression_results_total.rds",multipart = TRUE)
end = Sys.time()
end-start





