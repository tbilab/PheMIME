# Common functions that are used across reports etc
rank_by_col <- function(df, rank_by, rank_col){
  df %>%
    arrange(-{{rank_by}}) %>%
    mutate({{rank_col}} := row_number())
}

join_phecode_info = function (data_w_phecode, phecode_column = phecode, cols_to_join = c("description",
                                                                     "category", "category_number", "phecode_index"))
{
  phecode_column_name <- rlang::quo_name(rlang::enquo(phecode_column))
  phecode_col_missing <- !(phecode_column_name %in% colnames(data_w_phecode))
  if (phecode_col_missing) {
    stop("Missing phecode column in data. Make sure phecode_column argument is correct.")
  }
  has_any_appended_cols <- cols_to_join %in% colnames(data_w_phecode)
  if (any(has_any_appended_cols)) {
    warning("Existing info columns in input. Joined info columns will be suffixed with '_info'.")
  }
  available_info_cols <- colnames(phecode_descriptions)
  bad_requests <- available_info_cols[!(cols_to_join %in% available_info_cols)]
  if (length(bad_requests) > 0) {
    stop(paste0("The request phecode information (", paste(bad_requests,
                                                           collapse = ","), (if (length(bad_requests) == 1)
                                                             ") is"
                                                             else "are"), " unavailable. Possible information values include: ",
                paste(available_info_cols, collapse = ", ")))
  }
  dplyr::left_join(data_w_phecode, dplyr::select(phecode_descriptions,
                                                 phecode, dplyr::one_of(cols_to_join)), suffix = c("",
                                                                                                   "_info"), by = rlang::set_names("phecode", phecode_column_name))
}

extract_code <- function(data, code){
  bind_rows(
    data %>%
      filter(a == code) %>%
      rename(description = b) %>%
      select(-a),
    data %>%
      filter(b == code) %>%
      rename(description = a) %>%
      select(-b)
  ) %>%
    left_join(.,phecode_def %>% dplyr::select(phecode,description),by="description") %>%
    join_phecode_info(cols_to_join = c("category", "phecode_index"))
}

bootstrap_pearson_cor <- function(data, x_col, y_col, B = 500){
  N <- nrow(data)
  X <- data[[x_col]]
  Y <- data[[y_col]]
  indices <- seq_len(N)

  cor_vals <- purrr::map_dbl(seq_len(B), ~{
    bs_indices <- sample(x = indices, size = N, replace = TRUE)
    cor(
      X[bs_indices],
      Y[bs_indices],
      method = "pearson"
    )
  })

  tibble(
    lower = quantile(cor_vals, 0.025),
    mean = mean(cor_vals),
    upper = quantile(cor_vals, 0.975)
  )
}

bootstrap_spearman_cor <- function(data, x_col, y_col, B = 500){
  N <- nrow(data)
  X <- data[[x_col]]
  Y <- data[[y_col]]
  indices <- seq_len(N)

  cor_vals <- purrr::map_dbl(seq_len(B), ~{
    bs_indices <- sample(x = indices, size = N, replace = TRUE)
    cor(
      X[bs_indices],
      Y[bs_indices],
      method = "spearman"
    )
  })

  tibble(
    lower = quantile(cor_vals, 0.025),
    mean = mean(cor_vals),
    upper = quantile(cor_vals, 0.975)
  )
}

category_table_color_scale <- scales::col_factor(palette = phewasHelper::category_colors(), domain = names(phewasHelper::category_colors()), ordered = TRUE)


# These functions help us run linear models on correlation values which inherently are bound to an interval of (-1,1) which is bad for basic linear models.
logit <- function(x){
  log(x/(1 - x))
}
inverse_logit <- function(logit_val){
  1/(1 + exp(-logit_val))
}

logit_transform_correlations <- function(similarities){
  logit( (similarities + 1)/2)
}

invert_logit_correlation <- function(logit_cor_val){

  (inverse_logit(logit_cor_val)*2)-1
}

magnitude_relative_difference <- function(a_vec, b_vec){
  (a_vec - b_vec)/ ((abs(a_vec) + abs(b_vec))/2)
}

val_or_zero <- function(x){
  ifelse(is.na(x), 0, x)
}


mirror_a_b <- function(pairs){
  bind_rows(
    pairs,
    pairs %>% rename(a = b, b = a)
  )
}

# Function to make sure a is always the "earlier" code. Usefull for merging two
# different sets of pairs that may have a different ordering
sort_pairs <- . %>%
  mutate(
    old_a = a,
    old_b = b,
    a = ifelse(old_a < old_b, old_a, old_b),
    b = ifelse(old_a < old_b, old_b, old_a)
  ) %>%
  select(-old_a, -old_b)



vandy_color <- "#D8AB4C"
mgh_color <- "#A41034"
both_color <-  "#525252"

extract_and_diff <- function(pairs_df, code){
  pairs_df %>%
    extract_code(code)
  # %>%
  #   drop_na(z_avg_vandy, z_avg_mgh, z_avg_ukbb,z_combined_vandy_mgh_ukbb)
  # %>%
  #   mutate(difference = vandy - mgh) %>%
  #   arrange(difference)
}

gen_top_values_table <- function(code_id, code_data, type, top_n = 10){
  code_data() %>%
    arrange(-combined) %>%
    head(top_n) %>%
    mutate(label = paste(phecode, description)) %>%
    select(label, category, combined, vandy, mgh) %>%
    gt(rowname_col = "label") %>%
    fmt_number(
      columns = vars(combined, vandy, mgh),
      decimals = 2
    ) %>%
    tab_spanner(
      columns = vars(combined, vandy, mgh),
      str_to_title(type)
    ) %>%
    cols_label(
      category = "Category",
      combined = "Combined",
      vandy = "Vanderbilt",
      mgh = "MGH"
    ) %>%
    tab_header(
      title = md(glue("**Top {top_n} most {str_remove(type,'ity')}<sup>*</sup> Phecodes with {code_id()}**")),
      subtitle = md("<sup>*</sup> according to combined comorbidity strength")
    )
}

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






