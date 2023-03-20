gen_table <- function(code_id, code_data, type){
  code_data() %>%
    dplyr::select(phecode,description,category,
                  glue("{(type)}_vandy"),
                  glue("{(type)}_mgh"),
                  glue("{(type)}_ukbb"))
  # %>%
  #   mutate(vandy=round(glue("{(type)}_vandy"),3),
  #          mgh=round(glue("{(type)}_mgh"),3),
  #          ukb=round(glue("{(type)}_ukbb"),3))
}
