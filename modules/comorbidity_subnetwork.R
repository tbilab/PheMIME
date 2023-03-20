
comorbidity_subnetwork = function(comorbidities,brushed_points,z_institution,code_description) {

  association_pairs = comorbidities %>%
    dplyr::arrange(desc(!!sym(z_institution))) %>%
    dplyr::rename(strength=z_institution) %>%
    dplyr::select(a,b,strength) %>%
    dplyr::filter(!is.na(strength))
  #
  subgraphs <- association_pairs %>% calculate_subgraph_structure()
  #
  node_info = tibble(id = unique(c(association_pairs$a,association_pairs$b))) %>%
    left_join(.,phecode_def %>% dplyr::select(phecode,description,group,color) %>% dplyr::rename(id=description),by="id") %>%
    arrange(group) %>%
    mutate(color = ifelse((phecode %in% brushed_points) | (id %in% code_description()),color,"#DEDEDE"))

  if(((brushed_points[1]==0) & (length(brushed_points)==1)) | length(brushed_points)==0){
    #
    visualize_subgraph_structure(
      association_pairs,
      node_info = node_info,
      subgraph_results = subgraphs,
      trim_subgraph_results = TRUE
    )

  } else {

    visualize_subgraph_structure(
      association_pairs,
      node_info = node_info,
      subgraph_results = subgraphs,
      trim_subgraph_results = TRUE
      ,
      pinned_node = code_description()
    )

  }
}
