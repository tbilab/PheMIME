gen_manhattan <- function(code_id, pair_data, type,type_label,z_institution,phe_in){
  options(ggrepel.max.overlaps = Inf)

  if(phe_in == "yes"){#whether the phecode is existing in the selected institutions

   p = pair_data %>%
    mutate(label = ifelse(is_labeled, paste(phecode, description), "")) %>%
    mutate(phecode = reorder(phecode, phecode_index)) %>%
    # mutate(hover = ifelse(is_hovered, paste(phecode, description), "")) %>%
      ggplot(
      aes_string(
        x = "phecode",
        y = paste0(glue("{(type_label)}_{(z_institution)}")),
        color = "category"
      )
    ) +
    geom_point(shape=1,alpha=0.7,size=3) +
     # ggrepel::geom_label_repel(
     #   aes(label = hover),
     #   color = "black",
     #   min.segment.length = unit(0.2, "lines"),
     #   direction = "y",
     #   force = 5,
     #   box.padding = 1,
     #   show.legend = FALSE
     # )+
    scale_color_phecode() +
    # theme_phewas() +
    labs(
      x = "Phecode",
      y = glue("{str_to_title(type)} strength"),
      title = glue("{str_to_title(type)} values with {code_id()}")
    ) +
    theme_bw()+
    scale_size_area(limits = c(0, 1000), max_size = 10, guide = NULL)+
    theme_phewas() +
    theme(
      # panel.border = element_blank(),
      # axis.line = element_line(colour = "black"),
      legend.position = "none",
      # axis.ticks.x = ggplot2::element_blank(),
      # axis.text.x = ggplot2::element_blank(),
      # panel.grid.major.x = ggplot2::element_blank(),
      # panel.grid.minor.x = ggplot2::element_blank(),
      # axis.ticks.y = ggplot2::element_blank(),
      # axis.text.y = ggplot2::element_blank(),
      axis.title.x = element_text(size=15, face="bold", colour = "black"),
      axis.title.y = element_text(size=15, face="bold", colour = "black"),
      plot.title = element_text(size=15, face= "bold", colour= "black" ),
      legend.text=element_text(size=8)
    )

    p + geom_point(data=pair_data %>%
                     mutate(label = ifelse(is_labeled, paste(phecode, description), "")) %>%
                     mutate(phecode = reorder(phecode, phecode_index)) %>%
                     filter(is_labeled),
      aes_string(
        x = "phecode",
        y = paste0(glue("{(type_label)}_{(z_institution)}")),
        color = "category"
      ),alpha = 1,size=5)

 } else{

     ggplot(pair_data,
       aes_string(
         x = "phecode",
         y = paste0(glue("{(type_label)}_{(z_institution)}")),
         color = "category"
       )
     ) +
     labs(
       x = "Phecode",
       y = glue("{str_to_title(type)} strength"),
       title = glue("{str_to_title(type)} values with {code_id()}")
     ) +
     theme_bw()+
     scale_size_area(limits = c(0, 1000), max_size = 10, guide = NULL)+
     theme_phewas() +
     theme(
       # panel.border = element_blank(),
       # axis.line = element_line(colour = "black"),
       legend.position = "right",
       # axis.ticks.x = ggplot2::element_blank(),
       # axis.text.x = ggplot2::element_blank(),
       # panel.grid.major.x = ggplot2::element_blank(),
       # panel.grid.minor.x = ggplot2::element_blank(),
       # axis.ticks.y = ggplot2::element_blank(),
       # axis.text.y = ggplot2::element_blank(),
       axis.title.x = element_text(size=15, face="bold", colour = "black"),
       axis.title.y = element_text(size=15, face="bold", colour = "black"),
       plot.title = element_text(size=15, face= "bold", colour= "black" ),
       legend.text=element_text(size=8)
     )
 }
}





