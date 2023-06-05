gen_across_scatter <- function(code_id, pair_data, type,type_label,z_institution1,z_institution2,phe_in){
  if(phe_in == "yes"){
  p = pair_data %>%
    mutate(label = ifelse(is_labeled, paste(phecode, description), "")) %>%
    # mutate(larger_in = ifelse(vandy > mgh, "Vanderbilt", "MGH")) %>%
    ggplot(aes_string(x = paste0(glue("{(type_label)}_{(z_institution1)}")), y = paste0(glue("{(type_label)}_{(z_institution2)}")), color = "category")) +
    geom_abline(slope = 1, alpha = 0.5) +
    geom_point(shape=1,alpha=0.7,size=3) +
    scale_color_phecode() +
    labs(
      x = glue("Institution 1 {type}"),
      y = glue("Institution 2 {type}"),
      fill = glue("More {str_remove(type,'ity')} in"),
      title = glue("{str_to_title(type)} with {code_id()} across systems")
      # ,
      # subtitle = glue("Points above and below the diagonal line are more {str_remove(type,'ity')} in Vanderbilt or MGH's system respectively.")
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
      #panel.grid.major.x = ggplot2::element_blank(),
      #panel.grid.minor.x = ggplot2::element_blank(),
      # axis.ticks.y = ggplot2::element_blank(),
      # axis.text.y = ggplot2::element_blank(),
      axis.title.x = element_text(size=15, face="bold", colour = "black"),
      axis.title.y = element_text(size=15, face="bold", colour = "black"),
      plot.title = element_text(size=15, face= "bold", colour= "black" ),
      plot.subtitle = element_text(size=15, face= "bold", colour= "black" ),
      legend.text=element_text(size=8)
    )
  p + geom_point(data=pair_data %>%
                   mutate(label = ifelse(is_labeled, paste(phecode, description), "")) %>%
                   mutate(phecode = reorder(phecode, phecode_index)) %>%
                   filter(is_labeled),
                 aes_string(
                   x = paste0(glue("{(type_label)}_{(z_institution1)}")),
                   y = paste0(glue("{(type_label)}_{(z_institution2)}")),
                   color = "category"
                 ),alpha = 1,size=5)

  } else{
    ggplot(pair_data,aes_string(x = paste0(glue("{(type_label)}_{(z_institution1)}")), y = paste0(glue("{(type_label)}_{(z_institution2)}")), color = "category")) +
      geom_abline(slope = 1, alpha = 0.5) +
      labs(
        x = glue("Institution 1 {type}"),
        y = glue("Institution 2 {type}"),
        fill = glue("More {str_remove(type,'ity')} in"),
        title = glue("{str_to_title(type)} with {code_id()} across systems")
        # ,
        # subtitle = glue("Points above and below the diagonal line are more {str_remove(type,'ity')} in Vanderbilt or MGH's system respectively.")
      ) +
      theme_bw()+
      scale_size_area(limits = c(0, 1000), max_size = 10, guide = NULL)+
      theme_phewas() +
      theme(
        # panel.border = element_blank(),
        # axis.line = element_line(colour = "black"),
        legend.position = "none",
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        #panel.grid.major.x = ggplot2::element_blank(),
        #panel.grid.minor.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.x = element_text(size=15, face="bold", colour = "black"),
        axis.title.y = element_text(size=15, face="bold", colour = "black"),
        plot.title = element_text(size=12, face= "bold", colour= "black" ),
        plot.subtitle = element_text(size=15, face= "bold", colour= "black" ),
        legend.text=element_text(size=8)
      )
  }

}
