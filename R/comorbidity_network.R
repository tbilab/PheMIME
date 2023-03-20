comorbidity_networkPlot = function(id){
  ns <- NS(id)
  institution = list("VUMC"="vandy","UKB"="ukbb","MGB"="mgh","Combination of VUMC and MGB"="vandy & mgh",
                     "Combination of VUMC and UKB"="vandy & ukbb","Combination of MGB and UKB"="mgh & ukbb",
                     "Combination of VUMC, MGB and UKB"="vandy & mgh & ukbb")

  fluidRow(
    column(width=12,div(
      span(textOutput(ns("current_code_label"),inline = TRUE),style = "font-size:2.5rem;color: black;center;center;"),
      style =
        "padding-top: 10px;
             padding-bottom: 5px;
             margin-top: 3px;
             display: flex;
             align-items: center;
             justify-content: space-evenly;"
    )),
    column(width=12,
           # wellPanel(style ="margin-top: 10px;background-color: #fff; border-color: white;",
    box(width=12,title = strong("Select institutions and input p-value",style="font-size: 2.0rem;"),
        solidHeader=F,status="warning",
        fluidRow(column(12, div(
          p("Phenotypes co-occured with selected phecode and corresponding p-value < input p-value threshold are colored, and the other non-significant phenotypes are colored grey;",
            style = "text-align: left;font-size: 2.0rem; color:black;")),
          div(
            p("Please click 'update associationsubgraphs' to visualize the network when a different institution is selected, or a different p-value threshold is input.",
              style = "text-align: left;font-size: 2.0rem; color:black;")),
          hr()),

                  column(4,p(HTML("<b>Select institution</b>"),span(shiny::icon("info-circle"),id = "select_ins"),
                             selectInput(ns('institution'), NULL, choices=institution),
                     tippy::tippy_this(elementId = "select_ins",
                                       tooltip = "<span style='font-size:20px;'>Comorbidity network is build on comorbidity strength from single institution or combined comorbidity strength from multiple instutions<span>",placement = "right"))),

                   column(4,p(HTML("<b>Input p-value</b>"),span(shiny::icon("info-circle"),id = "info_pvalue_net"),
                            numericInput(ns('pvalue'), NULL,min=0,max=1,value=1),
                            tippy::tippy_this(elementId = "info_pvalue_net",
                                              tooltip = "<span style='font-size:20px;'>Phenotypes co-occured with selected phecode and corresponding p-value < input p-value threshold are colored<span>",placement = "right"))),

                  column(4,actionButton(ns("update_network"), "Update Associationsubgraphs"))
    ))),
    column(width=12,
           box(width=12,align="center",solidHeader=F,status="warning",
           # wellPanel(style ="margin-top: 10px;background-color: #fff; border-color: white;",
      div(p(strong("Visualize associationsubgraphs"),
            style = "text-align: left;font-size: 2.0rem; color:black;")),
      hr(),
      conditionalPanel(
        condition = "input.update_network > 0",
        style = "display: none;",
        ns=ns,
        withSpinner(r2d3::d3Output(ns("comorbidity_network_plot"),width = "100%", height = "550px"))
      )
    # )
    )
  ))
}

comorbidity_networkServer = function(id,code_description,code_id) {
  #phecode with pvalue corresponding to all pairs connected to it are above the threshold will be colored into grey
  ##this idea does not work (too many grey color)
  #phecode with pvalue corresponding to at least one pair connected to it are above the threshold will be colored into grey,
  #and not significant edge will be grey, significant edge will still be green
  moduleServer(
    id,
    function(input,output,session){

      selectedData = eventReactive(input$update_network,{
        #hard code
        #select pvalue
        #select institution

        if(input$institution == "vandy") {
          association_pairs_total = com_sim %>%
            dplyr::arrange(desc(z_vandy)) %>%
            dplyr::rename(strength=z_vandy) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))

          association_pairs = com_sim %>%
            dplyr::filter(p_val_vandy <= input$pvalue) %>%
            dplyr::filter((a==code_description()) | (b==code_description())) %>%
            dplyr::arrange(desc(z_vandy)) %>%
            dplyr::rename(strength=z_vandy) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))
          selected_id = unique(c(association_pairs$a,association_pairs$b))

          node_info = tibble(id = unique(c(association_pairs_total$a,association_pairs_total$b))) %>%
            left_join(.,phecode_def %>% dplyr::select(phecode,description,group,color) %>% dplyr::rename(id=description),by="id") %>%
            arrange(group) %>%
            mutate(color = ifelse(id %in% selected_id,color,"#DEDEDE"))

        } else if(input$institution == "ukbb"){
          association_pairs_total = com_sim %>%
            # dplyr::filter(p_val_ukbb <= input$pvalue) %>%
            dplyr::arrange(desc(z_ukbb)) %>%
            dplyr::rename(strength=z_ukbb) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))

          association_pairs = com_sim %>%
            dplyr::filter(p_val_ukbb <= input$pvalue) %>%
            dplyr::filter((a==code_description()) | (b==code_description())) %>%
            dplyr::arrange(desc(z_ukbb)) %>%
            dplyr::rename(strength=z_ukbb) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))
          selected_id = unique(c(association_pairs$a,association_pairs$b))

          node_info = tibble(id = unique(c(association_pairs_total$a,association_pairs_total$b))) %>%
            left_join(.,phecode_def %>% dplyr::select(phecode,description,group,color) %>% dplyr::rename(id=description),by="id") %>%
            arrange(group) %>%
            mutate(color = ifelse(id %in% selected_id,color,"#DEDEDE"))

        } else if(input$institution == "mgh"){
          association_pairs_total = com_sim %>%
            # dplyr::filter(p_val_mgh <= input$pvalue) %>%
            dplyr::arrange(desc(z_mgh)) %>%
            dplyr::rename(strength=z_mgh) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))

          association_pairs = com_sim %>%
            dplyr::filter(p_val_mgh <= input$pvalue) %>%
            dplyr::filter((a==code_description()) | (b==code_description())) %>%
            dplyr::arrange(desc(z_mgh)) %>%
            dplyr::rename(strength=z_mgh) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))
          selected_id = unique(c(association_pairs$a,association_pairs$b))

          node_info = tibble(id = unique(c(association_pairs_total$a,association_pairs_total$b))) %>%
            left_join(.,phecode_def %>% dplyr::select(phecode,description,group,color) %>% dplyr::rename(id=description),by="id") %>%
            arrange(group) %>%
            mutate(color = ifelse(id %in% selected_id,color,"#DEDEDE"))

        } else if(input$institution == "vandy & mgh"){
          association_pairs_total = com_sim %>%
            # dplyr::filter(p_vandy_mgh_max <= input$pvalue) %>%
            dplyr::arrange(desc(z_vandy_mgh)) %>%
            dplyr::rename(strength=z_vandy_mgh) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))

          association_pairs = com_sim %>%
            dplyr::filter(p_vandy_mgh_max <= input$pvalue) %>%
            dplyr::filter((a==code_description()) | (b==code_description())) %>%
            dplyr::arrange(desc(z_vandy_mgh)) %>%
            dplyr::rename(strength=z_vandy_mgh) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))
          selected_id = unique(c(association_pairs$a,association_pairs$b))

          node_info = tibble(id = unique(c(association_pairs_total$a,association_pairs_total$b))) %>%
            left_join(.,phecode_def %>% dplyr::select(phecode,description,group,color) %>% dplyr::rename(id=description),by="id") %>%
            arrange(group) %>%
            mutate(color = ifelse(id %in% selected_id,color,"#DEDEDE"))

        } else if(input$institution == "vandy & ukbb"){
          association_pairs_total = com_sim %>%
            # dplyr::filter(p_vandy_ukbb_max <= input$pvalue) %>%
            dplyr::arrange(desc(z_vandy_ukbb)) %>%
            dplyr::rename(strength=z_vandy_ukbb) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))

          association_pairs = com_sim %>%
            dplyr::filter(p_vandy_ukbb_max <= input$pvalue) %>%
            dplyr::filter((a==code_description()) | (b==code_description())) %>%
            dplyr::arrange(desc(z_vandy_ukbb)) %>%
            dplyr::rename(strength=z_vandy_ukbb) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))
          selected_id = unique(c(association_pairs$a,association_pairs$b))

          node_info = tibble(id = unique(c(association_pairs_total$a,association_pairs_total$b))) %>%
            left_join(.,phecode_def %>% dplyr::select(phecode,description,group,color) %>% dplyr::rename(id=description),by="id") %>%
            arrange(group) %>%
            mutate(color = ifelse(id %in% selected_id,color,"#DEDEDE"))

        } else if(input$institution == "mgh & ukbb"){
          association_pairs_total = com_sim %>%
            # dplyr::filter(p_mgh_ukbb_max <= input$pvalue) %>%
            dplyr::arrange(desc(z_mgh_ukbb)) %>%
            dplyr::rename(strength=z_mgh_ukbb) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))

          association_pairs = com_sim %>%
            dplyr::filter(p_mgh_ukbb_max <= input$pvalue) %>%
            dplyr::filter((a==code_description()) | (b==code_description())) %>%
            dplyr::arrange(desc(z_mgh_ukbb)) %>%
            dplyr::rename(strength=z_mgh_ukbb) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))
          selected_id = unique(c(association_pairs$a,association_pairs$b))

          node_info = tibble(id = unique(c(association_pairs_total$a,association_pairs_total$b))) %>%
            left_join(.,phecode_def %>% dplyr::select(phecode,description,group,color) %>% dplyr::rename(id=description),by="id") %>%
            arrange(group) %>%
            mutate(color = ifelse(id %in% selected_id,color,"#DEDEDE"))

        } else if(input$institution == "vandy & mgh & ukbb"){
          association_pairs_total = com_sim %>%
            # dplyr::filter(p_vandy_mgh_ukbb_max <= input$pvalue) %>%
            dplyr::arrange(desc(z_vandy_mgh_ukbb)) %>%
            dplyr::rename(strength=z_vandy_mgh_ukbb) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))

          association_pairs = com_sim %>%
            dplyr::filter(p_vandy_mgh_ukbb_max <= input$pvalue) %>%
            dplyr::filter((a==code_description()) | (b==code_description())) %>%
            dplyr::arrange(desc(z_vandy_mgh_ukbb)) %>%
            dplyr::rename(strength=z_vandy_mgh_ukbb) %>%
            dplyr::select(a,b,strength) %>%
            dplyr::filter(!is.na(strength))
          selected_id = unique(c(association_pairs$a,association_pairs$b))

          node_info = tibble(id = unique(c(association_pairs_total$a,association_pairs_total$b))) %>%
            left_join(.,phecode_def %>% dplyr::select(phecode,description,group,color) %>% dplyr::rename(id=description),by="id") %>%
            arrange(group) %>%
            mutate(color = ifelse(id %in% selected_id,color,"#DEDEDE"))

        }

        list(association_pairs = association_pairs_total,
             subgraphs = association_pairs_total %>% calculate_subgraph_structure(),
             node_info = node_info)
      })

        output$comorbidity_network_plot = r2d3::renderD3({
          Sys.sleep(3)
           if(code_description() %in% unique(c(selectedData()[["association_pairs"]]$a,selectedData()[["association_pairs"]]$b))){
             visualize_subgraph_structure(
               selectedData()[["association_pairs"]],
               node_info = selectedData()[["node_info"]],
               subgraph_results = selectedData()[["subgraphs"]],
               trim_subgraph_results = TRUE,
               pinned_node = code_description()
             )
           } else{
             visualize_subgraph_structure(
               selectedData()[["association_pairs"]],
               node_info = selectedData()[["node_info"]],
               subgraph_results = selectedData()[["subgraphs"]],
               trim_subgraph_results = TRUE
             )
           }
         })

        output$current_code_label <- renderText(glue("Current selection: {code_id()}"))

    })
}


