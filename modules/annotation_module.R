annotationPlot <- function(id) {
  ns <- NS(id)
  tagList(
    # tags$head(tags$script(src="my.js")),
    fluidRow(
      # load required Java Script
      useShinyjs(),
      
      # column(width=12,
      #        div(
      #   span(withSpinner(textOutput(ns("current_code_label"),inline = F),size=0.01),style = "font-size:2.5rem;color: black;center;center;"),
      #   style =
      #     "padding-top: 10px;
      #        padding-bottom: 5px;
      #        margin-top: 3px;
      #        display: flex;
      #        align-items: center;
      #        justify-content: space-evenly;"
      # )),

      column(12,
             box(width=12,title=strong("Interactive Manhattan/Scatter Plot and Network",style="font-size: 2.0rem;"),solidHeader = F,status="warning",
                fluidRow(
                  column(12,div(p("Click a point to annotate it. Drag region to highlight multiple points. Double click to remove annotation.",style = "font-size: 2.0rem;color: black;"))),
                  column(2,div(p("Hovered over:",style = "font-size:2.0rem;color: black;"))),
                  column(6,span(textOutput(ns("plot_hoverinfo")),style = "font-size:2.0rem;color: black;")),
                  column(4,actionButton(ns("dump_annotations"), "Reset all annotations")),
                  column(12,div(style = "height:30px"))))),
      ##three tabs
      column(12,tabBox(width=12,
      ## 1st tab
      tabPanel("VUMC vs MGB",
        fluidRow(
        column(6,withSpinner(plotOutput(ns("plot1_vandy_mgh"),width="600px",height="600px",
                                              click = clickOpts(ns("point_click")),
                                              dblclick = dblclickOpts(ns("point_dblclick")),
                                              hover = hoverOpts(id=ns("point_hover")),
                                              brush = brushOpts(id=ns("point_brush")))),
               downloadButton(ns("manhattan1"))
              ),
        column(6,
               withSpinner(plotOutput(ns("plot2_vandy_mgh"),width='600px',height = "600px",
                                      click = ns("point_click"),
                                      hover = hoverOpts(id=ns("point_hover")),
                                      brush = brushOpts(id=ns("point_brush")))),

               downloadButton(ns("scatter1"))
        ),

        column(12,div(style = "height:30px")),

        column(12,div(style = "height:30px")),

        box(width=12,title=strong("Annotated Points: points annotated in the plot will be highlighted below"),
            solidHeader = F,status="warning",
            column(6,""),
            # column(3,div(actionButton(ns("bring_top_vandy_mgh"), "Bring selected to the top"),style="float:right")),
            column(3,div(actionButton(ns("update_plot_vandy_mgh"), "Update manhattan/scatter plot"),style="float:right")),
            column(3,div(downloadButton(ns("downloadtable1"), "Download selected rows"),style="float:right")),
            column(12,div(style = "height:30px")),
            column(12,fluidRow(
              div(dataTableOutput(ns("selected_codes_table1")))))
        ),
        box(width=12,title=strong("associationSubgraphs: annotated points are colored"),solidHeader = F,status="warning",
            column(12,fluidRow(
                        column(12,div(actionButton(ns("update_subgraph_vandy_mgh"), "Visualize associationSubgraphs"),
                                      style="float:right")),
                        column(12,div(style = "height:30px")),
                        column(12,uiOutput(ns("spinner1")))
                        # div(p("Github: https://github.com/tbilab/associationsubgraphs")),
                        # div(p("Reference: Strayer,N. et al. (2022) [Interactive network-based clustering and investigation of multimorbidity association matrices with associationSubgraphs](https://doi.org/10.1093/bioinformatics/btac780). Bioinformatics, 39, btac780."))
                        ))
           )
        )
      ), #tabPanel

      tabPanel("VUMC vs UKB",
               fluidRow(
                 column(6,withSpinner(plotOutput(ns("plot1_vandy_ukbb"),width="600px",height="600px",
                                                     click = clickOpts(ns("point_click")),
                                                     dblclick = dblclickOpts(ns("point_dblclick")),
                                                     hover = hoverOpts(id=ns("point_hover")),
                                                     brush = brushOpts(id=ns("point_brush")))),
                        downloadButton(ns("manhattan2"))
                 ),
                 column(6,
                        withSpinner(plotOutput(ns("plot2_vandy_ukbb"),width='600px',height = "600px",
                                               click = ns("point_click"),
                                               hover = hoverOpts(id=ns("point_hover")),
                                               brush = brushOpts(id=ns("point_brush")))),

                        downloadButton(ns("scatter2"))
                 ),

                 column(12,div(style = "height:30px")),

                 box(width=12,title=strong("Annotated Points: points annotated in the plot will be highlighted below"),
                     solidHeader = F,status="warning",
                     column(6,""),
                     # column(3,div(actionButton(ns("bring_top_vandy_ukbb"), "Bring selected to the top"),style="float:right")),
                     column(3,div(actionButton(ns("update_plot_vandy_ukbb"), "Update manhattan/scatter plot"),style="float:right")),
                     column(3,div(downloadButton(ns("downloadtable2"), "Download selected rows"),style="float:right")),
                     column(12,div(style = "height:30px")),
                     column(12,fluidRow(
                       div(dataTableOutput(ns("selected_codes_table2")))))
                 ),

                 column(12,div(style = "height:30px")),

                 box(width=12,title=strong("associationSubgraphs: annotated points are colored"),solidHeader = F,status="warning",
                     column(12,fluidRow(
                       column(12,div(actionButton(ns("update_subgraph_vandy_ukbb"), "Visualize associationSubgraphs"),
                                     style="float:right")),
                       column(12,div(style = "height:30px")),
                       column(12,uiOutput(ns("spinner2")))
                       # div(p("Github: https://github.com/tbilab/associationsubgraphs")),
                       # div(p("Reference: Strayer,N. et al. (2022) [Interactive network-based clustering and investigation of multimorbidity association matrices with associationSubgraphs](https://doi.org/10.1093/bioinformatics/btac780). Bioinformatics, 39, btac780."))
                       ))
                 )
               )
      ), #tabPanel

      tabPanel("MGB vs UKB",
               fluidRow(
                 column(6,withSpinner(plotOutput(ns("plot1_mgh_ukbb"),width="600px",height="600px",
                                                 click = clickOpts(ns("point_click")),
                                                 dblclick = dblclickOpts(ns("point_dblclick")),
                                                 hover = hoverOpts(id=ns("point_hover")),
                                                 brush = brushOpts(id=ns("point_brush")))),
                        downloadButton(ns("manhattan3"))
                 ),
                 column(6,
                        withSpinner(plotOutput(ns("plot2_mgh_ukbb"),width='600px',height = "600px",
                                               click = ns("point_click"),
                                               hover = hoverOpts(id=ns("point_hover")),
                                               brush = brushOpts(id=ns("point_brush")))),

                        downloadButton(ns("scatter3"))
                 ),

                 column(12,div(style = "height:30px")),

                 box(width=12,title=strong("Annotated Points: points annotated in the plot will be highlighted below"),
                     solidHeader = F,status="warning",
                     column(6,""),
                     # column(3,div(actionButton(ns("bring_top_mgh_ukbb"), "Bring selected to the top"),style="float:right")),
                     column(3,div(actionButton(ns("update_plot_mgh_ukbb"), "Update manhattan/scatter plot"),style="float:right")),
                     column(3,div(downloadButton(ns("downloadtable3"), "Download selected rows"),style="float:right")),
                     column(12,div(style = "height:30px")),
                     column(12,fluidRow(
                       div(dataTableOutput(ns("selected_codes_table3")))))
                 ),

                 column(12,div(style = "height:30px")),

                 box(width=12,title=strong("associationSubgraphs: annotated points are colored"),solidHeader = F,status="warning",
                     column(12,fluidRow(
                       column(12,div(actionButton(ns("update_subgraph_mgh_ukbb"), "Visualize associationSubgraphs"),
                                     style="float:right")),
                       column(12,div(style = "height:30px")),
                       column(12,uiOutput(ns("spinner3")))
                       # div(p("Github: https://github.com/tbilab/associationsubgraphs")),
                       # div(p("Reference: Strayer,N. et al. (2022) [Interactive network-based clustering and investigation of multimorbidity association matrices with associationSubgraphs](https://doi.org/10.1093/bioinformatics/btac780). Bioinformatics, 39, btac780."))
                       ))
                 )
               )
      ) #tabPanel
      )) #tabbox
    ) ##fluidRow
  ) ##tagList
}

annotationPlotServer <- function(id, code_id, code_data, type, type_label,plot_fn1, plot_fn2, code_description) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #===============================================
      # Update selected code
      #===============================================
      observeEvent(code_id(),{
        output$current_code_label <- renderText({
          glue("Current selection: {code_id()}")
        })
      })

      #===============================================
      # Watch for manhattan plot code selection
      #===============================================
      annotated_points <- reactiveVal(c(0)) #change into reactive Values
      ##brush
      observeEvent(input$point_brush, {
        selected_point <- brushedPoints(code_data(), input$point_brush)$phecode
        old_selection <- annotated_points()
        already_selected <- selected_point %in% old_selection
        if(sum(already_selected)>=1){
          # Remove the point from the selection if it's been clicked again
          annotated_points(c(old_selection[!(old_selection %in% selected_point)],
                             selected_point))
        } else {
          # otherwise add it to the selection
          annotated_points(c(old_selection, selected_point))
        }
        #remove the previous selection, solve the flashing twice issue since the previous box still there if not removing it
        session$resetBrush(input$point_brush$brushId)
      })
      ##click
      observeEvent(input$point_click, {

        selected_point <- nearPoints(code_data(), input$point_click)$phecode
        old_selection <- annotated_points()
        already_selected <- selected_point %in% old_selection
        if(sum(already_selected)>=1){
          # Remove the point from the selection if it's been clicked again
          annotated_points(c(old_selection[!(old_selection %in% selected_point)],
                             selected_point))
        } else {
          # otherwise add it to the selection
          annotated_points(c(old_selection, selected_point))
        }
      })
      ##double click, remove selection
      observeEvent(input$point_dblclick, {
        selected_point <- nearPoints(code_data(), input$point_dblclick)$phecode
        old_selection <- annotated_points()
        annotated_points(old_selection[!(old_selection %in% selected_point)])
      })

      #===============================================
      #watch for remove selection button
      #===============================================
      observeEvent(input$dump_annotations, {
        annotated_points(c(0))
      })
      ## when user change into another selected code
      observeEvent(code_description(),annotated_points(c(0)))
      # observeEvent(code_description(), updateActionButton(session$ns,update_subgraph_vandy_mgh,))

      #======================================================
      #specify the institution
      #======================================================
      #===============================================
      ##watch for associationsubgraphs button click
      #===============================================
      observeEvent(input$update_subgraph_vandy_mgh,{
      
        output$plot3_vandy_mgh <- r2d3::renderD3({
          withProgress(message = "",
                       value=0,{
          incProgress(0.5,detail = "associationSubgraphs are running")               
          subgraph = comorbidity_subnetwork(com_sim,isolate(annotated_points()),
                                 paste0(glue("{(type_label)}_vandy_mgh")),isolate(code_description()))
          incProgress(0.4, detail = "Nearly done, associationSubgraphs will show in 10 seconds!")
          Sys.sleep(5)
          })
          subgraph
        })
        
        output$spinner1 = renderUI({
          withSpinner(r2d3::d3Output(session$ns("plot3_vandy_mgh"),width = "100%", height = "690px"),
                      hide.ui = F)
        })
        removeModal()
      })

      observeEvent(input$update_subgraph_vandy_ukbb,{
        output$plot3_vandy_ukbb <- r2d3::renderD3({
          withProgress(message = "",
                       value=0,{
                         incProgress(0.5,detail = "associationSubgraphs are running")                
                         subgraph = comorbidity_subnetwork(com_sim,isolate(annotated_points()),
                                                           paste0(glue("{(type_label)}_vandy_ukbb")),isolate(code_description()))
                         incProgress(0.4, detail = "Nearly done, associationSubgraphs will show in 10 seconds!")
                         Sys.sleep(5)
                       })
          subgraph
        })
        
        output$spinner2 = renderUI({
          withSpinner(r2d3::d3Output(session$ns("plot3_vandy_ukbb"),width = "100%", height = "690px"),
                      hide.ui = FALSE)
        })
        removeModal()
      })

      observeEvent(input$update_subgraph_mgh_ukbb,{
        output$plot3_mgh_ukbb <- r2d3::renderD3({
          withProgress(message = "",
                       value=0,{
                         incProgress(0.5,detail = "associationSubgraphs are running")                
                         subgraph = comorbidity_subnetwork(com_sim,isolate(annotated_points()),
                                                           paste0(glue("{(type_label)}_mgh_ukbb")),isolate(code_description()))
                         incProgress(0.4, detail = "Nearly done, associationSubgraphs will show in 10 seconds!")
                         Sys.sleep(5)
                       })
          subgraph
        })
        
        output$spinner3 = renderUI({
          withSpinner(r2d3::d3Output(session$ns("plot3_mgh_ukbb"),width = "100%", height = "690px"),
                      hide.ui = FALSE)
        })
        removeModal()
      })

      #===============================================
      #update manhattan/scatter once table selection is updated
      #===============================================
      observeEvent(input$update_plot_vandy_mgh, {
        if(is.null(input$selected_codes_table1_rows_selected)){
          return()
          } else {
        codes_to_keep <- code_data_vandy_mgh()$phecode[input$selected_codes_table1_rows_selected]
        annotated_points(c(codes_to_keep))
        }
        })

      observeEvent(input$update_plot_vandy_ukbb, {
        if(is.null(input$selected_codes_table2_rows_selected)){
          return()
        } else {
          codes_to_keep <- code_data_vandy_ukbb()$phecode[input$selected_codes_table2_rows_selected]
          annotated_points(codes_to_keep)
        }
      })

      observeEvent(input$update_plot_mgh_ukbb, {
        if(is.null(input$selected_codes_table3_rows_selected)){
          return()
        } else {
          codes_to_keep <- code_data_mgh_ukbb()$phecode[input$selected_codes_table3_rows_selected]
          annotated_points(codes_to_keep)
        }
      })

      #===============================================
      #bring selected to top
      #===============================================
      #===============================================
      ##table output of selected phecodes
      #===============================================
      code_data_vandy_mgh = reactive({code_data() %>% drop_na(glue("{(type_label)}_vandy_mgh")) %>% arrange(desc(glue("{(type_label)}_vandy_mgh")))})
      
      table_data1 = reactive({
        code_data_vandy_mgh() %>%
          dplyr::select(phecode,description,category,glue("{(type_label)}_vandy"),
                        glue("{(type_label)}_mgh"),
                        glue("{(type_label)}_ukbb")) %>%
          mutate(across(4:6, round, 3))
      })
      selected_rows1 <- reactive({
        if(length(annotated_points()>1)){
          ifelse(seq_len(nrow(code_data_vandy_mgh())) %in% which(code_data_vandy_mgh()$phecode %in% annotated_points()),
                 seq_len(nrow(code_data_vandy_mgh())),0)
        } else{
          seq_len(0)
        }
      })
      
      # observeEvent(input$bring_top_vandy_mgh,{
      #   selected_rows = input$selected_codes_table1_rows_selected
      #   
      #   # calculate new row order
      #   row_order <- order(
      #     seq_len(nrow(code_data_vandy_mgh())) %in% selected_rows,
      #     decreasing = TRUE
      #   )
      #   
      #   proxy <- DT::dataTableProxy('selected_codes_table1')
      #   DT::replaceData(proxy, table_data1()[row_order, ])
      #   # make sure to select the rows again
      #   DT::selectRows(proxy, seq_along(selected_rows))
      # 
      #   })
        
        output$selected_codes_table1 = DT::renderDataTable({
          
          table_data1() %>%
            datatable(
              options = list(scrollY = 600,
                             scroller = TRUE,
                             dom = 'ft',
                             # order = list(list(3, 'asc')),
                             pageLength = nrow(.)),
              selection = list(mode='multiple',selected = selected_rows1()))
      })
      
      # proxy = dataTableProxy('selected_codes_table1')
      # observeEvent(input$update_plot_vandy_mgh,{
      #   replaceData(proxy, formattable(table_data1(),
      #                                  options = list(iDisplayLength = 100),
      #                                  selection = list(mode='multiple',selected = selected_rows1())), resetPaging = FALSE)
      # })
      # values1 <- reactiveValues(
      #   options = list(
      #     # sDom  = '<"top">rt<"bottom">ip',
      #                  stateSave = TRUE,
      #                  # scrollY = 700,
      #                  # scroller = TRUE,
      #                  iDisplayLength = 100,
      #                  # dom = 'ft',
      #                  order = list())
      # )
      # observeEvent(input$selected_codes_table1_state$order, {
      #   values1$options$order <- input$selected_codes_table1_state$order
      # })
      
      ## download selected rows
      output$downloadtable1 <- downloadHandler(
        filename = function() {
          paste0("selected_phecodes_table",Sys.time(),".xlsx")
        },
        content = function(file) {
          openxlsx::write.xlsx(table_data1()[input$selected_codes_table1_rows_selected,] %>% 
                                 mutate(description = str_replace_all(description,",","_")), file)
        }
      )
      
      #vandy vs ukbb
      code_data_vandy_ukbb = reactive({code_data() %>% drop_na(glue("{(type_label)}_vandy_ukbb")) %>% arrange(desc(glue("{(type_label)}_vandy_ukbb")))})
      table_data2 = reactive({
        code_data_vandy_ukbb() %>%
          dplyr::select(phecode,description,category,glue("{(type_label)}_vandy"),
                        glue("{(type_label)}_mgh"),
                        glue("{(type_label)}_ukbb")) %>%
          mutate(across(4:6, round, 3))
      })
      selected_rows2 <- reactive({
        if(length(annotated_points()>1)){
          ifelse(seq_len(nrow(code_data_vandy_ukbb())) %in% which(code_data_vandy_ukbb()$phecode %in% annotated_points()),
                 seq_len(nrow(code_data_vandy_ukbb())),0)
        } else{
          seq_len(0)
        }
      })
      
      # observeEvent(input$bring_top_vandy_ukbb,{
      #   selected_rows = input$selected_codes_table2_rows_selected
      #   
      #   # calculate new row order
      #   row_order <- order(
      #     seq_len(nrow(code_data_vandy_ukbb())) %in% selected_rows,
      #     decreasing = TRUE
      #   )
      #   
      #   proxy <- DT::dataTableProxy('selected_codes_table2')
      #   DT::replaceData(proxy, table_data2()[row_order, ])
      #   # make sure to select the rows again
      #   DT::selectRows(proxy, seq_along(selected_rows))})
      
      
      output$selected_codes_table2 = DT::renderDataTable({
        
        table_data2() %>%
                  datatable(
                    options = list(scrollY = 600,
                                   scroller = TRUE,
                                   dom = 'ft',
                                   # order = list(list(3, 'asc')),
                                   pageLength = nrow(.)),
                    selection = list(mode='multiple',selected = selected_rows2()))
        
      })
      
      
      
      ## download selected rows
      output$downloadtable2 <- downloadHandler(
        filename = function() {
          paste0("selected_phecodes_table",Sys.time(),".xlsx")
        },
        content = function(file) {
          openxlsx::write.xlsx(table_data2()[input$selected_codes_table2_rows_selected,] %>% 
                                 mutate(description = str_replace_all(description,",","_")), file)
        }
      )
      
      ## mgh vs ukbb
      code_data_mgh_ukbb = reactive({code_data() %>% drop_na(glue("{(type_label)}_mgh_ukbb")) %>% arrange(desc(glue("{(type_label)}_mgh_ukbb")))})
      table_data3 = reactive({
        code_data_mgh_ukbb() %>%
          dplyr::select(phecode,description,category,glue("{(type_label)}_vandy"),
                        glue("{(type_label)}_mgh"),
                        glue("{(type_label)}_ukbb")) %>%
          mutate(across(4:6, round, 3))
      })
      selected_rows3 <- reactive({
        if(length(annotated_points()>1)){
          ifelse(seq_len(nrow(code_data_mgh_ukbb())) %in% which(code_data_mgh_ukbb()$phecode %in% annotated_points()),
                 seq_len(nrow(code_data_mgh_ukbb())),0)
        } else{
          seq_len(0)
        }
      })
      
      # observeEvent(input$bring_top_mgh_ukbb,{
      #   selected_rows = input$selected_codes_table3_rows_selected
      #   
      #   # calculate new row order
      #   row_order <- order(
      #     seq_len(nrow(code_data_mgh_ukbb())) %in% selected_rows,
      #     decreasing = TRUE
      #   )
      #   
      #   proxy <- DT::dataTableProxy('selected_codes_table3')
      #   DT::replaceData(proxy, table_data3()[row_order, ])
      #   # make sure to select the rows again
      #   DT::selectRows(proxy, seq_along(selected_rows))})
      
      
      output$selected_codes_table3 = DT::renderDataTable({
        
        table_data3() %>%
                  datatable(
                    options = list(scrollY = 600,
                                   scroller = TRUE,
                                   dom = 'ft',
                                   # order = list(list(3, 'asc')),
                                   pageLength = nrow(.)),
                    selection = list(mode='multiple',selected = selected_rows3()))
        
      })
      
      ## download selected rows
      output$downloadtable3 <- downloadHandler(
        filename = function() {
          paste0("selected_phecodes_table",Sys.time(),".xlsx")
        },
        content = function(file) {
          
          write.xlsx(table_data3()[input$selected_codes_table3_rows_selected,] %>% 
                       mutate(description = str_replace_all(description,",","_")), file, 
                     col.names = TRUE, row.names = F, append = FALSE)
        }
      )

      #===============================================
      ##manhattan plot
      #===============================================
      #vandy & mgh
      output$plot1_vandy_mgh <- renderPlot({

        if(nrow(code_data_vandy_mgh())!=0){
          m_plot = function(){
            plot_fn1(
            code_id,
            mutate(code_data_vandy_mgh(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy_mgh",
            "yes"
          )}
        } else{
          m_plot = function(){plot_fn1(
            code_id,
            mutate(code_data_vandy_mgh(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy_mgh",
            "no"
          )}
        }
        ggsave("manhattan1.png",m_plot())
        m_plot()
      })

      output$plot2_vandy_mgh <- renderPlot({

        if(nrow(code_data_vandy_mgh())!=0){
          s_plot = function(){plot_fn2(
            code_id,
            mutate(code_data_vandy_mgh(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy","mgh",
            "yes"
          )}
        } else{
          s_plot = function(){plot_fn2(
            code_id,
            mutate(code_data_vandy_mgh(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy","mgh",
            "no"
          )}
        }
        ggsave("scatter1.png",s_plot())
        s_plot()
      })

      output$plot1_vandy_ukbb <- renderPlot({

        if(nrow(code_data_vandy_ukbb())!=0){
          m_plot = function(){
            plot_fn1(
              code_id,
              mutate(code_data_vandy_ukbb(), is_labeled = phecode %in% annotated_points()),
              type,
              type_label,
              "vandy_ukbb",
              "yes"
            )}
        } else{
          m_plot = function(){plot_fn1(
            code_id,
            mutate(code_data_vandy_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy_ukbb",
            "no"
          )}
        }
        ggsave("manhattan2.png",m_plot())
        m_plot()
      })
      output$plot2_vandy_ukbb <- renderPlot({

        if(nrow(code_data_vandy_ukbb())!=0){
          s_plot = function(){plot_fn2(
            code_id,
            mutate(code_data_vandy_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy","ukbb",
            "yes"
          )}
        } else{
          s_plot = function(){plot_fn2(
            code_id,
            mutate(code_data_vandy_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "vandy","ukbb",
            "no"
          )}
        }
        ggsave("scatter2.png",s_plot())
        s_plot()
      })

      output$plot1_mgh_ukbb <- renderPlot({

        if(nrow(code_data_mgh_ukbb())!=0){
          m_plot = function(){
            plot_fn1(
              code_id,
              mutate(code_data_mgh_ukbb(), is_labeled = phecode %in% annotated_points()),
              type,
              type_label,
              "mgh_ukbb",
              "yes"
            )}
        } else{
          m_plot = function(){plot_fn1(
            code_id,
            mutate(code_data_mgh_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "mgh_ukbb",
            "no"
          )}
        }
        ggsave("manhattan3.png",m_plot())
        m_plot()
      })
      output$plot2_mgh_ukbb <- renderPlot({

        if(nrow(code_data_mgh_ukbb())!=0){
          s_plot = function(){plot_fn2(
            code_id,
            mutate(code_data_mgh_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "mgh","ukbb",
            "yes"
          )}
        } else{
          s_plot = function(){plot_fn2(
            code_id,
            mutate(code_data_mgh_ukbb(), is_labeled = phecode %in% annotated_points()),
            type,
            type_label,
            "mgh","ukbb",
            "no"
          )}
        }
        ggsave("scatter3.png",s_plot())
        s_plot()
      })
      
      ##output of hover points information
      output$plot_hoverinfo <- renderText({
        hover_phecode = nearPoints(code_data(), input$point_hover)$phecode[1]
        hover_description = nearPoints(code_data(), input$point_hover)$description[1]
        if(!is.na(hover_phecode)) {
          paste0(hover_phecode,"-",hover_description)
        } else {""}
      })

    })
}












