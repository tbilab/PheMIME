library(here)
# setwd(here())
source("modules/load_libraries.R")
source("modules/data_loading.R")
source("modules/data_table.R")
source("modules/overview_consistency.R")
source("modules/comorbidity_network.R")
source("modules/annotation_module.R")
source("modules/manhattan_plot.R")
source("modules/scatter_plot.R")
source("modules/comorbidity_subnetwork.R")


# starting_code <- "295.10"
starting_code = "289.90"
# Used in data table to both select correct row and navigate table to that row
start_index <- which(phecodes$phecode == starting_code)

ui <- navbarPage(div(strong("Multi-Institutional Multimorbidity Explorer"),style="font-size: 2.2rem;"),
                 #color:#2c3e50
                 tags$style(HTML("
                            .navbar-default {background-color: #fff !important;font-size: 13px;}
                            .navbar-default .navbar-brand {color: #50595e;}
                            .navbar-default .navbar-nav > li > a:hover {color: #fff;background-color:#859900;}
                            /* warning status color  */
         .box.box-solid.box-warning>.box-header {
                              color:black;
                              background:#2c3e50;
                              /*#dfdfdf;*/
                              }
         .box.box-solid.box-warning{
                              border-bottom-color:white;
                              border-left-color:white;
                              border-right-color:white;
                              border-top-color:#2c3e50;
                              }
          .box.box-warning>.box-header {
                              color:#000000;
                              background:#fff
                    }
          .box.box-warning{
                              border-bottom-color:#2c3e50;
                              border-left-color:#2c3e50;
                              border-right-color:#2c3e50;
                              border-top-color:#2c3e50;
                              }"
                 )),
                 setBackgroundColor("#F2F3F6"),

                 #====================================================
                 #====================================================
                 #####tabs
                 useShinydashboard(),

                 #1st tab content
                 tabPanel(
                   strong("Disease Selection"),
                   # select_phe("select_phenotype")
                   fluidRow(
                     # column(width=12,div(
                     #   span(textOutput("current_code_label",inline = TRUE),style = "font-size:2.5rem;color: black;center;center;"),
                     #   style = "padding-top: 10px;padding-bottom: 5px;
                     # margin-top: 3px;display: flex;align-items:
                     # center;justify-content: space-evenly;")),

                     column(width=12,box(width=12,
                                         title = strong("Select phecode of interest",style="font-size: 2.0rem;"),
                                         status="warning",solidHeader = FALSE,
                                         div(
                                           p("To select a phecode of interest, click on the corresponding row. To deselect a phecode, simply click on the row again.",
                                             style = "text-align: left;font-size: 2.0rem; color:black;"),
                                           p("If you want to search for a specific phecode or filter for a subset of phecodes to select, you can simply type of your query into the 'Search' box.",
                                             style = "text-align: left;font-size: 2.0rem; color:black;")
                                         ),
                                         hr(),
                                         DTOutput('code_selection')
                     )),
                     hr(),
                     div(p("Designed by @Xu TBI Lab. All rights reserved.",
                           style="text-align: center;
                                  color: black; font-size: 1.5rem;")
                     ),
                     tags$div(style = "text-align: center;
                                  color: black; font-size: 1.5rem;",
                       "Your feedback is valuable to us! Please share your comments and suggestions by submitting an issue on our GitHub page",
                       tags$a(href="https://github.com/tbilab/PheMIME/issues", 
                              "Github Issues")
                     )
                   )
                 ),
                 #==================================================================================

                 #2nd tab content
                 tabPanel(
                   strong("Multimorbidity Consistency Inspection"),
                   consistency("comorbidity_consistency")
                 ),
                 #==================================================================================
                 #3rd tab content
                 tabPanel(
                   strong("Multimorbidity Network Visualization"),
                   comorbidity_networkPlot("comorbidity_network")
                 ),
                 #==================================================================================
                 #4th tab content
                 tabPanel(
                   strong("Reproducible Multimorbidities Exploration"),
                   annotationPlot("comorbidity")
                 ),
                 #==================================================================================
                 #5th tab content
                 tabPanel(
                   strong("Multimorbidity Similarities Exploration"),
                   annotationPlot("similarity")
                 ),
                 header = div(
                   span(
                     textOutput("current_code_label", inline = F), 
                     style = "font-size:2.5rem;color: black;center;center;"
                   ),
                   style = "
             padding-top: 5px;
             padding-bottom: 5px;
             margin-top: 0px;
             margin-bottom:5px;
             display: flex;
             align-items: center;
             justify-content: space-evenly;" 
                 )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  current_code <- reactive(phecodes$phecode[input$code_selection_rows_selected]) #phecode
  code_description = reactive(phecodes$description[input$code_selection_rows_selected]) #phecode description
  code_id <- reactive(glue("{current_code()} ({get_phecode_info(current_code())})"))
  current_comorbidities <- reactive(extract_and_diff(com_sim, code_description())) #comorbidity table of selected phecode with other phecodes
  current_similarities <- reactive(extract_and_diff(com_sim, code_description())) #similarity table of selected phecode with other phecodes

  ## select phenotype
  # select_pheServer(id="select_phenotype", start_index=start_index,code_id=code_id)
  output$code_selection <- renderDT({
    datatable(phecodes,
              rownames = FALSE,
              options = list(displayStart = start_index - 2),
              selection = list(mode = 'single', selected = start_index)
    )},server = FALSE)

  output$current_code_label <- renderText(glue("Current selection: {code_id()}"))

  ## overview consistency across three institutions
  consistencyPlotServer(id="comorbidity_consistency",
                        code_description=code_description,
                        code_id=code_id)

  ## comorbidity network
  # output$comorbidity_network = r2d3::renderD3({
  #    comorbidity_network(all_dat,code_description)
  # })
  comorbidity_networkServer(
    id = "comorbidity_network",
    #comorbidities = com_sim,
    code_description = code_description,
    code_id = code_id)

  #explore comorbidity
  ##comorbidity_manhattan and scatter plot across institutions
  annotationPlotServer(
    id = "comorbidity",
    code_id = code_id,
    code_data = current_comorbidities,
    type = "comorbidities",
    type_label = "z",
    plot_fn1 = gen_manhattan,
    plot_fn2 = gen_across_scatter,
    code_description = code_description
  )
  #
  # #explore similarity
  # ##similarity_manhattan and scatter plot across institutions
  annotationPlotServer(
    id = "similarity",
    code_id = code_id,
    code_data = current_similarities,
    type = "similarities",
    type_label = "sim",
    plot_fn1 = gen_manhattan,
    plot_fn2 = gen_across_scatter,
    code_description = code_description
  )
  #
  # #data table
  output$comorbidities_table <- renderDT(gen_table(code_id, current_comorbidities, "z"))
  output$download1 <- downloadHandler(
    filename = function(){glue("{code_id()}_comorbidity.csv")},
    content = function(fname){
      write.csv(gen_table(code_id, current_comorbidities, "z"), fname)
    }
  )

  output$similarities_table <- renderDT(gen_table(code_id, current_similarities, "sim"))
  output$download2 <- downloadHandler(
    filename = function(){glue("{code_id()}_similarity.csv")},
    content = function(fname){
      write.csv(gen_table(code_id, current_similarities, "sim"), fname)
    }
  )
  output$all_comorbidity_label = renderText(glue("All comorbidities for: {code_id()}"))
  output$all_similarity_label = renderText(glue("All similarities for: {code_id()}"))

  # output$markdown <- renderUI({
  #   HTML(rmarkdown::render('ComorbiditySimilarityExplorer_Manual.md'))
  # })

  # addResourcePath("tmpuser", getwd())
  # output$use_case <- renderUI({
  #   tags$iframe(seamless=NA,
  #               src= "tmpuser/ComorbiditySimilarityExplorer_Manual.html",
  #               width=800,
  #               height=800,
  #
  #               scrolling="no")
  # })

}

# Run the application
shinyApp(ui = ui, server = server)

















