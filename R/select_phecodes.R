select_phe = function(id){
  ns <- NS(id)
  fluidRow(
    column(width=12,div(
      span(textOutput(ns("current_code_label"),inline = TRUE),style = "font-size:2.5rem;color: black;center;center;"),
      style = "padding-top: 10px;padding-bottom: 5px;
                     margin-top: 3px;display: flex;align-items:
                     center;justify-content: space-evenly;")),

    column(width=12,box(width=12,
                        title = strong("Select code of interest",style="font-size: 2.0rem;"),
                        status="warning",solidHeader = FALSE,
                        div(
                          p("Please click on the rows to select code of interest, click twice to unselect;",
                            style = "text-align: left;font-size: 2.0rem; color:black;"),
                          p("You could search the phecode by typing in the 'Search' box;",
                            style = "text-align: left;font-size: 2.0rem; color:black;")
                        ),
                        hr(),
                        DTOutput(ns('code_selection'))
    ))
  )

}

select_pheServer = function(id,start_index,code_id){
  moduleServer(
    id,
    function(input,output,session){
      output$code_selection <- renderDT({
              datatable(phecodes,
              rownames = FALSE,
              options = list(displayStart = start_index - 2),
              selection = list(mode = 'single', selected = start_index)
              )},server = FALSE)
      output$current_code_label <- renderText(glue("Current selection: {code_id()}"))
      }
  )
}









