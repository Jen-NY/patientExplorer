library(RSQLite)
library(timevis)
library(shinyBS)
library(plotly)

ui <- fluidPage(
  
  navbarPage("Patient explorer", inverse = TRUE,
             
             tabPanel("Queries",
                      # ----
                      sidebarLayout(
                        
                        sidebarPanel(
                          h4("Filter table"),
                          hr(),
                          uiOutput("flt_project"),
                          actionButton("selectall_flt_project", "Select all"),
                          actionButton("deselectall_flt_project", "Deselect all"),
                          hr(),
                          uiOutput("flt_diagnosis"),
                          actionButton("selectall_flt_diagnosis", "Select all"),
                          actionButton("deselectall_flt_diagnosis", "Deselect all"),
                          checkboxGroupInput("flt_diagnosis_na", "Include patients with unknown diagnosis:", choices = "NA",  selected = "NA"),
                          hr(),
                          conditionalPanel(
                            condition = "input.qyr_tables == 'Aliquots'",
                            uiOutput("flt_alqsampletype"),
                            actionButton("selectall_flt_alqsampletype", "Select all"),
                            actionButton("deselectall_flt_alqsampletype", "Deselect all"),
                            br(),
                            uiOutput("flt_alqcelltype"),
                            actionButton("selectall_flt_alqcelltype", "Select all"),
                            actionButton("deselectall_flt_alqcelltype", "Deselect all"),
                            br(),
                            checkboxGroupInput("flt_alqcelltype_na", label = NA, choices = "NA",  selected = "NA"),
                            checkboxGroupInput("flt_empty", "Used", choices = c("empty", "used", NA), selected = c("empty", "used", NA)),
                            hr()
                          ),
                          conditionalPanel(
                            condition = "input.qyr_tables == 'Analysis'",
                            uiOutput("flt_analysis"),
                            actionButton("selectall_flt_analysis", "Select all"),
                            actionButton("deselectall_flt_analysis", "Deselect all"),
                            hr()
                          )
                        ),
                        
                        mainPanel(
                          tabsetPanel(id = "qyr_tables",
                                      tabPanel("Patients",
                                               br(),
                                               downloadButton("download_pat", "Download"),
                                               hr(),
                                               DT::dataTableOutput("qyr_pat")),
                                      tabPanel("Samples",
                                               br(),
                                               downloadButton("download_smp", "Download"),
                                               hr(),
                                               DT::dataTableOutput("qyr_smp")),
                                      tabPanel("Aliquots",
                                               br(),
                                               downloadButton("download_alq", "Download"),
                                               hr(),
                                               DT::dataTableOutput("qyr_alq")),
                                      tabPanel("Analysis",
                                               br(),
                                               downloadButton("download_anl", "Download"),
                                               hr(),
                                               DT::dataTableOutput("qyr_anl"))
                          ))
                        )
                      # ----
             ),
             
             tabPanel("Stored boxes",
                      # ----
                      
                      uiOutput("select_box"), 
                      br(),
                      tabsetPanel(
                        tabPanel("Table", 
                                 br(),
                                 downloadButton("download_tbl_box", "Download"), 
                                 hr(),
                                 DT::dataTableOutput("tbl_box")),
                        tabPanel("Plot", 
                                 br(),
                                 downloadButton("download_plot_box"),
                                 hr(),
                                 plotOutput("plot_box"))
                      )
                      # ----
             ),
             
             tabPanel("Genetic data",
                      
                      fluidRow(
                        column(6, uiOutput("selectPatientID")),
                        column(6, actionButton("makeReport", "Create report", class = "btn-primary"))
                      ),
                      
                      DT::dataTableOutput("exp_pat"),
                      
                      hr(),
                      
                      tabsetPanel(
                        
                        tabPanel("Samples",
                                 br(),
                                 bsCollapse(id = "collapseSample", multiple = TRUE, open = "Sample timeline", 
                                            bsCollapsePanel( "Sample timeline",
                                                             br(),
                                                             fluidRow(
                                                               column(2,actionButton("scale", "Scale timeline", class = "btn-primary")),
                                                               column(10, timevisOutput("timeline")) 
                                                             )
                                            )
                                 ),
                                 h4("Samples"),
                                 DT::dataTableOutput("exp_smp"),
                                 hr(),
                                 h4("Aliquots"),
                                 DT::dataTableOutput("exp_alq"),
                                 hr()
                      ),
                      
                      tabPanel("ampliconSeq",
                               br(),
                               DT::dataTableOutput("tbl_ampliconSeq_snv")
                               ),
                        tabPanel("WES",
                                 br(),
                                 h4("List of single nucleotide variants (SNVs)"),
                                 DT::dataTableOutput("tbl_wes_snv"),
                                 br(),
                                 h4("Copy number variation (CNV) plot"),
                                 uiOutput("plot_wes_cnv") 
                                 #img(src = "WES/16S0001_plot_wes_cnv.png", height = 800, width = 800)
                                 
                                 )
                        )
             )
             
  )
)
