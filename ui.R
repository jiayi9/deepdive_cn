ui <- fluidPage(
  
  
  tags$body(tags$script(src="iframeResizer.contentWindow.min.js")),
  
  theme = "style.css",
  div(class="small_text",
    
    navbarPage("",
               tabPanel("主页",icon = icon("upload"),
                        br(),br(),br(),
                        
                        
                        fluidRow(
                          column(7,class = "grey",
                                 


                                 h3("#1 上传与设置"),
                                 
                                 fluidRow(
                                   column(5,

                                          fileInput('file1', '',
                                                    accept=c('text/csv', 
                                                             'text/comma-separated-values,text/plain', 
                                                             '.csv')),
                                          textOutput("info"),
                                          uiOutput("EXCLUDE"),
                                          uiOutput("TARGET_UI"),
                                          uiOutput("TARGET_DEFINE_UI"),
                                          checkboxInput("define_non_empty_as_fail",label = "空值标为Fail",value = FALSE)
                                          ,downloadButton('downloadAGG', '下载聚集好的数据',class="btn btn-default btn-xs")
                                          
                         
                                          
                                          ),
                                   column(5,
                                          br(),

                                          uiOutput("AGG_UI"),
                                          uiOutput("HEAD_OR_DRIVE"),
                                          checkboxInput("no_agg",label = "不聚集数据",value = FALSE),
                                          numericInput(inputId="max_p",label="类别变量P值阈值",value=0.1,min=0.01,max=1,step=0.01),
                                          numericInput(inputId="max_level",label="类别变量最多层数",value=15,min=3,max=50,step=1),
                                          helpText("上传新数据前，请刷新页面"),
                                          
                                          fluidRow(column(7,""),column(5,
                                                                       #br(),
                                                                       actionButton("update","处理数据",class="btn btn-primary btn-sm")#icon=icon("fa fa-check")),
                                                                       
                                                                       ))
                                          
                                          

                                          
                                          ),
                                   column(1,
                                          br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                          

                                          
                                         )
                                   )
                                 #narrow fluid row ends up
                                 ),
                          column(4,class = "grey",
                                 h3("#3 分析"),
                                 
                                 checkboxInput("show_advanced_settings","显示高级设置",value = FALSE),
                                 fluidRow(
                                   column(6,
                                          conditionalPanel(condition = "input.show_advanced_settings ==1",
                                                           numericInput(inputId = "chisq_tol",label="卡方检验最小组百分比阈值",value=0.05,min=0.01,max=0.99,step=0.01),
                                                           numericInput("para_missing_percent","忽略缺失百分比超过以下值的参数",
                                                                        value = 0.2,min = 0.01,max = 0.5,step = 0.01),
                                                           
                                                           numericInput("para_unique","忽略不同值数小于以下数值的数值变量",
                                                                        value = 20,min = 10, max = 100,step = 1),
                                                           numericInput("attr_many_levels_cutoff","区别分析不同值数超过以下数量的类别变量",
                                                                        value = 30,min=20,max=100,step=1)
                                          )
                                          
                                          ),
                                   column(6,
                                          conditionalPanel(condition = "input.show_advanced_settings ==1",
                                                           numericInput("max_p_para","数值变量逻辑回归P值阈值",
                                                                        value = 0.05,min = 0.01,max = 1, step =0.01),
                                                           numericInput("attr_clust_h",
                                                                        "类别变量聚类分组敏感度",
                                                                        value = 0.5,
                                                                        min = 0.01,max=1,step=0.01),
                                                           numericInput("para_clust_h",
                                                                        "数值变量聚类分组敏感度",
                                                                        value = 0.5,
                                                                        min = 0.01,max=1,step=0.01),

                                                           selectInput("treatNA","缺失值处理方法",
                                                                       choices = c("Exclude in analysis","Treat as a group"))
                                            )
                                          )
                                   ),

                                 
                                 
                                 actionButton("analyze","开始分析!",class="btn btn-primary btn-sm")
                                 ,helpText("在顶部其他分页中参看结果")
                                 
                                 )
                          
                          ),
                        #wide fluid row ends up
                        h3("#2 定义需分析的类别变量"),
                        
                        fluidRow(class="grey2",
                                 
                                 column(4,class="grey2",
                                        h4("待选类别变量"),
                                        tableOutput("left_table"),
                                        helpText("错误码401代表卡方检验无法实施")
                                 ),
                                 
                                 column(5,
                                        br(),
                                        br(),br(),
                                        p(""),

                                        

                                        
                                        uiOutput("CHOOSER")
                                 ),
                                 column(3,class="grey2",
                                        h4("已选类别变量"),
                                        tableOutput("right_table")
                                 )
                                 
                                 
                        )


                        #h4("upload log"),
                        #verbatimTextOutput("upload_log"),
                        #h4("exclude"),

                        #h4("TARGET_UI"),

                        #h4("TARGET_UI_log"),
                        #verbatimTextOutput("TARGET_UI_log"),
                        #h4("TARGET_log"),
                        
                        #verbatimTextOutput("TARGET_log"),
                        

                        
                        
                        #verbatimTextOutput("define_non_empty_as_fail_log"),
                        
                        #h4("agg_log"),
                        
                        #verbatimTextOutput("agg_log"),
                        #verbatimTextOutput("head_or_drive_log"),
                        
                        
               )
               #tabPanel upload&filter ends up
               
               ,tabPanel("数据探索",icon = icon("cube"),
                         p(""),
                         br(),br(),br(),
                         radioButtons("pivot_level","探索以下数据",inline = TRUE,selected = "raw uploaded",
#                                       choices = c("原始数据" = 2,
#                                                   "处理过的行级别数据"=0,
#                                                   "处理过的聚集级别数据"=1
#                                                   )
                                      choices = c("raw uploaded",
                                                  "row level data",
                                                  "aggregate level data"
                                      )
                                      
                         ),
                         rpivotTable::rpivotTableOutput("pivot")
               )
               
               
               ,navbarMenu("类别变量",icon = icon("pie-chart"),
                           tabPanel("分析结果汇总",
                                    br(),br(),br(),
                                    h4("类别变量：单维度聚类柱状图"),
                                    textOutput("barplot_text"),
                                    uiOutput("hist_1"),
                                    uiOutput("hist_2_x"),
                                    fluidRow(
                                      column(2,
                                             numericInput("onewaybarheight","图形高度",value = 450,step=10,width = 100)
                                             
                                             ),
                                      column(2,
                                             br(),
                                             checkboxInput("showbarcolor","显示分类颜色",value = TRUE)
                                             
                                             )
                                      ),
                                    uiOutput("ATTR_CLUST_CHART")
                                    
                                    
                                    ),
                           tabPanel("其他分析",
                                    br(),br(),br(),
                                    h4("双因素柱状图"),
                                    fluidRow(
                                      column(2,
                                             uiOutput("factor1"),
                                             uiOutput("factor2"),
                                             uiOutput("two_way_size")
                                             ),
                                      column(10,
                                             uiOutput("TWO_WAY_BARPLOT")
                                             
                                             )
                                      ),

                                    hr(),
                                    
                                    
                                    h4("多层级类别变量分析"),
                                    fluidRow(
                                      column(4,
                                             uiOutput("factor3")

                                             
                                      ),
                                      column(2,
                                             br(),
                                             checkboxInput("attr_many_levels_rank","打分排序",value = FALSE)
                                             
                                             
                                      ),
                                      column(6,
                                             uiOutput("custom_barplot_size")
                                             
                                             
                                      )

                                      
                                      
                                    ),
                                    uiOutput("CUSTOM_BARPLOT"),
                                    h4("汇总信息"),
                                    uiOutput("text_rank")
                                    #dataTableOutput("attr_many_levels_summary")

#                                     p("ATTR_many_levels"),
#                                     verbatimTextOutput("ATTR_many_levels_log"),
#                                     p("ATTR_many_levels_drive"),
#                                     verbatimTextOutput("ATTR_many_levels_drive_log"),
#                                     p("attr_many_levels"),
#                                     verbatimTextOutput("attr_many_levels_log"),
#                                     plotOutput("two_way_barplot"),


                                    
                                    
                                    )
                           ),
#navMenu attributes ends up

                          tabPanel("数值变量",icon = icon("area-chart"),
                                   br(),br(),br(),
                                   h4("数值变量分析汇总"),
                                   uiOutput("para"),
                                   hr(),
                                   h4("显著性排序与散点图"),
                                   fluidRow(
                                     column(3,
                                            div(class="small_table",
                                                tableOutput("ranked_para_names_table")
                                                #dataTableOutput("ranked_para_names")
                                            )
                                     ),
                                     column(9,
                                            uiOutput("parametrics")
                                     )
                                     
                                   )

                                   
                                   
                                  ),
# tabPanel Parametrics ends up
                
                        navbarMenu("交互分析",icon = icon("random"),
                                   tabPanel("交互分析汇总",
                                            br(),br(),br(),br(),
                                            actionButton("interaction","搜索所有的显著交互",,class="btn btn-primary btn-sm"),
                                            checkboxInput("use_attr_list","只分析预定义的类别变量",value = TRUE),

                                            checkboxInput("inter_log","对数Y轴",value = FALSE),
                                            textOutput("interaction_text"),
                                            uiOutput("boxplots_para")
                                            
                                            ),
                                   tabPanel("主类别变量交互分析",
                                            br(),br(),br(),
                                            
                                            
                                            
                                            fluidRow(
                                              column(3,uiOutput("dominant_ATTR")),
                                              column(3,uiOutput("second_PARA")),
                                              column(2,checkboxInput("rank_second_para","重要性排序",value = TRUE)
                                                     ,
                                                     checkboxInput("dominant_ATTR_log","对数Y轴",value = FALSE),
                                                     checkboxInput("use_attr_list_1","只分析预定义的类别变量",value = FALSE)
                                                     
                                                     ),
                                              column(2,numericInput("dominant_attr_width","图形宽度",value = 600,step = 10)),
                                              column(2,numericInput("dominant_attr_height","图形高度",value = 450,step=10))
                                            ),
                                            fluidRow(
                                              column(6,uiOutput("dominant_attr_BOXPLOT"))
                                              ,
                                              column(6,plotOutput("dominant_attr_cdf"))
                                            )
                                            ,
                                            
                                            hr(),
                                            h4("针对主类别变量最显著的数值变量信息汇总"),
                                            actionButton("dominant_attr_go","显示"),
                                            fluidRow(
                                              column(6,
                                                     uiOutput("dominant_attr_boxplots")
                                              ),
                                              column(6,
                                                     uiOutput("dominant_attr_CDF")
                                              )
                                              
                                            )
                                            

                                            ),
                                   tabPanel("主数值变量交互分析",
                                            br(),br(),br(),
                                            fluidRow(
                                              column(3,uiOutput("dominant_PARA")),
                                              column(3,uiOutput("second_ATTR")),
                                              column(2,
                                                     checkboxInput("rank_second_attr","重要性排序",value = TRUE)
                                                     ,
                                                     checkboxInput("dominant_PARA_log","对数Y轴",value = FALSE),
                                                     checkboxInput("use_attr_list_2","只分析预定义的类别变量",value = FALSE)
                                                     
                                                     ),
                                              column(2,numericInput("dominant_para_width","图形宽度",value = 600,step = 10)),
                                              column(2,numericInput("dominant_para_height","图形高度",value = 450,step=10))
                                            ),
                                            fluidRow(
                                              column(6,uiOutput("dominant_para_BOXPLOT"))
                                              ,
                                              column(6,plotOutput("dominant_para_cdf"))
                                              )
                                            ,
                                            hr(),
                                            h4("针对主数值变量最显著的类别变量信息汇总"),
                                            actionButton("dominant_para_go","显示"),
                                            
                                            fluidRow(
                                              column(6,
                                                     uiOutput("dominant_para_boxplots")
                                              ),
                                              column(6,
                                                     uiOutput("dominant_para_CDF")
                                              )
                                              
                                            )
                                            
                                            #tableOutput("Interaction_DF")
                                            
                                            )                                   
                                   )
#navpage interaction ends up
                              ,tabPanel("数据信息",icon = icon("table"),
                                        br(),br(),br(),
                                        div(class="small_table",
                                            fluidRow(
                                              column(4,
                                                        h4("原始数据信息"),
                                                        tableOutput("UPLOAD_summary")
                                                     
                                                     ),
                                              column(4,
                                                     div(class = "grey_small",
                                                         
                                                     h4("类别变量数据信息"),
                                                     tableOutput("ATTR_summary")
                                                     )
                                                     ),
                                              column(4,
                                                     h4("数值变量数据信息"),
                                                     tableOutput("PARA_summary")
                                                     )
                                              )
                                        ))

#                               ,tabPanel("Pivot",icon = icon("cube"),
#                                         p(""),
#                                         br(),br(),br(),
#                                         radioButtons("pivot_level","Construct the Pivot table for:",inline = TRUE,
#                                                      choices = c("Head level all data (raw data)"=0,
#                                                                  "Drive level attributes data"=1)
#                                                      
#                                                      ),
#                                         rpivotTableOutput("pivot")
#                                         )

#DataView ends up
                              ,tabPanel("报表生成",icon = icon("dedent"),
                                        br(),br(),br(),
                                        
                                        h3("在选中模块并下载报表前，请先浏一遍览所选中的模块！")      
                                        ,
                                        hr(),
                                        actionButton("reportSelectAll","全（不）选",class="btn btn-primary btn-sm"),
                                        br(),br(),
                                        checkboxGroupInput("reportList","生成包含以下模块的报表:",
                                                           choices = 
                                                             c(
                                                             "Attributes summary",
                                                             "Attributes clustering",
                                                             "Two way barplot",
                                                             "Attributes with many levels",
                                                             "Parametrics Summary",
                                                             "Parametrics clustering",
                                                             "Custom Parametric Plot",
                                                             "Interaction Summary",
                                                             "Attribute-Dominant Custom plots",
                                                             "Attribute-Dominant Rank",
                                                             "Parametric-Dominant Custom plots",
                                                             "Parametric-Dominant Rank"
                                                             ),
                                                           selected = ""
#                                                              c(
#                                                              "Attributes summary",
#                                                              "Attributes clustering",
#                                                              
#                                                              "Two way barplot",
#                                                              "Attributes with many levels",
#                                                              "Parametrics Summary",
#                                                              "Parametrics clustering",
#                                                              "Custom Parametric Plot",
#                                                              "Attribute Interaction Boxplot",
#                                                              "Parametric Interaction Boxplot"
#                                                            )
                                                           ),
                                        radioButtons('format', '报表文件格式', c('HTML', 'PDF', 'Word')),#,inline=TRUE),
                                        br(),
                                        downloadButton("download","生成并下载报表",class="btn btn-primary btn-sm")
                                        
                                        
                                        )
                          


#   
#  checkboxInput("disp_missing_as_group","Missing as a group",value = TRUE),
               
     ,windowTitle = "高级工业数据分析器", position = "fixed-top"   #navPage settings          
    )
    #navpage ends up
  ),
  #div ends up
  
  
  
  
#   h4("RAW:"),
#   verbatimTextOutput("RAW_log"),
#   
#   h4("ATTR:"),
#   verbatimTextOutput("ATTR_log"),
#   h4("Y"),
#   verbatimTextOutput("Y_log"),
#   h4("DSN"),
#   verbatimTextOutput("DSN_log"),
#   h4("ATTR_drive log"),
#   verbatimTextOutput("ATTR_drive_log"),
#   h4("ATTR_drive head"),
  #tableOutput("ATTR_drive_table") 

  
#   
#   h4("DSN_drive_log"),
#   verbatimTextOutput("DSN_drive_log"),
#   h4("Y_drive_log"),
#   verbatimTextOutput("Y_drive_log"),
#   
#   
#   h4("chisq table head"),
#   verbatimTextOutput("chisq_table_head_log"),
#   
#   
#   h4("chisq table drive"),
#   verbatimTextOutput("chisq_table_drive_log"),
#   
#   h4("chisq_table"),
#   verbatimTextOutput("chisq_table_log"),
#   
#   h4("attr_names_log"),
#   verbatimTextOutput("attr_names_log"),
  
#   p("SELECTED NAMES"),
#   verbatimTextOutput("SELECTED_NAMES_log"),
#   p("UNSELECTED NAMES"),
#   verbatimTextOutput("UNSELECTED_NAMES_log"),
  

#   p("attr_y_dsn_log"),
#   verbatimTextOutput("attr_y_dsn_log"),
#   p("attr_ranked_log"),
#   verbatimTextOutput("attr_ranked_log"),
  #plotOutput("attr_clust_chart"),

  
  

  
#   p("PARA_log"),
#   verbatimTextOutput("PARA_log"),
#   p("para_log"),
#   verbatimTextOutput("para_log"),
#   p("para_p_table"),
#   verbatimTextOutput("para_p_table_log"),
#   
#   p("scatterData_log"),
#   verbatimTextOutput("scatterData_log"),
#   p("scatter_color_log"),
#   verbatimTextOutput("scatter_color_log"),



  
  p(""),
br(),br(),br(),
  div(class="small_table",
  helpText("ning.h.he@seagate.com"),
  
  helpText("jiayi.l.lu@seagate.com")
  )
)
#fluidPage