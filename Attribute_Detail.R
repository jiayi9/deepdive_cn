

ATTR_many_levels = reactive({
  withProgress(message = 'Extracting attributes with many levels', {
    
    req(RAW(),Y())
    X = RAW()
    X = exclude_double(X)
    X[] = lapply(X,as.character)
    X[is.na(X)|X==""]="N/A"
    
    LEVELS = sapply(X,function(x) length(unique(x)))
    
    MAX_N = ATTR_MANY_LEVELS_CUTOFF()
    
    R = X[,LEVELS>=MAX_N & LEVELS<150, drop=FALSE]
    
    validate(need(ncol(R)>0,"No such attributes."))
    
    if(input$attr_many_levels_rank){
      R = data_ranked_Score(R,Y())$DATA      
    } else {
      R = R[,sort(names(R))]
    }
  })
  R
})

ATTR_many_levels_drive = reactive({
  withProgress(message = 'Extracting attributes with many levels', {
    
    req(RAW(),Y(),DSN())
    X = RAW()
    X = exclude_double(X)
    X[] = lapply(X,as.character)
    X[is.na(X)|X==""]="N/A"
    
    X = aggregate_data(group = DSN(),flag = Y(),X = X)$DATA
    
    LEVELS = sapply(X,function(x) length(unique(x)))
    
    MAX_N = ATTR_MANY_LEVELS_CUTOFF()
    
    
    R = X[,LEVELS>=MAX_N & LEVELS<150, drop=FALSE]
    
    validate(need(ncol(R)>0,"No such attributes."))
    
    if(input$attr_many_levels_rank){
      R = data_ranked_Score(R,y())$DATA      
    } else {
      R = R[,sort(names(R))]
    }
  })
  R
})

attr_many_levels = reactive({
  req(ATTR_many_levels(),ATTR_many_levels_drive())
  if(input$head_or_drive =="HEAD"){
    ATTR_many_levels()
  } else {
    ATTR_many_levels_drive()
  }
})

output$ATTR_many_levels_log =renderPrint({
  names(ATTR_many_levels())
})

output$ATTR_many_levels_drive_log =renderPrint({
  names(ATTR_many_levels_drive())
})

output$attr_many_levels_log =renderPrint({
  names(attr_many_levels())
})

output$twoway_text = renderText({
  n = ncol(attr())
  m = ceiling(n/3)
  paste0("There are ",n," attributes and ",m," clusters analyzed by ",input$head_or_drive,".")
})


output$factor1 <- renderUI({
  req(ATTR(),ATTR_drive())
  X = if(input$head_or_drive =="HEAD") ATTR() else ATTR_drive()
  #X = X[,order(chisq_table()[names(X)])]
  X = X[,sort(names(X))]    
  selectInput("factor_1", "Factor 1:",  choices = names(X),selected = names(X)[1])
}) 

output$factor2 <- renderUI({
  req(ATTR(),ATTR_drive())
  X = if(input$head_or_drive =="HEAD") ATTR() else ATTR_drive()
  #X = X[,order(chisq_table()[names(X)])]
  X = X[,sort(names(X))]    
  selectInput("factor_2", "Factor 2:",  choices = names(X),selected = names(X)[2])
}) 

output$factor3 <- renderUI({
  X = attr_many_levels()
  selectInput("factor_3", "Factor:",  choices = names(X),selected = names(X)[1])
}) 


output$two_way_barplot = renderPlot({
  withProgress(message = 'Making two way barchart', {
    
    req(input$factor_1,input$factor_2)
    
    X = if(input$head_or_drive =="HEAD") ATTR() else ATTR_drive()
    #        X = X[,order(chisq_table()[names(X)])]
    #        X = [,sort(names(X))]        
    y = y()      
    
    #       if(is.null(input$factor_1) & is.null(input$factor_2)){
    #         x1 = X[,1]
    #         x2 = X[,2]
    #         FAIL = y
    #         xname1 = names(X)[1]
    #         xname2 = names(X)[2]
    #       } else{
    x1 = X[,input$factor_1]
    x2 = X[,input$factor_2]
    FAIL = y
    xname1 = input$factor_1
    xname2 = input$factor_2
    #      }
    barplot_2(x1,x2,FAIL,xname1,xname2)
  })
})

output$two_way_size = renderUI({
  req(ATTR(),ATTR_drive,input$factor_1,input$factor_2)
  X = if(input$head_or_drive =="HEAD") ATTR() else ATTR_drive()
  req(input$factor_1,input$factor_2)
  WIDTH = uniqueLength(X[,input$factor_1])*uniqueLength(X[,input$factor_2])*40
  WIDTH = ifelse(WIDTH>=1000,1000,WIDTH)
  div(
    numericInput("twoway_width","2-way barplot Width",value = WIDTH,step = 10),
    numericInput("twoway_height","2-way barplot Height",value = 400,step=10)
  )
})

output$TWO_WAY_BARPLOT = renderUI({
  req(input$twoway_width,input$twoway_height)
  WIDTH = paste0(input$twoway_width,"px")
  plotOutput("two_way_barplot", width = WIDTH,height = input$twoway_height)
})

output$custom_barplot = renderPlot({
  withProgress(message = 'Generating barplot', {
    
    req(input$factor_3)
    X = attr_many_levels()
    y = y()
    
    xname = input$factor_3
    
    x = as.character(X[,xname])
    
    p = barplot_1_custom(x,
                         y,
                         xname = xname,
                         chisq_test(x,y,nrow(X)*input$chisq_tol,ignoreNA = ignoreNA()),
                         L=NULL,
                         
                         NUM_CLUST_A()
                         
    )
  })
  grid::grid.newpage()
  grid::grid.draw(p)
})

output$custom_barplot_size = renderUI({
  
  req(ATTR(),ATTR_drive(),input$factor_3)
  X = attr_many_levels()
  req(input$factor_3)
  WIDTH = uniqueLength(X[,input$factor_3])*20
  WIDTH = ifelse(WIDTH>=1220,1220,WIDTH)
  div(
    fluidRow(
      column(6,
             numericInput("bar_width","1-way barplot Width",value = WIDTH ,step = 10)
             
      ),
      column(6,
             numericInput("bar_height","1-way barplot Height",value = 400,step=10)
             
      )
    )
    
  )
})

output$CUSTOM_BARPLOT = renderUI({
  req(input$bar_height,input$bar_width)
  barheight = paste0(input$bar_height,"px")
  barwidth = paste0(input$bar_width,"px")
  
  plotOutput("custom_barplot", height = barheight, width = barwidth)
  
  
})

output$attr_many_levels_summary = renderDataTable({
  withProgress(message = 'Summarizing attributes with many levels', {
    
    X = attr_many_levels()
    y = y()
    
    n = ncol(X)
    
    validate(need(n>0,"No enough qualified long attributes"))
    
    ListFails = function(x,y){
      D = data.frame(x,y)
      ddply(D,.(x), function(df) sum(df$y=="F"))$V1
    }
    
    LIST = lapply(X,function(x) ListFails(x,y))
    
    max_length = max(sapply(LIST, length))
    
    M_1 = lapply(LIST,function(x) c(x,rep("",max_length-length(x))))
    
    
    M_2 = t(data.frame(M_1))
    colnames(M_2) = paste0(1:ncol(M_2))
    library(DT)
    colfunc <- colorRampPalette(c("white", "red"))
    colors = colfunc(100)
    
    datatable(M_2,options =
                list(
                  paging = FALSE,
                  ordering = FALSE,
                  filtering = FALSE,
                  searching =FALSE,
                  rownames = TRUE
                  #,
                  #                     autoWidth = TRUE,
                  #                     columnDefs = list(
                  #                       list(width="1px"))
                )
    )%>% 
      formatStyle(
        paste0(1:ncol(M_2)),
        color = styleInterval(seq(0,3,length.out = 99), colors)
      )
    
  })
})

observe({
  X = attr_many_levels()
  y = y()
  n = ncol(X)
#  n = min(n, 20)
  
  validate(need(n>0,"No enough qualified long attributes"))
  
  output$text_rank = renderUI({
    text_rank_output_list = lapply(1:n,function(i){
      
      name = paste("line",i,sep="")
      tags$div(class = "group-output",
               textOutput(name)
      )
    })
    do.call(tagList,text_rank_output_list)
    
  })
  
  for(j in 1:n){
    local({
      my_i = j
      
      name = paste("line",my_i,sep="")
      
      output[[name]] = renderText({
        x = as.character(X[,my_i])
        
        ListFails = function(x,y){
          D = data.frame(x,y)
          ddply(D,.(x), function(df) sum(df$y=="F"))$V1
        }
        
        QUANT = ListFails(x,y)
        LEN_1 = length(QUANT)
        LEN_2 = min(LEN_1,80)
        QUANT_2 = QUANT[1:LEN_2]
        
        c(names(X)[my_i],rep("-",25-nchar(names(X)[my_i])),QUANT_2)
      })
    })
  }
  
})