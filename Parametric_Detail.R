#para_ranked_fit()$DATA

#STATUS()
output$ranked_para_names = renderDataTable({
  colfunc <- colorRampPalette(c("red", "white"))
  colors = colfunc(100)
  X = data.frame(P_value = para_p_table())
  datatable(X,options =
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
      paste0("P_value"),
      color = styleInterval(seq(0,1,length.out = 99), colors)
    )
  
})


output$ranked_para_names_table = renderTable({

  X = data.frame(Names =names(para_p_table()),P_value = para_p_table())
  rownames(X) = 1:nrow(X)
  X
  
})


output$para_select <- renderUI({
  req(para())
  X = para()
  selectInput("uni_para", "Parameter:",  choices = names(X),selected = names(X)[1])
}) 

output$para_scatter_1 <- renderUI({
  req(para())
  X = para()
  selectInput("para_scatter_1", "X parameter:",  choices = names(X),selected = names(X)[1])
}) 

output$para_scatter_2 <- renderUI({
  req(para())
  X = para()
  selectInput("para_scatter_2", "Y parameter:",  choices = names(X),selected = names(X)[2])
}) 

#single chart
# output$single_para = renderPlot({
#   withProgress(message = 'Making boxplot', {
#     FAIL = STATUS()
#     WEEK = upload()$CEE_TEST_WORK_WEEK
#     highlight =   (FAIL == "F")
#     
#     if(is.null(input$uni_para)) {
#       temp = data.frame(WEEK=WEEK,y=para_ranked_fit()$DATA[,1])
#     } else {
#       temp = data.frame(WEEK=WEEK,y=para_ranked_fit()$DATA[,input$uni_para] )
#     }
#     
#     temp2 = Random_Sample_prop(  temp, 1)
#     temp3 = subset(temp,highlight)
#     
#     p = ggplot(data=temp2,aes(WEEK,y))+
#       geom_boxplot()+
#       geom_jitter()+
#       xlab("Fiscal Week")+
#       ylab("Value")+
#       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#       geom_point(
#         data = temp3,aes(WEEK,y),color="red",size=4
#       ) 
#     p
#   })
# })

output$scatterplot = renderPlot({
  req(input$para_scatter_1,input$para_scatter_2,Y(),para())
  withProgress(message = 'Making scatterplot', {
    FAIL = Y()
    highlight =   (FAIL == "F")
    
    X = para()
    
    if(is.null(input$para_scatter_1) & is.null(input$para_scatter_2)){
      temp = data.frame(x = X[,1], y = X[,2])
      namex = names(X)[1]
      namey = names(X)[2]
    } else {
      temp = data.frame(x = X[,input$para_scatter_1], y = X[,input$para_scatter_2])
      namex = input$para_scatter_1
      namey = input$para_scatter_2
    }
    
    temp2 = Random_Sample_prop(  temp, 1)
    temp3 = subset(temp,highlight)
          
    ggplot(data=temp2,aes(x=x,y=y)) +
      geom_point(alpha=0.4)+
#       scale_x_continuous(limit=c(min(x), max(x)), 
#                          breaks=round(fivenum(x),1))+
#       scale_y_continuous(limit=c(min(y), max(y)),
#                          breaks=round(fivenum(y),1)) +    
      
      geom_rug(size=0.05) +   
      #geom_smooth(method=lm) +
      stat_ellipse(type = "norm",linetype = 2)+
      xlab(namex)+ ylab(namey)+
      theme_bw()+
      geom_point(data=temp3,aes(x=x,y=y),color="red",size=3)+
  geom_rug(data=temp3,size=0.1,color="red") +   
      theme(legend.position="none")

#      +
#      theme_set(theme_minimal(base_size = 18))
  })
})

output$parametrics = renderUI({
  div(

    fluidRow(
      column(3,
             uiOutput("para_scatter_1"),
             uiOutput("para_scatter_2")
      ),
      column(9,
             plotOutput("scatterplot",height="500px",click = "plot1_click", brush = brushOpts(id = "plot1_brush")),
             h4("Points near click"),
             verbatimTextOutput("click_info"),
             h4("Selected points"),
             verbatimTextOutput("brush_info")
             
#             plotOutput("scatterplot",height="500px")
      )
    )
#    hr(),
#     fluidRow(
#       column(3,uiOutput("para_select")),
#       column(9,plotOutput("single_para"))
#     )
  )
})


output$click_info <- renderPrint({
  req(input$para_scatter_1,input$para_scatter_2,Y(),para())
  
  FAIL = Y()
  highlight =   (FAIL == "F")
  
  X = para()
  
  if(is.null(input$para_scatter_1) & is.null(input$para_scatter_2)){
    temp = data.frame(x = X[,1], y = X[,2])
    namex = names(X)[1]
    namey = names(X)[2]
  } else {
    temp = data.frame(x = X[,input$para_scatter_1], y = X[,input$para_scatter_2])
    namex = input$para_scatter_1
    namey = input$para_scatter_2
  }
  #temp = data.frame(temp,DSN())
  #names(temp) = c(namex,namey,"Drive Serial Num")
  
  X = nearPoints(temp, input$plot1_click, addDist = TRUE)
  
  X = data.frame(DSN()[X$x],X[,1:2])
  names(X) = c("Drive_Serial_Num",namex,namey)
  X
})

output$brush_info <- renderPrint({
  req(input$para_scatter_1,input$para_scatter_2,Y(),para())
  
  FAIL = Y()
  highlight =   (FAIL == "F")
  
  X = para()
  
  if(is.null(input$para_scatter_1) & is.null(input$para_scatter_2)){
    temp = data.frame(x = X[,1], y = X[,2])
    namex = names(X)[1]
    namey = names(X)[2]
  } else {
    temp = data.frame(x = X[,input$para_scatter_1], y = X[,input$para_scatter_2])
    namex = input$para_scatter_1
    namey = input$para_scatter_2
  }

  X = brushedPoints(temp, input$plot1_brush)
  X = data.frame(DSN()[X$x],X[,1:2])
  names(X) = c("Drive_Serial_Num",namex,namey)
  X
})

# output$fails_table = renderTable({
#   withProgress(message = 'Making failure table', {
#     FAIL = FAIL()
#     highlight =   (FAIL == "F")
#     
#     if(is.null(input$para_scatter_1) & is.null(input$para_scatter_2)){
#       temp = data.frame(x = para_ranked_fit()$DATA[,1], y = para_ranked_fit()$DATA[,2])
#       namex = names(para_ranked_fit()$DATA)[1]
#       namey = names(para_ranked_fit()$DATA)[2]
#     } else {
#       temp = data.frame(x = para_ranked_fit()$DATA[,input$para_scatter_1], y = para_ranked_fit()$DATA[,input$para_scatter_2])
#       namex = input$para_scatter_1
#       namey = input$para_scatter_2
#     }
#     names(temp)=c(namex,namey)
#     
#     R = subset(temp,highlight)
#     SN = subset(ID()[,c(1,3)],highlight)
#     
#     min1 = min(temp[,1],na.rm = TRUE)
#     min2 = min(temp[,2],na.rm = TRUE)
#     max1 = max(temp[,1],na.rm = TRUE)
#     max2 = max(temp[,2],na.rm = TRUE)
#     
#     v1 = sapply(R[,1], function(i) sum(i>=temp[,1],na.rm=TRUE)/nrow(temp))
#     v2 = sapply(R[,2], function(i) sum(i>=temp[,2],na.rm=TRUE)/nrow(temp))
#     
#     
#     R2 = data.frame(SN,round(v1,2),round(v2,2))
#     names(R2)=c("DRIVE_SN","HEAD_SN",namex,namey)
#     R2 = R2[order(R2[,3]),]
#     R2
#   })
# },include.rownames=FALSE)
