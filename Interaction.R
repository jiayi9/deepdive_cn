source("global.R",local = TRUE)

#-----------------------all interactions----------------------------------

Interaction_DF = eventReactive(list(input$interaction),{
#Interaction_DF = reactive({
    
  withProgress(message = "Searching for all Interactions!",{
    ATTR = ATTR()
    
    if( input$use_attr_list ){
      ATTR = ATTR[,toupper(names(ATTR)) %in% attr_list_inter ,drop = FALSE]      
    }
    
    validate(need(ncol(ATTR)>0,"No Attributes in the list exist."))
    PARA = para()
    Y = Y()
    M = InteractionMatrix(ATTR,PARA,Y)
    validate(need(nrow(M)>0,"All pvalues big."))
    M
  })
})




observeEvent(list(input$interaction,input$inter_log),{
#observe({
  
  
  N = 50
  top_n = ifelse(  nrow(Interaction_DF())>N, N, nrow(Interaction_DF()))
  X = Interaction_DF()[1:top_n,]
  
  
  PARA_UNIQUE = unique(X$PARA)
  n = length(PARA_UNIQUE)
  


  output$L = renderText(PARA_UNIQUE)
  

  
  
  output$boxplots_para = renderUI({
    
    boxplots_para_output_list = lapply(1:n,function(i){
      
      current_PARA = PARA_UNIQUE[i]
      temp = X[X$PARA == current_PARA, ]
      m = ceiling(nrow(temp)/3)
      HEIGHT = paste0(m*400 ,"px")
      
      name = paste("boxplot_para",i,sep="")
      text_name = paste("boxplot_para_text",i,sep="")
      tags$div(class = "group-output",
               hr(),
               h3(textOutput(text_name)),
               plotOutput(name,height = HEIGHT),
               br()  
      )
    })
    do.call(tagList,boxplots_para_output_list)
    
  })
  
  for(j in 1:n){
    local({
      my_i = j
      
      X = Interaction_DF()[1:top_n,]
      
      Y = Y()
      
      current_PARA = PARA_UNIQUE[my_i]
      
      temp = X[X$PARA == current_PARA, ]
      
      m = nrow(temp)
      
      name = paste("boxplot_para",my_i,sep="")
      
      P = lapply(1:m, function(i){
        attr_name = temp$ATTR[i]
        
        para_name = current_PARA
        
        attr = ATTR()[,   attr_name ]
        
        para = para()[, para_name]
        
        PlotLogistGroup(attr,para,Y,
                        attr_name=attr_name,
                        para_name=para_name,
                        cutoff = 0.4,hide=1,uselog = input$inter_log)        
      })
      
      
      output[[name]] = renderPlot({
        
        do.call(grid.arrange,c(P,ncol=3))
        
        
      })
      
      text_name = paste("boxplot_para_text",my_i,sep="")
      output[[text_name]] = renderText(current_PARA)
    })
  }  
})

#--------------------------custom boxplot-----------------------

output$BOX_ATTR <- renderUI({
  X = ATTR()
  selectInput("box_attr", "Attribute:",  choices = names(X),selected = names(X)[1])
}) 

output$BOX_PARA <- renderUI({
  X = para()
  selectInput("box_para", "Parametric:",  choices = names(X),selected = names(X)[1])
}) 

output$custom_boxplot = renderPlot({
  withProgress(message = 'Making custom boxplot', {
    
    req(input$box_attr,input$box_para)
    
    
#     if(is.null(input$box_attr) & is.null(input$box_para)){
#       attr = attr_ranked_p_re()[,1]
#       para = para_ranked_fit()$DATA[,1]
#       FAIL = STATUS()
#       xname1 = names(attr_ranked_p_re())[1]
#       xname2 = names(para_ranked_fit()$DATA)[1]
#     } else{
      attr = ATTR()[,input$box_attr]
      para = para()[,input$box_para]
      FAIL = Y()
      xname1 = input$box_attr
      xname2 = input$box_para
#     }
    PlotLogistGroup_jitter(attr,para,FAIL,xname1,xname2,0.4,1)
  })
})

  output$Interaction_DF = renderTable({
    X = Interaction_DF()
    R = X[,1:4]
    colnames(R) = c("Attributes","Parametrics","LR P value","ANOVA")
    R
  })

output$CUSTOM_BOXPLOT = renderUI({
  
  req(input$box_height,input$box_width)
  
  tags$div(class = "group-output",
           plotOutput("custom_boxplot",
                      height = paste0(input$box_height,"px"),
                      width = paste0(input$box_width,"px")
           )
  )
})

output$interaction_text = renderText({
  ATTR()
  para()
  NULL
})









#---------------------------- Dominant Attribute --------------------------------


output$dominant_ATTR <- renderUI({
  # can change  to ATTR() later
  X = sort(names(ATTR()))
  
  if( input$use_attr_list_1 ){
    X = X[X %in% attr_list_inter]
  }
  validate(need(length(X)>0,"No qualified Attributes"))
  
  selectInput("dominant_attr", "Dominant Attribute:",  choices = X,selected = X[1])
}) 

dominant_attr_para_fit = reactive({
  req(input$dominant_attr)
  # all qualified parametrics
  D = para()
  
  #tempary checking for new data, is there any better way of doing so? add an action box?
  flag = all(UNSELECTED_NAMES() %in%  names(ATTR()))
  validate(need(flag == 1, "New data uploaded.\nClick Process Data."))
  
  ATTR = ATTR()[,input$dominant_attr]
  withProgress(message = "Ranking parametrics by Dominant Attribute",{
    fit = data_ranked_ANOVA(X = D,ATTR)
  })
  fit
})


output$second_PARA <- renderUI({
  
  D = dominant_attr_para_fit()$DATA
    
  if(input$rank_second_para){
    R = names(D)
  } else{
    R = sort(names(D))    
  }
  
  validate(need(length(R)>0,"No qualified Parametrics"))
  
  
  selectInput("second_para", "Second Parametric:",  choices = R,selected = R[1])
})







output$dominant_attr_boxplot = renderPlot({
  withProgress(message = 'Making Attr-dominant custom boxplot', {
    
    req(input$dominant_attr,input$second_para)
    
    attr = ATTR()[,input$dominant_attr]
    para = para()[,input$second_para]
    FAIL = Y()
    xname1 = input$dominant_attr
    xname2 = input$second_para


  })
  PlotLogistGroup_jitter(attr,para,FAIL,xname1,xname2,0.4,1,uselog = input$dominant_ATTR_log)
})

output$dominant_attr_cdf = renderPlot({
  withProgress(message = 'Making Attr-dominant CDF', {
    
    req(input$dominant_attr,input$second_para)

    Group = ATTR()[,input$dominant_attr]
    y = para()[,input$second_para]
    
    xname1 = input$dominant_attr
    xname2 = input$second_para
            
    D = data.frame(Group,y)
    
    TT = paste("CDF  ",  xname2  ," by ", xname1 )
    p = ggplot(D, aes(x=y,colour = Group)) + stat_ecdf() +#+ theme(legend.position="none")
      theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
      scale_fill_discrete(name="Group")+
      ggtitle(TT)+
      theme(plot.title = element_text(size=15,color="blue"))
  })
  p

      
})



output$dominant_attr_BOXPLOT = renderUI({
  
  req(input$dominant_attr_height,input$dominant_attr_width)
  

  plotOutput("dominant_attr_boxplot",
                      height = paste0(input$dominant_attr_height,"px"),
                      width = paste0(input$dominant_attr_width,"px")
  )
           
  
})


SECOND_PARAS = reactive({
  withProgress(message = 'Ranking ANOVA', {
    
    req(dominant_attr_para_fit())
    
    fit = dominant_attr_para_fit()
    X = fit$DATA
    P = fit$P
    
    R = X[,P < 0.05]
    validate(need(ncol(R)>0,"All have no discrepancies."))
    
    })
  R
  
})

observeEvent(list(input$dominant_attr_go,input$use_attr_list_1),{
  withProgress(message = 'Doing interaction analysis for dominant Attribute', {
    
    req(input$dominant_attr)#,input$disp_missing_as_group)
    
    N = min(10,ncol(SECOND_PARAS()),na.rm = TRUE)
    
    withProgress(message = "Doing Top ANOVA boxplots for Dominant Attr!",{
      
      
      output$dominant_attr_boxplots = renderUI({
        
        dominant_attr_boxplot_list = lapply(1:N,function(i){
          
          name = paste("dominant_attr_boxplot",i,sep="")
          #text_name = paste("boxplot_ANOVA_text",i,sep="")
          tags$div(class = "group-output",
                   hr(),
                   #h3(textOutput(text_name)),
                   plotOutput(name,height = "300px"),
                   br()  
          )
        })
        do.call(tagList,dominant_attr_boxplot_list)
        
      })
      
      for(j in 1:N){
        local({
          my_i = j
          
          x = as.character(RAW()[,input$dominant_attr])
          y = SECOND_PARAS()[,my_i]
          
          
          D = data.frame(x,y)
          
          name = paste("dominant_attr_boxplot",my_i,sep="")
          
          #text_name = paste("boxplot_ANOVA_text",my_i,sep="")
          #if(input$disp_missing_as_group) x[is.na(x)] = "N/A"
          pvalue = tryCatch({
            fit = aov(y ~ x)
            summary(fit)[[1]][[1,"Pr(>F)"]]
          }, error = function(e){
            1
          })
          
          TT = paste(names(SECOND_PARAS())[my_i],"    ANOVA p-value:",round(pvalue,3),sep=" ")
          
          output[[name]] = renderPlot({
            p = ggplot(D, aes(factor(x), y))+geom_boxplot()+geom_jitter(width=WIDTH, alpha=0.5)+ theme_bw()+
              xlab(input$box_attr) + ggtitle(TT)+
              theme(plot.title = element_text(size=15,color="blue"),
                    axis.title.y=element_blank(),
                    axis.text.x = element_text(angle = 90, hjust = 0)
              )
                      if (input$dominant_ATTR_log){
                        #p = p + scale_y_continuous(trans = log_trans())
                        p = p + scale_y_log10()
                      }
            p
          })
        })
      }
    })
    
    
    withProgress(message = "Doing Top CDFs!",{
      
      
      output$dominant_attr_CDF = renderUI({
        
        dominant_attr_CDF_output_list = lapply(1:N,function(i){
          
          name = paste("dominant_attr_CDF",i,sep="")
          #text_name = paste("boxplot_ANOVA_text",i,sep="")
          tags$div(class = "group-output",
                   hr(),
                   #h3(textOutput(text_name)),
                   plotOutput(name,height = "300px"),
                   br()  
          )
        })
        do.call(tagList,dominant_attr_CDF_output_list)
        
      })
      
      for(j in 1:N){
        local({
          my_i = j
          
          Group = as.character(RAW()[,input$dominant_attr])
          #if(input$disp_missing_as_group) Group[is.na(Group)] = "N/A"
          y = SECOND_PARAS()[,my_i]
          
          D = data.frame(Group,y)
          
          name = paste("dominant_attr_CDF",my_i,sep="")
          
          TT = paste("CDF  ",  names(SECOND_PARAS())[my_i]  ," by ",input$dominant_attr )
          
          #text_name = paste("boxplot_ANOVA_text",my_i,sep="")
          
          output[[name]] = renderPlot({
            ggplot(D, aes(x=y,colour = Group)) + stat_ecdf() +#+ theme(legend.position="none")
              theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
              scale_fill_discrete(name="Group")+
              ggtitle(TT)+
              theme(plot.title = element_text(size=15,color="blue"))
            
          })
        })
      }
    })
  })
})



#---------------------------- Dominant Parametric --------------------------------


output$dominant_PARA <- renderUI({
  
  validate(need(ncol(para())>0,"No parametrics"))
  
  X = sort(names(para()))
  selectInput("dominant_para", "Dominant Parametric:",  choices = X,selected = X[1])
}) 

dominant_para_attr_fit = reactive({
  req(input$dominant_para)
  # all qualified parametrics
  
  D = ATTR()
  
  if( input$use_attr_list_2 ){
    D = D[,names(D) %in% attr_list_inter]
  }
  
  validate(need(ncol(D)>0,"No qualified attributes."))
  
  #tempary checking for new data, is there any better way of doing so? add an action box?
  flag = input$dominant_para %in% names(para())
  validate(need(flag == 1, "New data uploaded.\nClick Process Data."))
  
  
  PARA = para()[,input$dominant_para]
  withProgress(message = "Ranking parametrics by Dominant Parametric",{
    fit = data_ranked_ANOVA_2(X = D,PARA)
  })
  fit
})


output$second_ATTR <- renderUI({
  
  D = dominant_para_attr_fit()$DATA
  
  if(input$rank_second_attr){
    R = names(D)
  } else{
    R = sort(names(D))    
  }
  

  
  selectInput("second_attr", "Second Attribute:",  choices = R,selected = R[1])
}) 







output$dominant_para_boxplot = renderPlot({
  withProgress(message = 'Making Para-dominant custom boxplot', {
    
    req(input$dominant_para,input$second_attr)
    
    attr = ATTR()[,input$second_attr]
    para = para()[,input$dominant_para]
    FAIL = Y()
    xname1 = input$second_attr
    xname2 = input$dominant_para
    
    
  })
  PlotLogistGroup_jitter(attr,para,FAIL,xname1,xname2,0.4,1,uselog = input$dominant_PARA_log)
})

output$dominant_para_cdf = renderPlot({
  withProgress(message = 'Making PARA-dominant CDF', {
    
    req(input$dominant_para,input$second_attr)
    
    Group = ATTR()[,input$second_attr]
    y = para()[,input$dominant_para]
    
    xname1 = input$second_attr
    xname2 = input$dominant_para
    
    D = data.frame(Group,y)
    
    TT = paste("CDF  ",  xname2  ," by ", xname1 )
    p = 
      ggplot(D, aes(x=y,colour = Group)) + stat_ecdf() +#+ theme(legend.position="none")
      theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
      scale_fill_discrete(name="Group")+
      ggtitle(TT)+
      theme(plot.title = element_text(size=15,color="blue"))
    
  })
  
  p
})

output$dominant_para_BOXPLOT = renderUI({
  
  req(input$dominant_para_height,input$dominant_para_width)
  
  
  plotOutput("dominant_para_boxplot",
             height = paste0(input$dominant_para_height,"px"),
             width = paste0(input$dominant_para_width,"px")
  )
  
  
})


SECOND_ATTRS = reactive({
  withProgress(message = 'Ranking ANOVA', {
    
    req(dominant_para_attr_fit())
    
    fit = dominant_para_attr_fit()
    X = fit$DATA
    P = fit$P
    
    R = X[,P < 0.05]
    validate(need(ncol(R)>0,"All have no discrepancies."))
    
  })
  R
  
})

observeEvent(list(input$dominant_para_go,input$use_attr_list_2),{
  withProgress(message = 'Doing interaction analysis for the dominant Parametric', {
    
    req(input$dominant_para)#,input$disp_missing_as_group)
    
    N = min(10,ncol(SECOND_ATTRS()),na.rm = TRUE)
    
    withProgress(message = "Doing Top ANOVA boxplots for the Dominant Parametric!",{
      
      
      output$dominant_para_boxplots = renderUI({
        
        dominant_para_boxplot_list = lapply(1:N,function(i){
          
          name = paste("dominant_para_boxplot",i,sep="")
          #text_name = paste("boxplot_ANOVA_text",i,sep="")
          tags$div(class = "group-output",
                   hr(),
                   #h3(textOutput(text_name)),
                   plotOutput(name,height = "300px"),
                   br()  
          )
        })
        do.call(tagList,dominant_para_boxplot_list)
        
      })
      
      for(j in 1:N){
        local({
          my_i = j
          
#           x = as.character(RAW()[,input$dominant_attr])
#           y = SECOND_PARAS()[,my_i]

          x = SECOND_ATTRS()[,my_i]
          y = RAW()[,input$dominant_para]
          
          
          D = data.frame(x,y)
          
          name = paste("dominant_para_boxplot",my_i,sep="")
          
          #text_name = paste("boxplot_ANOVA_text",my_i,sep="")
          #if(input$disp_missing_as_group) x[is.na(x)] = "N/A"
          pvalue = tryCatch({
            fit = aov(y ~ x)
            summary(fit)[[1]][[1,"Pr(>F)"]]
          }, error = function(e){
            1
          })
          
          TT = paste(names(SECOND_ATTRS())[my_i],"    ANOVA p-value:",round(pvalue,3),sep=" ")
          
          output[[name]] = renderPlot({
            p = ggplot(D, aes(factor(x), y))+geom_boxplot()+geom_jitter(width=WIDTH, alpha=0.5)+ theme_bw()+
              xlab(input$box_attr) + ggtitle(TT)+
              theme(plot.title = element_text(size=15,color="blue"),
                    axis.title.y=element_blank(),
                    axis.text.x = element_text(angle = 90, hjust = 0)
              )
                      if (input$dominant_PARA_log){
                        #p = p + scale_y_continuous(trans = log_trans())
                        p = p + scale_y_log10()
                      }
            p
          })
        })
      }
    })
    
    
    withProgress(message = "Doing Top CDFs!",{
      
      
      output$dominant_para_CDF = renderUI({
        
        dominant_para_CDF_output_list = lapply(1:N,function(i){
          
          name = paste("dominant_para_CDF",i,sep="")
          #text_name = paste("boxplot_ANOVA_text",i,sep="")
          tags$div(class = "group-output",
                   hr(),
                   #h3(textOutput(text_name)),
                   plotOutput(name,height = "300px"),
                   br()  
          )
        })
        do.call(tagList,dominant_para_CDF_output_list)
        
      })
      
      for(j in 1:N){
        local({
          my_i = j
          
#           Group = as.character(RAW()[,input$dominant_attr])
#           #if(input$disp_missing_as_group) Group[is.na(Group)] = "N/A"
#           y = SECOND_PARAS()[,my_i]
          
          Group = SECOND_ATTRS()[,my_i]
          y = RAW()[,input$dominant_para]
          
          D = data.frame(Group,y)
          
          name = paste("dominant_para_CDF",my_i,sep="")
          
          TT = paste("CDF  ",  names(SECOND_ATTRS())[my_i]  ," by ",input$dominant_para )
          
          #text_name = paste("boxplot_ANOVA_text",my_i,sep="")
          
          output[[name]] = renderPlot({
            ggplot(D, aes(x=y,colour = Group)) + stat_ecdf() +#+ theme(legend.position="none")
              theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
              scale_fill_discrete(name="Group")+
              ggtitle(TT)+
              theme(plot.title = element_text(size=15,color="blue"))
            
          })
        })
      }
    })
  })
})