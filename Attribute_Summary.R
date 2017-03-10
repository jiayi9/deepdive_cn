
    
    Ncol = ncol(D)
    Rows = floor(Ncol/5)
    
    #-
    output$hist_1 = renderUI({
      if (Rows>0) {
        barplot_output_list = lapply(1:Rows,function(i){
          
          name = paste("row",i,sep="")
          tags$div(class = "group-output",
                   plotOutput(name,height=paste0(input$onewaybarheight,"px")),
                   br()  
          )
        })
        do.call(tagList,barplot_output_list)
      } else{
        NULL
      }
    })
    #-
    
    #-
    if(Rows>0){
      for(j in 1:Rows){
        local({
          my_i = j
          
          name = paste("row",my_i,sep="")
          
          output[[name]] = renderPlot({
            
            #D
            #y
            y = y()
            #labelx
            N = length(unique(labelx$group))
            for(i in 1:5){
              index=my_i*5-5+i
              assign(paste("p",i,sep=""),
                     barplot_1(D[,index],
                               y,
                               names(D)[index],
                               chisq_test(D[,index],y,nrow(D)*input$chisq_tol,ignoreNA = ignoreNA()),
                               labelx,
                               N,
                               #NUM_CLUST_A(),
                               showbarcolor = input$showbarcolor
                     )#,envir = globalenv() 
              )
            }
            g = cbind(p1,p2,p3,p4,p5, size="first")
            g$heights = grid::unit.pmax(p1$heights, p2$heights,p3$heights,p4$heights,p5$heights)
            grid::grid.newpage()
            grid::grid.draw(g)
            
          })
        })
      }
    }
    #-
    
    Nleft = Ncol%%5
    eachRow = 5
    N = NUM_CLUST_A()
    STATUS = y()
    
    if(Nleft>0){
      pvalues=c()
      pvalues[(Rows*eachRow+1):(Rows*eachRow+Nleft)] = sapply(1:Nleft, function(i){chisq_test(D[,Rows*eachRow+i],STATUS,nrow(D)*input$chisq_tol,ignoreNA = ignoreNA())})
    }
    p0=ggplot(mtcars, aes(x = wt, y = mpg)) + geom_blank()+theme(
      panel.background = element_rect(fill = NA), 
      panel.grid = element_blank(),
      axis.title=element_blank(),
      axis.ticks=element_blank(),
      axis.text=element_blank()
    )
    p0 = ggplotGrob(p0)
    
    if(Nleft==1){
      pp1 = barplot_1( D[,Rows*eachRow+1],
                       STATUS,
                       names(D)[Rows*eachRow+1],
                       pvalues[Rows*eachRow+1],
                       labelx,
                       N,
                       showbarcolor = input$showbarcolor)
      gx = cbind(pp1, p0,p0,p0,p0, size="first")
      gx$heights = grid::unit.pmax(pp1$heights)
      
    }
    if(Nleft==2){
      pp1 = barplot_1( D[,Rows*eachRow+1],
                       STATUS,
                       names(D)[Rows*eachRow+1],
                       pvalues[Rows*eachRow+1],
                       labelx,
                       N,
                       showbarcolor = input$showbarcolor)
      
      pp2 = barplot_1( D[,Rows*eachRow+2],
                       STATUS,
                       names(D)[Rows*eachRow+2],
                       pvalues[Rows*eachRow+2],
                       labelx,
                       N,
                       showbarcolor = input$showbarcolor)
      
      gx = cbind(pp1, pp2,p0,p0,p0, size="first")
      gx$heights = grid::unit.pmax(pp1$heights,pp2$heights)
      
    }
    if(Nleft==3){
      pp1 = barplot_1( D[,Rows*eachRow+1],
                       STATUS,
                       names(D)[Rows*eachRow+1],
                       pvalues[Rows*eachRow+1],
                       labelx,
                       N,
                       showbarcolor = input$showbarcolor)
      
      pp2 = barplot_1( D[,Rows*eachRow+2],
                       STATUS,
                       names(D)[Rows*eachRow+2],
                       pvalues[Rows*eachRow+2],
                       labelx,
                       N,
                       showbarcolor = input$showbarcolor)
      
      pp3 = barplot_1( D[,Rows*eachRow+3],
                       STATUS,
                       names(D)[Rows*eachRow+3],
                       pvalues[Rows*eachRow+3],
                       labelx,
                       N,
                       showbarcolor = input$showbarcolor)  
      gx = cbind(pp1, pp2,pp3,p0,p0, size="first")
      gx$heights = grid::unit.pmax(pp1$heights,pp2$heights,pp3$heights)
      
    }
    if(Nleft==4){
      pp1 = barplot_1( D[,Rows*eachRow+1],
                       STATUS,
                       names(D)[Rows*eachRow+1],
                       pvalues[Rows*eachRow+1],
                       labelx,
                       N,
                       showbarcolor = input$showbarcolor)
      
      pp2 = barplot_1( D[,Rows*eachRow+2],
                       STATUS,
                       names(D)[Rows*eachRow+2],
                       pvalues[Rows*eachRow+2],
                       labelx,
                       N,
                       showbarcolor = input$showbarcolor)
      
      pp3 = barplot_1( D[,Rows*eachRow+3],
                       STATUS,
                       names(D)[Rows*eachRow+3],
                       pvalues[Rows*eachRow+3],
                       labelx,
                       N,
                       showbarcolor = input$showbarcolor)  
      pp4 = barplot_1( D[,Rows*eachRow+4],
                       STATUS,
                       names(D)[Rows*eachRow+4],
                       pvalues[Rows*eachRow+4],
                       labelx,
                       N,
                       showbarcolor = input$showbarcolor)  
      gx = cbind(pp1, pp2,pp3,pp4,p0, size="first")
      gx$heights = grid::unit.pmax(pp1$heights,pp2$heights,pp3$heights,pp4$height)
      
    }
    output$hist_2 = renderPlot({
      grid::grid.newpage()
      if(Nleft ==0 ){NULL}else(grid::grid.draw(gx))
    })

    if(Nleft == 0){
      output$hist_2_x = renderUI(
        
        plotOutput("hist_2",height="0px")
        
      )
    } else {
      output$hist_2_x = renderUI(
        
        plotOutput("hist_2",height=paste0(input$onewaybarheight,"px"))
        
      )
    }