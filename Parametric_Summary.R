#PlugIn:
#input$flush input$go
#para_ranked_top()
#NUM_CLUST_P()



# scatterData = eventReactive(list(input$flush,input$go), {
#   
#   para_ranked_top()[,sample(1:18)]
# },ignoreNULL=TRUE)

MAX_P_PARA = reactive({
input$max_p_para
})

scatterData = eventReactive(list(input$analyze),{
  req(para())
  pvalues = para_p_table()
  R = para()[,pvalues< MAX_P_PARA()]
  validate(need(ncol(R)>0,"No parametrics have P value < specified value (0.05 by default)"))
  R
})


output$scatterData_log = renderPrint({
  print_summary(scatterData())
})


para_clust_fit = reactive({
  req(scatterData())
  X = scatterData()
  validate(need(ncol(X)>2,"Need at least 2 parametrics to do clustering.\nTry increasing clustering Sensitivity value in the advanced settings."))
  fit = ClustOfVar::hclustvar(X.quanti =  X)
  fit
})


scatter_color = reactive({
  req(para_clust_fit())
  fit = para_clust_fit()
  temp = cutree(fit,h = PARA_CLUST_H())
  data.frame(NAME = names(temp),  group = as.vector(temp))
})

output$scatter_color_log = renderPrint({
  scatter_color()
})

#observeEvent(list(input$flush,input$go),{
observeEvent(list(input$analyze,input$flush),{
  req(scatterData(),Y(),scatter_color())
  withProgress(message = 'Drawing Summary parametrics', {
  D = scatterData()
  D = D[,sample(1:ncol(D))]
  FAIL = Y()
  highlight =   (FAIL == "F")
  
  color = scatter_color()
  
  N = length(unique(color$group))
  
  n_pair = floor(ncol(D)/2)
  
  L = list()
  
  for ( i in 1:n_pair){
    x = D[,2*i-1]
    y = D[,2*i]
    namex = names(D)[2*i-1]
    namey = names(D)[2*i]
    temp = data.frame(x,y)
    temp2 = Random_Sample_prop(temp,0.5)
    temp3 = temp[highlight,]
    
    
    
    xcol = ggplotColours(N)[  color[color[,1]==namex,2]      ]
    ycol = ggplotColours(N)[  color[color[,1]==namey,2]      ]
    
    p = ggplot(data=temp2,aes(x=x,y=y)) +
      geom_point()+
      xlab(namex)+ ylab(namey)+
      geom_point(data=temp3,aes(x=x,y=y),color="red",size=4)+
      theme(legend.position="none")+
      theme(axis.title.x=element_text(colour=xcol))+
      theme(axis.title.y=element_text(colour=ycol))
    
    #assign(paste("p",i,sep=""),p)
    L = c(L,list(p))
    
  }
#   output$scatter1 =renderPlot({
#     grid::grid.newpage()
#     g=cbind(ggplotGrob(p1),ggplotGrob(p2),ggplotGrob(p3),size="first")
#     grid::grid.draw(g)
#     
#     })
#   
#   output$scatter2 =renderPlot({
#     grid::grid.newpage()
#     g=cbind(ggplotGrob(p4),ggplotGrob(p5),ggplotGrob(p6),size="first")
#     grid::grid.draw(g)
#   })
#   
#   output$scatter3 =renderPlot({
#     grid::grid.newpage()
#     g=cbind(ggplotGrob(p7),ggplotGrob(p8),ggplotGrob(p9),size="first")
#     grid::grid.draw(g)
#     
#   })
    output$para_tops = renderPlot({
      withProgress(message = 'Making scatterplots', {
        
        do.call(grid.arrange,c(L,ncol=3))
      })
    })

  })
  
})




output$para_clust_chart = renderPlot({
  req(para_clust_fit())
  withProgress(message = 'Clustering parametrics', {
    fit = para_clust_fit()

    
    df2<-data.frame(cluster=cutree(fit,h=PARA_CLUST_H()),
                    states=factor(fit$labels,levels=fit$labels[fit$order]))
    df3<-ddply(df2,.(cluster),summarise,pos=mean(as.numeric(states)))
    p1 = ggdendrogram(as.dendrogram(fit), rotate=TRUE)+
      scale_x_continuous(expand = c(0, 0.5), labels = levels(df2$states), breaks = 1:length(df2$states)) +
      scale_y_continuous(expand = c(0.02, 0)) 
    p2 = ggplot(df2,aes(states,y=1,fill=factor(cluster)))+geom_tile()+
      scale_y_continuous(expand=c(0,0))+
      scale_x_discrete(expand = c(0, 0)) +
      theme(axis.title=element_blank(),
            axis.ticks=element_blank(),
            axis.text.x=element_blank(),
            legend.position="none")+coord_flip()+
      geom_text(data=df3,aes(x=pos,label=cluster))
    gp1<-ggplotGrob(p1)
    gp2<-ggplotGrob(p2)  
    maxHeight = grid::unit.pmax(gp1$heights[2:5], gp2$heights[2:5])
    gp1$heights[2:5] <- as.list(maxHeight)
    gp2$heights[2:5] <- as.list(maxHeight)
    grid.arrange(gp2, gp1, ncol=2,widths=c(1/6,5/6))
  })
})

output$para_text = renderText({
 # para_ranked_fit()
  NULL
})

output$PARA_CLUST_CHART = renderUI({
  HEIGHT = paste0(ncol(scatterData())*25,"px")
  plotOutput("para_clust_chart",height = HEIGHT)  
})


output$PARA_TOPS = renderUI({
  n = ncol(scatterData()) -1
  validate(need(n>0,"No significant parametrics"))
  Nrow = ceiling(n/6)
  plotOutput("para_tops",height = paste0(350*Nrow,"px"))
})

output$para = renderUI({
  tags$div(class = "group-output",
           actionButton("flush","FLUSH ORDER", icon = icon('fa fa-refresh')),
           textOutput("para_text"),
           uiOutput("PARA_TOPS"),
           hr(),
           uiOutput("PARA_CLUST_CHART")
  )
})


