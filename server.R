InsPack <- function(pack) { 
  if (!pack %in% rownames(installed.packages())) { 
    print(paste("installing",pack)) 
    install.packages(pack) 
  } else {
    print(paste(pack," already installed")) 
  }
}
InsPack("rpivotTable")
InsPack("ggplot2")
InsPack("gtable")
InsPack("ClustOfVar")
InsPack("ggdendro")
InsPack("gridExtra")
InsPack("scales")
InsPack("ggvis")
InsPack("grid")
InsPack("DT")
InsPack("data.table")
InsPack("dplyr")
InsPack("plyr")
InsPack("zoo")
InsPack("rmarkdown")
InsPack("knitr")


library(rpivotTable)
library(ggplot2)
library(gtable)
library(ClustOfVar)
library(ggdendro)
library(gridExtra)
library(scales)
library(plyr)
library(ggvis)
library(grid)
library(DT)
shinyOptions(progress.style="old")

options(shiny.maxRequestSize=100*1024^2)
options(shiny.usecairo=TRUE)
options(warn=-1)
#options(scipen=99)
#options(encoding = "UTF-8")  

  Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')


server <- function(input, session,output) {
  upload = reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    R = read.csv(inFile$datapath, header=TRUE, sep=",",stringsAsFactors=FALSE)
    R
  })
  
  output$info = renderText({
    req(upload())
    LEVELS = getLevels(upload())
    n = sum(LEVELS==1)
    
    m = sum(sapply(upload(),function(x) all(is.na(x))))
    
    paste("该文件包含",
          nrow(upload()),
          "行与",
          ncol(upload()),
          "列数据(其中",
          n,
          "列的值全部相同, ",
          m,
          "a列的值全为空)."
          
    )
    NULL
  })
  
  output$upload_log = renderPrint({
    req(upload())
    print_summary(upload())    
  })
  
  output$EXCLUDE = renderUI({
    req(upload())
    X = upload()
    selectInput("exclude","忽略以下列数据",choices = c("",sort(names(X))), multiple = TRUE ,selected ="")
  })
  
  
  output$TARGET_UI = renderUI({
    req(upload())
    
    NAMES = setdiff(names(upload()),input$exclude)
    
    default_target = 
      if("STATUS" %in% NAMES) {"STATUS"} else if
    ("EFA_RESULT" %in% NAMES) {"EFA_RESULT"} else if
    ("EFA" %in% NAMES) {"EFA_RESULT"} else
    {NAMES[1]}
    
    selectInput("target","目标列",choices = sort(NAMES),selected = default_target)
    
  })
  
  output$TARGET_UI_log = renderPrint({
    input$target
  })
  
  TARGET = reactive({
    req(upload())
    req(input$target)
    upload()[,input$target]
    
  })
  
  output$TARGET_log = renderPrint({
    req(TARGET())
    #try({table(TARGET())},silent = TRUE)
    table(TARGET())
  })
  
  
  output$TARGET_DEFINE_UI = renderUI({
    
    req(input$target)
    
    
    NAMES = unique(TARGET())
    default =  character()
    
    if("F" %in% NAMES ) {default = c(default,"F")}
    if("Fail" %in% NAMES ) {default = c(default,"Fail")}
    if("FAIL" %in% NAMES ) {default = c(default,"FAIL")}
    if("Failure" %in% NAMES ) {default = c(default,"Failure")}
    if("FAILURE" %in% NAMES ) {default = c(default,"FAILURE")}
    if(input$define_non_empty_as_fail){
      default =  setdiff(NAMES,c("",NA))
    }
    selectInput("target_define","定义Failures",choices = unique(TARGET()),multiple = TRUE,selected = default)
    
  })
  
  output$define_non_empty_as_fail_log = renderPrint({
    input$define_non_empty_as_fail
  })
  
  output$AGG_UI = renderUI({
    req(input$target)
    NAMES = setdiff(names(upload()),input$exclude)
    
    default =  
      if("DRIVE_SERIAL_NUM" %in% NAMES ) {"DRIVE_SERIAL_NUM"} else if
    ("Drive_Serial_Num" %in% NAMES ) {"Drive_Serial_Num"} else if
    ("drive_serial_num" %in% NAMES ) {"drive_serial_num"} else if
    
    ("DRIVE.SERIAL.NUM" %in% NAMES ) {"DRIVE.SERIAL.NUM"} else if
    ("Drive.Serial.Num" %in% NAMES ) {"Drive.Serial.Num"} else if
    ("drive.serial.num" %in% NAMES ) {"drive.serial.num"} else if
    
    ("SERIAL_NUM" %in% NAMES ) {"SERIAL_NUM"} else if
    ("serial_num" %in% NAMES ) {"serial_num"} else if
    ("SN" %in% NAMES ) {"SN"}
    
    if(!input$no_agg){
      selectInput("agg","定义聚集列",choices = NAMES,selected = default)      
    }
    
    
    
  })
  
  output$agg_log = renderPrint({input$agg})
  
  output$HEAD_OR_DRIVE = renderUI({
    if(!input$no_agg){
      selectInput("head_or_drive","分析级别",choices = c("DRIVE","HEAD"),selected = "DRIVE")
    } else {
      selectInput("head_or_drive","分析级别",choices = c("DRIVE","HEAD"),selected = "HEAD")
      
    }
    
  })
  
  output$head_or_drive_log = renderPrint({input$head_or_drive})
  source("FUNCTIONS.R",local = TRUE)
  source("www/chooser.R", local = TRUE)
  
 # source("control.R", local = TRUE)#,encoding = "utf-8")
  source("values.R",local = TRUE)
  
  
  
  
  
  #--------------------Data Manipulation--------------
  
  
  RAW = reactive({
    #validate(need(!is.null(upload()),"No data"))
    req(upload(),input$target,input$agg)
    R = upload()
    NAMES = c(input$target,input$agg,input$exclude)
    R[,!(names(R) %in% NAMES) ]
  })
  
  output$RAW_log = renderPrint({
    req(RAW())
    print_summary(RAW())
    
  })
  
  MAX_N = 30
  
  ATTR = reactive({    
    R = getATTR(RAW(),MAX_N)
    R = exclude_double(R)
    R
  })
  
  output$ATTR_log = renderPrint({
    print_summary(ATTR())
  })
  
  Y = reactive({
    req(upload(),input$target)
    TARGET = upload()[,input$target]
    R = ifelse(TARGET %in% input$target_define,"F","P")
    R

  })
  
  output$Y_log = renderPrint({
    table(Y())
  })
  
  DSN = reactive({
    req(upload(),input$agg)
      
    upload()[,input$agg]
  })
  
  output$DSN_log = renderPrint({
    table(DSN())[1:10]
  })
  
  temp_aggregate = reactive({
    withProgress(message = 'Aggregating Attributes...', {
      #if(!is.null(upload())&!is.null(input$agg)&!is.null(input$target)&!is.null(input$target_define)){
      fit = aggregate_data(group = DSN(),flag = Y(),X = ATTR() )
      fit
    })
  })
  
  #aggregate_data = function(group,flag,X)    R$DATA R$group R$flag
  
  ATTR_drive = reactive({
    temp_aggregate()$DATA
  })
  
  output$ATTR_drive_log = renderPrint({
    print_summary(ATTR_drive())
  })
  
  output$ATTR_drive_table = renderTable({
    head(ATTR_drive())
  })
  
  DSN_drive = reactive({
    temp_aggregate()$group
  })
  
  output$DSN_drive_log = renderPrint({
    nrow(DSN_drive())
  })
  
  Y_drive = reactive({
    temp_aggregate()$flag
  })
  
  output$Y_drive_log = renderPrint({
    table(Y_drive())
  })
  
  
  #-------------------chooser------------------------
  
  chisq_table_head = eventReactive(input$update,{
    withProgress(message = 'Generating Chisq Table - Head', {
      
    X = ATTR()
    Y = Y()
    tol = input$chisq_tol
    sort(sapply(X, function(x) chisq_test(x,Y, nrow(X)*tol,ignoreNA = ignoreNA())))
    })
  })
  
  
  output$chisq_table_head_log = renderPrint({
    START = proc.time()
    chisq_table_head()
    #print(proc.time()-START)
    
  })
  
  chisq_table_drive = eventReactive(input$update,{
    withProgress(message = 'Generating Chisq Table - Drive', {
      
    X = ATTR_drive()
    Y = Y_drive()

    tol = input$chisq_tol
    sort(sapply(X, function(x) chisq_test(x,Y, nrow(X)*tol,ignoreNA = ignoreNA())))
    })
  })
  
  
  output$chisq_table_drive_log = renderPrint({
    #START = proc.time()
    
    chisq_table_drive()
    #print(proc.time()-START)
    
  })
  
  
  
  chisq_table = reactive({
    req(chisq_table_drive(),chisq_table_head())
    if(input$head_or_drive =="DRIVE"){
      chisq_table_drive()
    } else {
      chisq_table_head()
    }
  })
  
  output$chisq_table_log = renderPrint({
    c(HEAD = identical(chisq_table(),chisq_table_head()),Drive = identical(chisq_table(),chisq_table_drive()))
  })
  
  
  
  
  

  
  attr_names = eventReactive(input$update,{
    withProgress(message = 'Choosing qualified attributes', {
      
    req(ATTR_drive())
    if(input$head_or_drive == "DRIVE"){
      X = ATTR_drive()
      Y = Y_drive()

      pvalue_table = chisq_table_drive()
    } else if(input$head_or_drive == "HEAD"){
      X = ATTR()
      Y = Y()
      pvalue_table = chisq_table_head()
    } else {
      stop("logic error!")
    }
    tol = input$chisq_tol
    LEVELS = sapply(X,function(x) length(unique(x)))
    X2 = X[,LEVELS>1&LEVELS<=MAX_LEVEL(),drop=FALSE]
    pvalues = pvalue_table[names(X2)]
    #print(pvalues)
    #print(as.numeric(pvalues)<=as.numeric(MAX_P()))
    #print(dim(X))
    #print(LEVELS)
    #print(length(LEVELS))
    #print(dim(X2))
    D = X2[,as.numeric(pvalues)<=as.numeric(MAX_P()),drop=FALSE]
    #print(5)
    
    R = list()
    R$accepted = names(D)
    
    rejected_names = setdiff(names(X),names(D))
    #TEMP = data_ranked_Chisq(X[,rejected_names],Y,floor(nrow(X)*tol))$DATA
    
    
    R$rejected = rejected_names
    R
  })
  })
  
  output$attr_names_log = renderPrint({
    attr_names()
  })
  
  output$CHOOSER = renderUI({
    
    left_choices = attr_names()$rejected
    right_choices = attr_names()$accepted
#     pvalues = if(input$head_or_drive =="DRIVE"){
#       chisq_table_drive()[choices]
#     } else {
#       chisq_table_head()[choices]
#     }
    
    pvalues_left = chisq_table()[left_choices]
    pvalues_right = chisq_table()[right_choices]


    left_choices = left_choices[order(pvalues_left)]

    right_choices = right_choices[order(pvalues_right)]
#     print(pvalues)
#     print(choices)
    
    chooserInput("chooser",
                 leftLabel="In",
                 rightLabel="Out",
                 leftChoices = left_choices,
                 rightChoices =right_choices,
                 size=max(length(left_choices),length(right_choices),na.rm = TRUE),
                 multiple=TRUE)
  })
  
  SELECTED_NAMES = reactive({
    R = input$chooser$right
    
    pvalues = chisq_table()[R]
    
    R = R[order(pvalues)] 
    
    validate(need(length(R)>0,"Select at least one variable."))
    R
  })
  
  output$SELECTED_NAMES_log = renderPrint({
    SELECTED_NAMES()
  })
  
  UNSELECTED_NAMES = reactive({

    input$chooser$left    
  })
  
  output$UNSELECTED_NAMES_log = renderPrint({
    UNSELECTED_NAMES()
  })
  
  output$left_table = renderTable({
    req(UNSELECTED_NAMES(),ATTR(),chisq_table_head(),chisq_table_drive())
    
    
    # for uploading a new file
    flag = all(UNSELECTED_NAMES() %in%  names(ATTR()))
    validate(need(flag == 1, "New data uploaded.\nClick Process Data."))
    
    X = data.frame(
      ATTRs =   UNSELECTED_NAMES(),
      P_Head =  round(chisq_table_head()[UNSELECTED_NAMES()],3),
      P_Drive = round(chisq_table_drive()[UNSELECTED_NAMES()],3),
      Lvls = getLevels(ATTR()[,UNSELECTED_NAMES()])
    )
    rownames(X)=NULL
    
    if(input$head_or_drive == "HEAD"){
      R = X[order(X$P_Head),]
      rownames(R) = 1:nrow(R)
    } else {
      R = X[order(X$P_Drive),]
      R = R[,c("ATTRs","P_Drive","P_Head","Lvls")]
      rownames(R) = 1:nrow(R)
    }
    
    if(input$no_agg) {R = R[,names(R) != "P_Drive"]}
    R
  }) 
  
  output$right_table = renderTable({
    
    
    
    req(SELECTED_NAMES())
    
    # for uploading a new file
    flag = all(SELECTED_NAMES() %in%  names(ATTR()))
    validate(need(flag == 1, "New data uploaded.\nClick Process Data."))
    
    X = data.frame(
      ATTRs =   SELECTED_NAMES(),
      P_Head =  round(chisq_table_head()[SELECTED_NAMES()],3),
      P_Drive = round(chisq_table_drive()[SELECTED_NAMES()],3),
      Lvls = sapply(ATTR()[,SELECTED_NAMES(),drop=FALSE], function(x) length(unique(x)))
    )
    rownames(X)=NULL

    
    if(input$head_or_drive == "HEAD"){
      R = X[order(X$P_Head),]
      rownames(R) = 1:nrow(R)
    } else {
      R = X[order(X$P_Drive),]
      R = R[,c("ATTRs","P_Drive","P_Head","Lvls")]
      rownames(R) = 1:nrow(R)
    }
    
    if(input$no_agg) {R = R[,names(R) != "P_Drive"]}
    R
    
    
  }) 
  
  #--------------------------data manipulation 2--------------------

  attr = reactive({
    req(SELECTED_NAMES())
    if(input$head_or_drive =="HEAD"){
      R = ATTR()[,SELECTED_NAMES(),drop=FALSE]
    } else {
      R = ATTR_drive()[,SELECTED_NAMES(),drop=FALSE]
    }
    R
  })

  y = reactive({
    if(input$head_or_drive =="HEAD"){
      R = Y()
    } else {
      R = Y_drive()
    }
    R
  })

  dsn = reactive({
    if(input$head_or_drive =="HEAD"){
      R = DSN()
    } else {
      R = DSN_drive()
    }
    R
  })

  output$attr_y_dsn_log = renderPrint({
    print_summary(attr())
    #print(list(table(y()),print_summary(attr()),length(dsn())))
  })

  attr_ranked = reactive({
    withProgress(message = 'Ranking Attributes', {
      
    X = attr()
    
    
    tol = input$chisq_tol
    y = y()
    R = data_ranked_Score(X = X,y = y, nrow(X)*tol)$DATA
    
    cat("-----------------------\n")
    print("attr_ranked")
    print(names(R))
#     print(round(data_ranked_Score(X = X,y = y, nrow(X)*tol)$P,4))
    cat("-----------------------\n")
    
    })
    R
  })

  output$attr_ranked_log = renderPrint({
    print(list(names(attr_ranked()),dim(attr_ranked())))
  })

  NUM_CLUST_A = reactive({
#     x = ncol(attr_ranked())
#     n = ceiling(x/3)
#     n  
    length(unique(labelx()$group))
  })

  cluster_fit = eventReactive(input$analyze,{
    withProgress(message = 'Clustering Attributes', {
      
    K = attr_ranked()
    
#     print(dim(K))
#     print(names(K))
    
    validate(need(ncol(K)>2,"Need at least 3 attributes to cluster."))
    
#    write.csv(K,"D:/reports/for_debugging.csv",row.names=FALSE)
    
    validate(need(
      all(
      sapply(K, function(x) length(unique(x)))>1
      ),
      "After aggregation, some columns have only 1 value."))
    
#     print(sapply(K,is.numeric))
#     print(sapply(K,is.character))
#     print(sapply(K,is.factor))
    
    set.seed(123)
#    K[] = lapply(K,as.factor)
    
    
    fit = NULL
    count = 1
    seeds = 1:10
    while(is.null(fit) & count<=10){
      print(paste("Robust Column clustering round",count))      
      set.seed(seeds[count])
      if( count == 1){
        fit = tryCatch({ ClustOfVar::hclustvar(X.quali = Random_Sample_prop(K,1))},
                       warning = function(w) {
                         suppressWarnings({
                           ClustOfVar::hclustvar(X.quali = Random_Sample_prop(K,1))
                         })
                       },
                       error = function(e) {NULL}
        )
      } else {
        fit = tryCatch({ ClustOfVar::hclustvar(X.quali = Random_Sample_prop(K[,sample(1:ncol(K))],1))},
                       warning = function(w) {
                         suppressWarnings({
                           ClustOfVar::hclustvar(X.quali = Random_Sample_prop(K[,sample(1:ncol(K))],1))
                         })
                       },
                       error = function(e) {NULL}
        )
      }
      count = count+1
      
    }
    
    
#     fit = tryCatch({ ClustOfVar::hclustvar(X.quali = Random_Sample_prop(K,1))},
#                    warning = function(w) {
#                      suppressWarnings({
#                       ClustOfVar::hclustvar(X.quali = Random_Sample_prop(K[,sample(1:ncol(K))],1))
#                       })
#                      },
#                    error = function(e) {NULL}
#     )

#fit= ClustOfVar::hclustvar(X.quali = K)
    
    validate(need(!is.null(fit),"Data failed the robust column clustering. Try changing p value, change analysis level or examine raw data."))
    fit
    })
  })

#   PickOneFromEach = reactive({
#     fit = cluster_fit()
#     
#     labels = cutree(fit,NUM_CLUST_A())
#     labelx = data.frame(Names=names(labels),group = paste("Group",as.vector(labels)),num=as.vector(labels))
#     ddply(labelx,.(num), function(df) df[1,])$Names   
#     
#   })

  labelx = reactive({
    withProgress(message = 'Generating labels for plotting', {
      
    fit = cluster_fit()
    #labels = cutree(fit,NUM_CLUST_A())

    x = ncol(attr_ranked())
    n = ceiling(x/3) 
    
    labels = tryCatch({
      print("clust cutree code 1 - success")
      cutree(fit, h=ATTR_CLUST_H())
    },warning = function(w){
      print("clust cutree code 1 - success with warnings")
      cutree(fit, h=ATTR_CLUST_H())
    },error = function(e){
      print("clust cutree code 0 - failure with h, cutree by k instead (k = ceiling(ncol(X)/3)) ")
      cutree(fit,k = n)
    })
    
    
    labelx = data.frame(Names=names(labels),group = paste("Group",as.vector(labels)),num=as.vector(labels))
    
    cat("-------------\n")
    print("labelx")
    print(labelx)
    cat("-------------\n")
    
    
    labelx
    })
  })

  output$barplot_text = renderText({
    cluster_fit()
    n = ncol(attr_ranked())
    m = NUM_CLUST_A()
    paste0("There are ",n," attributes and ",m," clusters analyzed by ",input$head_or_drive,".")
  })

  observeEvent(input$analyze,{
    withProgress(message = 'Drawing Summary Barplots', {
      
    req(attr_ranked(),cluster_fit(),labelx())
     K = attr_ranked()
# 
#     set.seed(123)
#     fit = tryCatch({ ClustOfVar::hclustvar(X.quali = Random_Sample_prop(K,1))},
#                    warning = function(w) { NULL},
#                    error = function(e) {NULL}
#     )
#     
#     validate(need(!is.null(fit),"Clustering Issue. Try changing p value or examine raw data."))
    
    fit = cluster_fit()
    
#     labels = cutree(fit,NUM_CLUST_A())
#     labelx = data.frame(Names=names(labels),group = paste("Group",as.vector(labels)),num=as.vector(labels))
    labelx = labelx()
    
    D = rearrange(X = K, labelx, chisq_table())

cat("-----------------------\n")
print("rearranged")
print(names(D))
cat("-----------------------\n")
    
#    print(labelx)
    
    output$attr_clust_chart = renderPlot({
#       N=NUM_CLUST_A()
#       cols = rainbow(N)
#       
#       dd.row = as.dendrogram(fit)
#       ddata_x <- dendro_data(dd.row)
#       temp = cutree(fit, k = N)
#       lab <- ggdendro::label(ddata_x)
#       x = c()
#       for(i in 1:nrow(lab)){
#         x[i] = paste("clust", as.vector(temp[lab$label[i] == names(temp)]), sep = "")
#       }
#       lab$group <- x
#       
#       p1 <- ggplot(segment(ddata_x)) + 
#         geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
#         geom_text(data = lab, aes(label = label, x = x, y = -.05, colour = group),  # y = -.05 adds a little space between label and tree
#                   size = 4, hjust = 1) +
#         scale_x_continuous(expand = c(0, .5)) +   # 1. Add half a unit to each end of the vertical axis
#         expand_limits(y = -0.4) +   # 2. Make room for labels 
#         theme_classic() + 
#         scale_colour_manual(values = cols) + 
#         coord_flip() +
#         theme(legend.position = "none", axis.line = element_blank(),
#               axis.text = element_blank(), axis.title = element_blank(), 
#               axis.ticks = element_blank(), 
#               axis.ticks.length = unit(0, "cm")) 
#       
#       df2 <- data.frame(cluster = cutree(fit, N), 
#                         states = factor(fit$labels, levels = fit$labels[fit$order]))
#       df3 <- ddply(df2, .(cluster),summarise,pos=mean(as.numeric(states)))
#       p2 <- ggplot(df2, aes(states, y = 1, 
#                             fill = factor(as.character(cluster)))) +   # 'as.character' - so that colours match with 10 or more clusters
#         geom_tile() +
#         scale_y_continuous(expand = c(0, 0)) + 
#         scale_x_discrete(expand = c(0, 0)) +
#         coord_flip() +
#         geom_text(data = df3,aes(x = pos, label = cluster, size = 12)) +
#         scale_fill_manual(values = cols)
#       
#       gp1 <- ggplotGrob(p1)  # Get ggplot grobs
#       gp2 <- ggplotGrob(p2)  
#       
#       gp2 <- gp2[3, 4]      # 3. Grab plot panel only from tiles plot (thus, no margins)
#       gp1 <- gtable_add_grob(gp1, gp2, t = 3, l = 2, name = "tiles")  # 3. Insert it into dendrogram plot
#       gp1$widths[2] = unit(1, "cm")  # 3. Set width of column containing tiles
#       
#       grid.newpage()
#       grid.draw(gp1)
      

      
      df2<-data.frame(cluster=cutree(fit,NUM_CLUST_A()),states=factor(fit$labels,levels=fit$labels[fit$order]))
      df3<-ddply(df2,.(cluster),summarise,pos=mean(as.numeric(states)))
      
      p1 = ggdendrogram(as.dendrogram(fit), rotate=TRUE)+
        scale_x_continuous(expand = c(0, 0.5), labels = levels(df2$states), breaks = 1:length(df2$states)) +
        scale_y_continuous(expand = c(0.02, 0)) 
      
      
      p2 = ggplot(df2,aes(states,y=1,fill=factor(cluster)))+geom_tile()+
        scale_y_continuous(expand=c(0,0))+
        scale_x_discrete(expand = c(0, 0)) +
        theme(axis.title=element_blank(),
#              axis.ticks=element_blank(),
#              axis.text=element_blank(),
             axis.ticks.x=element_blank(),
             axis.text.x=element_blank(),


              legend.position="none")+
        coord_flip()+
        geom_text(data=df3,aes(x=pos,label=cluster))
      gp1<-ggplotGrob(p1)
      gp2<-ggplotGrob(p2)  
      maxHeight = grid::unit.pmax(gp1$heights[2:5], gp2$heights[2:5])
      gp1$heights[2:5] <- as.list(maxHeight)
      gp2$heights[2:5] <- as.list(maxHeight)
      grid.arrange(gp2, gp1, ncol=2,widths=c(1/6,5/6))
      
      
      
      
      
    })
    
    output$ATTR_CLUST_CHART = renderUI({
      plotOutput("attr_clust_chart",height=paste0(ncol(attr_ranked())*20,"px")) 
    })
    
    source("Attribute_Summary.R",local = TRUE)
    
    
    
 
    })
  })
  #observe ends up





  source("Attribute_Detail.R",local = TRUE)



  PARA = reactive({
    withProgress(message = 'Extracting Parametrics', {
      
    
    req(RAW())
    
    X = RAW()
    
     X = X[,sapply(X,is.numeric)]
    
     MISSINGS = sapply(X,function(x) sum(is.na(x),na.rm = TRUE))
     
    cbind( MISSINGS)
    
     UNIQUE = sapply(X,function(x) length(unique(x)))
     
     cbind(UNIQUE)
     
     MISSINGS_PERCENT = MISSINGS/nrow(X)
 
    IN = (MISSINGS_PERCENT < input$para_missing_percent) & (UNIQUE > input$para_unique)
    })
    print(sum(IN))
    validate(need(sum(IN)>0,"All parametrics are of low quality. Examine Raw data or view summary.\n Or try increase missing percent tolerance in advanced settings"))
    R = X[,IN]
    validate(need(ncol(R)>0,"All parametrics are of low quality. Examine Raw data or view summary.\n Or try increase missing percent tolerance in advanced settings"))
    R
    
  })

  output$PARA_log = renderPrint({
    
      print_summary(PARA())
    
  })


  PARA_fit = reactive({
    withProgress(message = 'Ranking Parametrics', {
      
    req(PARA(),Y())
    X = PARA()

    Y = Y()
    fit=data_ranked_logist(X,Y=="F")
    })
    fit
  })

  para = reactive({
    req(PARA_fit())
    fit = PARA_fit()
    fit$DATA
  })

  output$para_log = renderPrint({
    
    print_summary(para())
    
  })

  para_p_table = reactive({
    req(PARA_fit())
    fit = PARA_fit()
    fit$P
  })

  output$para_p_table_log = renderPrint({
    para_p_table()
  })



  source("Parametric_Summary.R",local = TRUE)

  source("Parametric_Detail.R",local = TRUE)

  source("Interaction.R", local = TRUE)

#  source("Discrepancy.R",local = TRUE)

  source("report.R",local = TRUE)

  source("Pivot.R", local = TRUE)

  output$raw_data = renderDataTable({
    upload()
  })








  my.percent<- function(x) {
    if(length(x)==1) if(x==0) return(paste0(0,"%") )
    return(scales::percent(x) )
  }

# summary

  output$UPLOAD_summary = renderTable({
    withProgress(message = 'Summarizing', {
      req(upload())
      X = upload()
      X = X[,sort(names(X))]
      n = nrow(X)
      LEVELS = sapply(X, function(x) length(unique(x)))
      MISSING = sapply(X, function(x) my.percent(  sum(x=="N/A"|is.na(x))/n  ))
      data.frame(NAMES = names(LEVELS),LEVELS = as.vector(LEVELS), MISSING_PROPORTION = as.vector(MISSING))      
    })
  })


  output$ATTR_summary = renderTable({
    withProgress(message = 'Summarizing', {
      req(upload())
      X = ATTR()
      X = X[,sort(names(X))]
      n = nrow(X)
      LEVELS = sapply(X, function(x) length(unique(x)))
      MISSING = sapply(X, function(x) my.percent(  sum(x=="N/A")/n  ))
      data.frame(NAMES = names(LEVELS),LEVELS = as.vector(LEVELS), MISSING_PROPORTION = as.vector(MISSING))
    })
  })

  output$PARA_summary = renderTable({
    withProgress(message = 'Summarizing', {
      req(upload())
      X = PARA()
      X = X[,sort(names(X))]
      n = nrow(X)
      MISSING = sapply(X, function(x) my.percent(  sum(is.na(x))/n  ))      
      data.frame(NAMES = names(MISSING), MISSING_PROPORTION = as.vector(MISSING))
    })
  })  



ReportList =                                                              
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
)

observeEvent(input$reportSelectAll,{
  if(is.null(input$reportList)){
    updateCheckboxGroupInput(session ,"reportList", selected =ReportList)
  } else{
    updateCheckboxGroupInput(session ,"reportList", selected = "")
  }
})



exportedFile = reactive({
 # req(upload(),ATTR_drive(),DSN_drive(),Y_drive())
  validate(need(!is.null(upload()),"Please upload and process data first"))
  R = data.frame(DRIVE_SERIAL_NUM = DSN_drive(),
                            STATUS = Y_drive(),
                            ATTR_drive()
  )
  R
})

output$downloadAGG = downloadHandler(
  filename = function(){"aggregated_attrs.csv"},
  content = function(file){
    write.csv( exportedFile()  ,file, row.names=FALSE)
  }
  )





##################################### pivot module ##############################
library(rpivotTable)
output$pivot = renderRpivotTable({
  
  
  withProgress(message = '生成数据透视表', {
    

    
    if(input$pivot_level == "处理过的行级别数据") {
      D = data.frame(
        DRIVE_SERIAL_NUM = DSN(),
        STATUS = Y(),
        ATTR(),
        PARA()
      )
    } else if(input$pivot_level == "处理过的聚集级别数据"){
      D = data.frame(
        DRIVE_SERIAL_NUM = DSN_drive(),
        STATUS = Y_drive(),
        ATTR_drive()
      )
      
    } else {
      
      UPLOAD = upload()
      LEVELS = sapply(UPLOAD, function(x) length(unique(x)))
      UPLOAD = UPLOAD[,LEVELS<120]
      
      D = data.frame(
        DRIVE_SERIAL_NUM = DSN(),
        STATUS = Y(),
        UPLOAD
      )      
    }
    
    D = D[,sort(names(D))]
    D = data.frame(D[,c("DRIVE_SERIAL_NUM","STATUS")],
                   D[,setdiff(names(D),c("DRIVE_SERIAL_NUM","STATUS"))]
    )
    rpivotTable(D)
  })
  
})

}