exclude_double = function(X){
  X[,  !(sapply(X,is.numeric) & !sapply(X,is.integer)) ]
}

WIDTH = 0.1
# X = data.frame(a=1:10,b=rnorm(10),c=rpois(10,10))
# exclude_double(X)

aggregate_data = function(group,flag,X){
#  print(0)
  library(data.table)
  print(data.frame(group=length(group),flag=length(flag),nrow_X = nrow(X)))
#  print(1)
  temp = data.frame(group,flag,X)
#  print(2)
  D = data.table(temp)
  D2 = D[D[, .I[which.min(flag)] , by = group]$V1]
  R = list()
  R$DATA = data.frame(D2[,3:ncol(D2),with=FALSE])
  R$group = data.frame(D2[,1,with = FALSE])[,1,drop=TRUE]
  R$flag = data.frame(D2[,2,with=FALSE])[,1,drop=TRUE]
#  print(4)
  R
}

#
print_summary = function(X){
  data.frame(Nrow = nrow(X), 
             Ncol = ncol(X), 
             has.all.na = sum(sapply(X,function(x) all(is.na(x))))>0,
             Numeric = sum(sapply(X,is.numeric)),
             Character = sum(sapply(X,is.character)),
             Factor = sum(sapply(X,is.factor))
             )
}


# used in barplots
chisq_test = function(x,y,n,sig=3,ignoreNA = 1){
  options(warn=1)
  library(plyr)
  M = data.frame(x=x,y=y,stringsAsFactors = FALSE)
 # M = M[!is.na(x),]
  if(ignoreNA) {
    M = M[!x=="N/A",]
  }
  
  x = M[,1]
  y = M[,2]
  
 
 library(dplyr)
 temp = M %>% group_by(x) %>% summarise(sum = sum(y=="F"))    
 
 #>=3 group  
 big1 = temp$x[temp$sum>sig]
 
 #>= n group
 big2 = names(table(x))[table(x)>n]
 
 big_group = union(big1,big2)  
 
 
 
 
#   #>=3 group
#   temp = plyr::ddply(M, .(x), summarise, sum = sum(y=="F"), .drop = FALSE)
#   index1 = temp[,2]>=sig
#   
#   #>= n group
#   index2 = as.vector(table(x)>=n)
#   
#   big_group = names(table(x))[ index1 |index2 ]
  
  M2 = M[M$x %in% big_group,]
  R = 400
  
#   if(  length(big_group)<=1  || length(unique(M2$x))<=1 || length(unique(M2$y))<=1 ) {
#     R= 401
#   } else{
#     suppressWarnings({R = (chisq.test(M2$x,M2$y)$p.value)})
#   }
  R = tryCatch({
    chisq.test(M2$x,M2$y)$p.value
  },
  warning = function(w){
    suppressWarnings({R = (chisq.test(M2$x,M2$y)$p.value)})
  },
  error = function(e){
    401
  }
  )
  return(R)
}

# used in Ranking attributes
Score = function(x,y,n,sig=3, ignoreNA=1){
  options(warn=1)
  library(plyr)
  M = data.frame(x=x,y=y,stringsAsFactors = FALSE)
  if(ignoreNA) {
    M = M[!x=="N/A",]
  }
  #M = M[!is.na(x),]
  x = M[,1]
  y = M[,2]

  #>=3 group
  #   temp = plyr::ddply(M, .(x), summarise, sum = sum(y=="F"), .drop = FALSE)
  #   index1 = temp[,2]>=sig

  #>= n group
  #   index2 = as.vector(table(x)>=n)
  
  #   big_group = names(table(x))[ index1 |index2 ]
  
  library(dplyr)
  temp = M %>% group_by(x) %>% summarise(sum = sum(y=="F"))  
  #>=3 group  
  big1 = temp$x[temp$sum>sig]
  
  #>= n group
  big2 = names(table(x))[table(x)>n]
  
  big_group = union(big1,big2)  

  M2 = M[M$x %in% big_group,]

  R = tryCatch({
      chisq.test(M2$x,M2$y)$p.value
    },
    warning = function(w){
      suppressWarnings({R = (chisq.test(M2$x,M2$y)$p.value)})
    },
    error = function(e){
      401
    }
  )
  
  #maxF = max(temp[,2])
  if(R == 401){
    R2 = 501
  } else{
    R2 = R*length(unique(x))/max(temp[,2])
  }
  return(R2)
}


data_ranked_Chisq = function(X,y,fisher_n){
  R = list()
  n = ncol(X)
  pvalues = numeric(n)
  #   for (i in 1:n){
  #     pvalues[i] = chisq_test(X[,i],y,fisher_n)
  #   }
  pvalues = as.vector(sapply(X,function(x) chisq_test(x,y,fisher_n)))
  
  ORDER = order(pvalues)
  R$DATA = X[,ORDER]
  R$P = sort(pvalues)
  return(R)
}


data_ranked_Score = function(X,y,fisher_n = 5){
  R =list()
  score = as.vector(sapply(X,function(x) Score(x,y,fisher_n)))
  pvalues = as.vector(sapply(X,function(x) chisq_test(x,y,fisher_n)))
  ORDER = order(score)
  R$DATA = X[,ORDER]
  R$P = pvalues[ORDER]
  return(R)
}

#input a vector, output its number of unique values, NA as a value
uniqueLength = function(x){
  length(unique(x))
}

# input a data.frame, output a named vector of number of unique values
getLevels = function(X){
  sapply(X,function(x) length(unique(x)))
}

# input a data,frame, output a data.frame with columns with number of unique values less than n
getATTR = function(X,n){
  # force to characters
  X = exclude_double(X)
  X[] = lapply(X,as.character)
  
  # replace NA with "N/A"
  X[is.na(X)|X==""]="N/A"
  LEVELS = sapply(X,function(x) length(unique(x)))
  R = X[,LEVELS>1&LEVELS<= n, drop=FALSE]
  return(R)
}

# ggplot color generator
ggplotColours <- function(n=6, h=c(0, 360) + 15){
  if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

# random sampling a data.frame
Random_Sample_prop = function(X, prop = 1){
  if (prop>1 ){error("prop > 1!")}
  temp = runif(nrow(X))
  R = X[temp<=prop,]
  return(R)
}

# delete columns, this one is redundant with getATTR()
delete_col_by_n_level = function(X, n=10){
  L = as.vector(sapply(X,function(x) length(unique(x))))
  keep = (L < n)&(L>1)
  R = X[,keep]
  return(R)
}

# not used often
delete_col_by_missing_prop = function(X, prop=0.1){
  keep = ( apply(is.na(X),2,sum) < nrow(X)*prop)  
  R = X[,keep]
  return(R)
}


barplot_1 = function(x,FAIL,xname=" ",pvalue,L,num_clust,min_to_display=-1,showbarcolor=0){
  library(ggplot2)
  library(plyr)
  library(gtable)
  D = data.frame(x,FAIL,min_to_display)
  temp = ddply(D,"x",summarise,N=length(FAIL),n=sum(FAIL=="F"),min_to_display=mean(min_to_display))
  temp = ddply(temp,"x",transform,
               labels_N = ifelse(N>as.numeric(min_to_display),paste(as.character(N))," "),
               fr=round(n/N,5))
  
  ii = L[L[,1]==xname,3]
  
  COLOR = ggplotColours(num_clust)[ii]
  
  #   TITLE = paste(xname,"\n",
  #                 as.character(
  #                   L[L[,1]==xname,2]
  #                 ),"\n",
  #                 "p:",round(pvalue,3)
  #   )
  title1 = xname
  title2 = as.character(
    L[L[,1]==xname,2]
  )
  title3 = paste("p:",round(pvalue,3))
  TITLE = bquote(
    atop(
      bold(.(title1)),
      atop(
        .(title2),.(title3)
        
      )
    )
  )
  p1 = ggplot(temp,aes(x=x,y=fr)) + 
    geom_bar(stat="identity",fill = ggplotColours(2)[2])+              #,fill=COLOR  ) + 
    geom_text(aes(label = n),stat="identity",vjust=-0.2,hjust=0.5,color="red",size=4)+
    geom_text(aes(label = labels_N),stat="identity",vjust= 1.5,hjust=0.5,color="black",size=3.5)+
    labs(title = TITLE)+
    theme(legend.position="none",
          axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 0),
          axis.title.x = element_blank(),
          plot.title = element_text( colour = COLOR,size=rel(1.4))
    )+
    scale_y_continuous(labels  = percent) 
  
  if(showbarcolor==1){
    p2 = p1 + geom_bar(stat="identity",fill = COLOR)+
      geom_text(aes(label = labels_N),stat="identity",vjust= 1.5,hjust=0.5,color="black",size=3.5)
    
  } else {
    p2 = p1
  }
  
  return(ggplotGrob(p2))
}

barplot_1_custom = function(x,FAIL,xname=" ",pvalue,L=NULL,num_clust,min_to_display=-1,showbarcolor=0){
  library(ggplot2)
  library(plyr)
  library(gtable)
  D = data.frame(x,FAIL,min_to_display)
  temp = ddply(D,"x",summarise,N=length(FAIL),n=sum(FAIL=="F"),min_to_display=mean(min_to_display))
  temp = ddply(temp,"x",transform,
               labels_N = ifelse(N>as.numeric(min_to_display),paste(as.character(N))," "),
               fr=round(n/N,5))
  
  #  ii = L[L[,1]==xname,3]
  #   print(1)
  #  COLOR = ggplotColours(num_clust)[ii]
  
  #   TITLE = paste(xname,"\n",
  #                 as.character(
  #                   L[L[,1]==xname,2]
  #                 ),"\n",
  #                 "p:",round(pvalue,3)
  #   )
  #   print(2)
  #   title1 = xname
  #   title2 = as.character(
  #     L[L[,1]==xname,2]
  #   )
  #   title3 = paste("p:",round(pvalue,3))
  #   TITLE = bquote(
  #     atop(
  #       bold(.(title1)),
  #       atop(
  #         .(title2),.(title3)
  #         
  #       )
  #     )
  #   )
  #   print(3)
  p1 = ggplot(temp,aes(x=x,y=fr)) + 
    geom_bar(stat="identity",fill = ggplotColours(2)[2])+              #,fill=COLOR  ) + 
    geom_text(aes(label = n),stat="identity",vjust=-0.2,hjust=0.5,color="red",size=4)+
    geom_text(aes(label = labels_N),stat="identity",vjust= 1.5,hjust=0.5,color="black",size=3.5)+
    ggtitle(xname)+
    theme(legend.position="none",
          axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 0),
          axis.title.x = element_blank(),
          plot.title = element_text( colour = "black",size=rel(1.4))
    )+
    scale_y_continuous(labels  = percent) 
  
  #   print(4)
  if(showbarcolor==1){
    p2 = p1 + geom_bar(stat="identity",fill = COLOR)+
      geom_text(aes(label = labels_N),stat="identity",vjust= 1.5,hjust=0.5,color="black",size=3.5)
    
  } else {
    p2 = p1
  }
  #   print(5)
  #   print(p2)
  #   print(6)
  #   print(ggplotGrob(p2))
  #   print(7)
  return(ggplotGrob(p2))
}


barplot_2 = function(x1,x2,FAIL,xname1="factor1",xname2="factor2"){
  library(ggplot2)
  library(plyr)
  library(gtable)
  D = data.frame(x1,x2,FAIL)
  
  
  temp = ddply(D,c("x1","x2"),summarise,N=length(FAIL),n=sum(FAIL=="F"))
  temp = ddply(temp,c("x1","x2"),transform,
               labels_N = paste(as.character(N)),
               fr=round(n/N,4))
  
  
  p1 = ggplot(temp,aes(x=x1,y=fr,fill=x1)) + 
    geom_bar(stat="identity"  )  +
    facet_grid(. ~ x2)+
    geom_text(aes(label = n,group=x2),stat="identity",vjust=-0.2,hjust=0.5,color="red",size=4)+
    geom_text(aes(label = labels_N),stat="identity",vjust= 1.5,hjust=0.5,color="black",size=4)+
    labs(title = xname2)+
    xlab(paste(xname1))+
    ylab("FAILURE RATE")+
    theme(legend.position="none",
          #axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 0),
          axis.title.x = element_text(colour="grey50"),
          plot.title = element_text(size=rel(1))
    )+
    scale_y_continuous(labels  = percent)
  p1  
}

rearrange = function(X,clust,p_table = NULL){
  
  if(!is.null(p_table)){
    clust$pvalue = as.vector(p_table[as.character(clust$Names)])
#     print(clust$Names)
#     print(p_table[clust$Names])
#     print(clust)
    clust = clust[order(clust$pvalue),]
#    print(clust)
    clust = clust[,-4]
  }
  
  N = ncol(X)
  numclust = length(unique(clust$num))
  for(i in 1:N){
    if(i==1){
      R=c()
      R[1]=clust$num[1]
      index = 1
    } 
    if( !(clust$num[i] %in% R)){
      index = index + 1
      R[index] = clust$num[i]
    }
  }
  
  #R=1:numclust
  Names = as.character(clust$Names)
  for(i in 1:numclust){
    if(i==1) K=Names[clust$num==R[i]]
    if(i>1){
      temp = Names[clust$num==R[i]]
      K=c(K,temp)
    }
  }
  X[,K]
}

data_ranked_logist = function(X,y){
  n = ncol(X)
  #   DATA = na.omit(data.frame(X,y))
  #   XX = DATA[,1:n]
  #   y = DATA$y
  pvalues = rep(1,n)
  for (i in 1:n){
    fit = glm(y~X[,i],family="binomial")
    pvalues[i] = coef(summary(fit))[2,4]
  }
  ORDER = order(pvalues)
  R =list()
  R$DATA = X[,ORDER]
  R$P = round(sort(pvalues),3)
  names(R$P) = names(R$DATA)
  #attr(R,"pvalues") = data.frame(NAMES = as.character(names(X)), p = round(pvalues,3))
  return(R)  
}


InteractionMatrix = function(A,P,STATUS){
  
  A = data.frame(A)
  P = data.frame(P)
  M = expand.grid(names(A),names(P),stringsAsFactors = FALSE)
  names(M) = c("ATTR","PARA")
  M = cbind(M,pvalue = rep(401,nrow(M)), ANOVA = rep(402,nrow(M)))
  
  ANOVA = sapply(1:nrow(M), function(i){
    attr_name = M$ATTR[i]
    para_name = M$PARA[i]
    R = 1
    tryCatch({
      fit = aov(P[,para_name] ~ A[,attr_name])
      R =     summary(fit)[[1]][[1,"Pr(>F)"]]
    }, error = function(e){
      R = 1
    })
    return(R)
  })
  
  M$ANOVA = ANOVA
  
  M = M[M$ANOVA<0.01,]
  
  pvalues = sapply(1:nrow(M), function(i){
    attr_name = M$ATTR[i]
    para_name = M$PARA[i]
    min(PvalueLogistGroup(attr = A[,attr_name],P[,para_name],STATUS))
  })
  
  M$pvalue = pvalues
  
  R = M[order(M$pvalue),]
  R = R[R$pvalue<0.05,]
  rownames(R)=NULL
  R
}

PlotLogistGroup = function(attr,para,STATUS,attr_name="ATTR",para_name="PARA",cutoff = 0.4,hide=1,uselog=0){
  library(ggplot2)
  
  attr = as.character(attr)
  STATUS = as.character(STATUS)
  STATUS[STATUS!="F"]="P"
  
  if(all(is.na(para))) para = rep(0,length(para))
  
  D = data.frame(para,attr,STATUS,stringsAsFactors = FALSE)
  # The following line is crucial. Hide or show attr levels with all NA.
  if(hide) {
    D = na.omit(D)
  }
  D2 = D[D$STATUS == "F",]
  
  temp = c(by(D$para, D$attr, function(x) {
    if (all(is.na(x))) {R = median(para,na.rm = TRUE)} else {
      R = min(x,na.rm = TRUE)
    }
    return(R)
  }))
  
  # positions of pvalues
  temp[]=min(D$para,na.rm = TRUE)
  
  # sample sizes and position
  sample_size = sapply(split(D,D$attr),nrow)
  temp2=temp
  temp2[]=max(D$para,na.rm = TRUE)
  
  #calculate p values in-group
  pvalues = sapply(split(D,D$attr),function(df) {
    if (all(is.na(df$para))){
      #if na.omit used, not useful
      R = 1
    } else {
      try({
        suppressWarnings({fit = glm(STATUS=="F"~para,df,family="binomial")  })
        X = coef(summary(fit))
        # If only 1 record in a level, X will have only 1 row.
        if(nrow(X)==1){
          R = 1
        } else {
          # extract p value
          R = X[2,4]
        } 
      })
    }
    return(R)
  })
  
  #pvalues = PvalueLogistGroup(attr = D$attr,para = D$para,STATUS = STATUS)
  
  # help df
  
  min_p = min(pvalues,na.rm = TRUE)
  
  ANOVA_p = 1
  tryCatch({
    fit = aov(D$para ~ D$attr)
    ANOVA_p =     summary(fit)[[1]][[1,"Pr(>F)"]]
  }, error = function(e){
    ANOVA_p = 1
  })
  
  #  TITLE = paste("Min Logist P value: ",round(min_p,4),". ANOVA P value: ",round(ANOVA_p,4),".",sep = "")
  TITLE = paste("Min Logist P value: ",round(min_p,4),".",sep = "")
  
  POS = data.frame(NAMES = names(temp),
                   position = temp,
                   pvalues = pvalues,
                   COLS = as.character(ifelse(pvalues<cutoff,"blue","red")    ),
                   sample_size = sample_size,
                   position2 = temp2)
  
  Random_Sample_prop = function(X, prop = 1){
    if (prop>1 ){error("prop > 1!")}
    temp = runif(nrow(X))
    R = X[temp<=prop,]
    return(R)
  }
  
  D3 = Random_Sample_prop(D,0.2)
  
  p = ggplot(D,aes(x=attr,y=para)) + 
    theme_bw()+
    
    geom_boxplot() + 
    geom_point(data = D2,aes(x=attr,y=para),color="red",size=4)+
    #geom_jitter()+
    #geom_jitter(data=D3,aes(x=attr,y=para),alpha=0.5)+
    xlab(attr_name)+
    ylab(para_name)+
    theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_text(data=POS, aes(x=NAMES, y=position, color = COLS,
                            label=paste(" ",round(pvalues,2))    )
              , size=4
              , vjust=0,hjust = 0.5)+
    geom_text(data=POS, aes(x=NAMES, y=position2, 
                            label=sample_size  )
              , size=4
              , vjust=0,hjust = 0.5)+
    ggtitle(TITLE)+
    theme(plot.title = element_text(size=15,color="blue"))+
    theme(axis.title.y=element_blank())
  
  if (uselog){
    #p = p + scale_y_continuous(trans = log_trans())
    p = p + scale_y_log10()
  } 
  p
}


PlotLogistGroup_jitter = function(attr,para,STATUS,attr_name="ATTR",para_name="PARA",cutoff = 0.4,hide=1,uselog=0){
  library(ggplot2)
  
  attr = as.character(attr)
  STATUS = as.character(STATUS)
  STATUS[STATUS!="F"]="P"
  
  if(all(is.na(para))) para = rep(0,length(para))
  
  D = data.frame(para,attr,STATUS,stringsAsFactors = FALSE)
  # The following line is crucial. Hide or show attr levels with all NA.
  if(hide) {
    D = na.omit(D)
  }
  D2 = D[D$STATUS == "F",]
  
  temp = c(by(D$para, D$attr, function(x) {
    if (all(is.na(x))) {R = median(para,na.rm = TRUE)} else {
      R = min(x,na.rm = TRUE)
    }
    return(R)
  }))
  
  # positions of pvalues
  temp[]=min(D$para,na.rm = TRUE)
  
  # sample sizes and position
  sample_size = sapply(split(D,D$attr),nrow)
  temp2=temp
  temp2[]=max(D$para,na.rm = TRUE)
  
  #calculate p values in-group
  pvalues = sapply(split(D,D$attr),function(df) {
    if (all(is.na(df$para))){
      #if na.omit used, not useful
      R = 1
    } else {
      try({
        suppressWarnings({fit = glm(STATUS=="F"~para,df,family="binomial")  })
        X = coef(summary(fit))
        # If only 1 record in a level, X will have only 1 row.
        if(nrow(X)==1){
          R = 1
        } else {
          # extract p value
          R = X[2,4]
        } 
      })
    }
    return(R)
  })
  
  #pvalues = PvalueLogistGroup(attr = D$attr,para = D$para,STATUS = STATUS)
  
  # help df
  
  min_p = min(pvalues,na.rm = TRUE)
  
  ANOVA_p = 1
  tryCatch({
    fit = aov(D$para ~ D$attr)
    ANOVA_p =     summary(fit)[[1]][[1,"Pr(>F)"]]
  }, error = function(e){
    ANOVA_p = 1
  })
  
  TITLE = paste("Min Logist P value: ",round(min_p,4),". ANOVA P value: ",round(ANOVA_p,4),".",sep = "")
  #TITLE = paste("Min Logist P value: ",round(min_p,4),".",sep = "")
  
  POS = data.frame(NAMES = names(temp),
                   position = temp,
                   pvalues = pvalues,
                   COLS = as.character(ifelse(pvalues<cutoff,"blue","red")    ),
                   sample_size = sample_size,
                   position2 = temp2)
  
  Random_Sample_prop = function(X, prop = 1){
    if (prop>1 ){error("prop > 1!")}
    temp = runif(nrow(X))
    R = X[temp<=prop,]
    return(R)
  }
  
  D3 = Random_Sample_prop(D,0.2)
  
  p = ggplot(D,aes(x=attr,y=para)) + 
    theme_bw()+
    geom_boxplot() + 
    geom_jitter(alpha=0.5,width=WIDTH)+

    geom_point(data = D2,aes(x=attr,y=para),color="red",size=4)+
    
    #geom_jitter(data=D3,aes(x=attr,y=para),alpha=0.5)+
    xlab(attr_name)+
    ylab(para_name)+
    theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_text(data=POS, aes(x=NAMES, y=position, color = COLS,
                            label=paste(" ",round(pvalues,2))    )
              , size=4
              , vjust=0,hjust = -0)+
    geom_text(data=POS, aes(x=NAMES, y=position2, 
                            label=sample_size  )
              , size=4
              , vjust=0,hjust = 0)+
    ggtitle(TITLE)+
    theme(plot.title = element_text(size=15,color="blue"))
  if (uselog){
    #p = p + scale_y_continuous(trans = log_trans())
    p = p + scale_y_log10()
  } 
  p
}

PvalueLogistGroup = function(attr,para,STATUS){
  
  attr = as.character(attr)
  STATUS = as.character(STATUS)
  STATUS[STATUS!="F"]="P"
  
  if(!is.numeric(para)) stop("You are inputting a character column as Y!")
  
  if(all(is.na(para))) para = rep(0,length(para))
  
  D = data.frame(para,attr,STATUS,stringsAsFactors = FALSE)
  
  D = na.omit(D)
  
  D2 = D[D$STATUS == "F",]
  
  pvalues = sapply(split(D,D$attr),function(df) {
    if (all(is.na(df$para))){
      #if na.omit used, not useful
      R = 1
    } else {
      try({
        suppressWarnings({fit = glm(STATUS=="F"~para,df,family="binomial")  })
        X = coef(summary(fit))
        # If only 1 record in a level, X will have only 1 row.
        if(nrow(X)==1){
          R = 1
        } else {
          # extract p value
          R = X[2,4]
        } 
      })
    }
    
    return(R)
  })
  return(pvalues)
}

#rank parametrics
data_ranked_ANOVA = function(X,y){
  
  
  n = ncol(X)
  y = as.character(y)
  #y[is.na(y)] = "N/A"
  pvalues = rep(1,n)
#  print(table(y))
  

  
  for(i in 1:n){
    x = X[,i]
    pvalues[i] = tryCatch({
      fit = aov(x ~ y)
      summary(fit)[[1]][[1,"Pr(>F)"]]
    }, error = function(e){
      1
    })
  }
  P = pvalues
  names(P) = names(X)
  DATA = X[,order(P)]
  P = sort(P)
  list(DATA=DATA, P = P)
  
}



#rank attributes
data_ranked_ANOVA_2 = function(X,y){
  
  # the data.frame of attributes
  X[] = lapply(X,as.character)
  n = ncol(X)
  
  y = y
  #y[is.na(y)] = "N/A"
  pvalues = rep(1,n)
  #  print(table(y))
  
  
  
  for(i in 1:n){
    x = X[,i]
    pvalues[i] = tryCatch({
      fit = aov(y ~ x)
      summary(fit)[[1]][[1,"Pr(>F)"]]
    }, error = function(e){
      1
    })
  }
  P = pvalues
  names(P) = names(X)
  DATA = X[,order(P)]
  P = sort(P)
  list(DATA=DATA, P = P)
  
}