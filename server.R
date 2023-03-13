# ********************************************
#
# The server logic for the R-Shiny App
# TypologyGenerator
#
# You can run the
# application by clicking 'Run App' above.
#
# ********************************************



library(shiny)
library(shinyjs)
library(rhandsontable)
library(vegan)
library(ade4)
library(dendextend)
library(ggplot2)
library(MASS)
library(ggradar)
library(dplyr)
library(tidyr)
library(ggpubr)
library(viridis)


shinyServer(function(input, output) {
  
  # ******************
  # Functions --------
  
  # for consistent colourings
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  
  # *************************
  # Read in data file ------
  
  
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  
  # read in data
  typo_data <- reactive(read.csv(input$file$datapath))

  
  
  # *************************************************
  # Define Categories  and associated weights -------
  
  nCategory <- reactive(input$nCategory)
  
  output$category <- renderUI({
    # i=nCategory()
    lapply(1:nCategory(), function(i){textInput(paste("cat",i,sep="_"), paste("Category", i), paste("Category",i,sep=""))})
  })
  
  output$catweights <- renderUI({
    lapply(1:nCategory(), function(i){numericInput(paste("catw",i,sep="_"), paste("Category Weight", i),1)})
  })
  
  catnames <- reactive({
      cnames <- input$cat_1
      if (nCategory() > 1){
        for(i in 2:nCategory()){
          cnames <- c(cnames, input[[paste("cat",i,sep="_")]])
        }
      }
      return(cnames)
    })
  
    mainWeights <- reactive({
      
      mw <- input$catw_1
      if (nCategory() > 1){
        for (i in 2:nCategory()){
          mw <- c(mw, input[[paste("catw",i,sep="_")]])
        }
      }
      return(mw)
    })
  

    # ***************************************************************
    # Define Variable Selection and full weighting scheme ----------
  
    # create initial Table
    initialTable <- reactive({
      
  
      varTable <- data.frame(Variable = names(typo_data()), Include = factor("Yes", levels=c("No","Yes")), Binary=factor("No", levels=c("No", "Yes")), Category=factor(input$cat_1, levels=catnames()))
      
      varTable$`Main Weights` <- NULL
      for (i in 1:nCategory()){
        varTable$`Main Weights`[varTable$Category == catnames()[i]] <- mainWeights()[i]
      }
      
      varTable$`Individual Weights` <- as.integer(1)
      
      varTable$`Main Weights`[varTable$Include == "No"] <- 0
      varTable$`Individual Weights`[varTable$Include == "No"] <- 0
      
      varTable$`Overall Weight` <- varTable$`Main Weights` * varTable$`Individual Weights`
      varTable$`Overall Weight (normalised)` <- varTable$`Overall Weight` / sum(varTable$`Overall Weight`)
      
      varTable$Dissimilarity = factor("Scaled Euclidean", levels=c("Scaled Euclidean", "Euclidean", "SMC", "Jaccard", "Bray-Curtis", "Bray-Curtis: Binary"))
      
      return(varTable)
    })
    
    
    output$vtable <- renderRHandsontable({
      
      if (is.null(input$vtable$data)){
        updatedTable <- initialTable()
        # print(1)
        
      } else if (length(unique(unlist(input$vtable$data))) == 1 ){
        
        updatedTable <- initialTable()
        # print(2)
      } else if (sum(abs(dim(hot_to_r(input$vtable)) - dim(initialTable()))) != 0){
        # initialised table
        updatedTable <- initialTable()
        # print(3)
      } else {
        curr_table <- as.data.frame(hot_to_r(input$vtable))
        
        # Update number of categories
        if(length(levels(curr_table$Category)) != nCategory()){
          curr_table$Category <- factor(curr_table$Category, levels=catnames())
        }
        
        # Update category names
        if (sum(levels(curr_table$Category) != catnames()) > 0){
          Category_new <- as.character(curr_table$Category)
          for (i in 1:nCategory()){
            Category_new[curr_table$Category == levels(curr_table$Category)[i]] <- catnames()[i]
          }
          curr_table$Category <- factor(Category_new, levels=catnames())
        }
        
        # Update weights if type is changed
        for (i in 1:nCategory()){
          curr_table$`Main Weights`[curr_table$Category == catnames()[i]] <- mainWeights()[i]
        }
        
        # Update weights if inclusion criteria is changed 
        curr_table$`Main Weights`[curr_table$Include == "No"] <- 0
        curr_table$`Individual Weights`[curr_table$Include == "No"] <- 0
        
        # Update weights
        for (i in 1:nCategory()){
          index <- curr_table$Category == catnames()[i]
          
          curr_table$`Overall Weight`[index] <- curr_table$`Main Weights`[index] * curr_table$`Individual Weights`[index] /sum(curr_table$`Individual Weights`[index])
        }
        
        curr_table$`Overall Weight (normalised)` <- curr_table$`Overall Weight` / sum(curr_table$`Overall Weight`)
        
        updatedTable <- curr_table
        # print(4)
      }
      
  
      outTable <- rhandsontable(updatedTable, rowHeaders=NULL) %>%
        hot_col("Variable", readOnly=TRUE) %>%
        hot_col("Main Weights", readOnly=TRUE) %>% 
        hot_col("Overall Weight", readOnly=TRUE) %>%
        hot_col("Overall Weight (normalised)", readOnly=TRUE) %>%
        hot_col("Overall Weight (normalised)",format="Numeric",renderer = "
                      function (instance, td, row, col, prop, value, cellProperties) {
                      Handsontable.renderers.NumericRenderer.apply(this, arguments);
                      if (value == 0) {
                      td.style.background = 'lightgrey';
                      } 
                      }") %>%
        hot_col("Overall Weight",format="Numeric",renderer = "
                      function (instance, td, row, col, prop, value, cellProperties) {
                      Handsontable.renderers.NumericRenderer.apply(this, arguments);
                      if (value == 0) {
                      td.style.background = 'lightgrey';
                      } 
                      }") %>%
        hot_col("Main Weights",format="Numeric",renderer = "
                      function (instance, td, row, col, prop, value, cellProperties) {
                      Handsontable.renderers.NumericRenderer.apply(this, arguments);
                      if (value == 0) {
                      td.style.background = 'lightgrey';
                      } 
                      }") %>%
        hot_col("Individual Weights",format="Numeric",renderer = "
                      function (instance, td, row, col, prop, value, cellProperties) {
                      Handsontable.renderers.NumericRenderer.apply(this, arguments);
                      if (value == 0) {
                      td.style.background = 'lightgrey';
                      } 
                      }") %>%
        hot_cols(columnSorting = TRUE, colWidths=c(200,70,70,100,70,70,70,100,150))
        # hot_cols(manualColumnResize = TRUE, colWidths=c(rep(60,5))) 
      
      return(outTable)
  
    })

    
    
    output$weights <- renderPlot({
      
      outTable <- as.data.frame(hot_to_r(input$vtable))
      # ensure order of plotting is same as order in the table
      outTable$Variable <- factor(outTable$Variable, levels=outTable$Variable)
      
      ggplot(outTable, aes(x=Variable, y=`Overall Weight (normalised)`, fill=Category)) + geom_col() + labs(x="") + theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), text=element_text(size=20)) + ggtitle("Variable Weights: all") 
      
    
    })
   
    output$weights2 <- renderPlot({
      
      outTable <- as.data.frame(hot_to_r(input$vtable))
       
      ggplot(outTable, aes(x=Category, y=`Overall Weight (normalised)`, fill=Variable)) + theme_bw() + geom_col(position="stack") + ggtitle("Variable weights: by Category") + theme(text = element_text(size=20))
      
    })
    
    
    
    # **************************************
    # Construct Dissimilarity Matrix -------
    
    
    # dissimilarity of a single variable
    # returns the squared dissimilarity multiplied by the weight
    calcDist <- function(x, dissType, dat, weights){
      if (dissType[x] == "Scaled Euclidean") {
        d = vegdist(dat[,x], method="gower")
      } else if (dissType[x] == "SMC"){
        d = dist.binary(as.data.frame(dat[,x]), method = 2)
      } else if (dissType[x] == "Jaccard"){
        d = dist.binary(as.data.frame(dat[,x]), method = 1)
      } else if (dissType[x] == "Euclidean"){
        d = vegdist(dat[,x], method="euclidean")
      } else if (dissType[x] == "Bray-Curtis"){
        d = vegdist(dat[,x], method="bray")
      } else if (dissType[x] == "Bray-Curtis: Binary"){
        d = vegdist(dat[,x], method="bray", binary=TRUE)
      }
      return(c(as.matrix(d))^2 * weights[x])
    }
  
    
    # Construct Distance matrix
    distMat <- reactive({
      outTable <- as.data.frame(hot_to_r(input$vtable))
      v_names <- outTable$Variable[outTable$Include == "Yes"]
      
      dat <- typo_data()[, v_names]
      
      weights <- outTable$`Overall Weight (normalised)`[outTable$Include == "Yes"]
      dissType <- outTable$Dissimilarity[outTable$Include == "Yes"]
      
      # for each variable, calculate the distance. NB, needs to be done individually, to allow for the weights to be added in
      allDists <- sapply(1:length(v_names), calcDist, dissType, dat, weights)
      
      distvec <- sqrt(rowSums(allDists))
      distmat <- matrix(distvec, nrow=dim(dat)[1], ncol=dim(dat)[1])
      
      # d is a matrix of dissimilarities
      distAll <- as.dist(distmat)
      return(distAll)
  
    })
  
    
    
    # **********************************
    # Dimension Reduction --------------
    
    

    runPCO <- reactive(input$pco)
    methodpco <- reactive(input$methodpco)
    scalepco <- reactive({input$scalepco})
    ndim <- reactive(input$ndim)

  
    # PCO (metric multidimensional scaling)
    getPCO <- reactive({
      if(runPCO() == 1 & methodpco() == "pco"){
        # NB, setting the number of dimensions doesn't change the ordination here - just the number of dimensions saved
        pco <- cmdscale(distMat(), eig=TRUE, k=min(20,dim(typo_data())[1]))
      } else {
        pco <- NULL
      }
      return(pco)
    })
    
    
    # Non-metric multidimensional scaling
    getNMS <- reactive({
      if (runPCO() == 1 & methodpco() == "nms"){
        nms <- NULL
        for (k in 1:max(ndim(),10)){
          nms[[k]] <- isoMDS(distMat(), k=k)
        }
      } else {
        nms <- NULL
      }
      return(nms)
    })
    
    
    # plot an assessment of the number of dimensions
    output$pcoDim <- renderPlot({

      if (methodpco() == "pco") {
        k = min(20,dim(typo_data())[1])
        # check if any negative eigenvalues
        if (min(getPCO()$eig) < -1e-7){
          print(min(getPCO()$eig))
          # negative eigenvalues
          plot(getPCO()$eig, xlab="Dimension", ylab="Eigenvalue")
          abline(h=0, col=2)
          text(y=mean(range(getPCO()$eig)), x=dim(typo_data())[1]/2, "Negative eigenvalues present: consider using Non-metric MDS")
        } else {
          plot(cumsum(getPCO()$eig[1:k]/ sum(getPCO()$eig[1:k])), ylab="Accummulated proportion of variation accounted for", xlab="Dimension", type="b")
          abline(v=ndim(), col=2)
          abline(h=sum(getPCO()$eig[1:ndim()])/ sum(getPCO()$eig[1:k]), col=2)
        }

      } else if (methodpco() == "nms"){
        stress <- rep(NA, ndim())
        for (k in 1:max(ndim(),10)){
          stress[k] <- getNMS()[[k]]$stress
        }
        plot(stress, ylab="Stress", xlab="Dimension", type="b")
        abline(v=ndim(), col=2)
        abline(h=stress[ndim()], col=2)
      } 

    })
    
    
    # Plot the projections of the reduced dimensional representation
    output$pcoOut <- renderPlot({
      
      cols = gg_color_hue(max(grpTypo()))
      
      if (runPCO() == 1){
        if (methodpco() == "pco") {
          if (ndim() > 1){
            pairs(getPCO()$points[,1:ndim()], col=cols[grpTypo()], labels=paste("Axis", 1:length(grpTypo())))
          } else{
            boxplot(getPCO()$points[,1] ~ grpTypo(), ylab="Axis 1", xlab="Typology")
          }
          
        } else if (methodpco() == "nms"){
          if (ndim() > 1){
            pairs(getNMS()[[ndim()]]$points[,1:ndim()], col=cols[grpTypo()], labels=paste("Axis", 1:length(grpTypo())))
          } else{
            boxplot(getNMS()[[ndim()]]$points[,1] ~ grpTypo(), ylab="Axis 1", xlab="Typology")
          }
          
        }
      } else {
          outTable <- as.data.frame(hot_to_r(input$vtable))
          v_names <- outTable$Variable[outTable$Include == "Yes" & outTable$Binary == "No"]
          dat <- typo_data()[, v_names]
          
          ncol <- ncol(dat)
          # plotting a maximum of 7 variables for visualisation (to be edited)
          pairs(dat[,1:min(ncol, 7)], col=cols[grpTypo()], main="Pairwise scatterplots of the first 7 variables")
        
      }
      
      
    })
    
    
    
    
    
    
    
    # **************************
    # Clustering --------------
  
    
    nclust <- reactive(input$nclust)
    typeclust <- reactive(input$typeclust)
    
    # hierarchical clustering
    methodclust <- reactive(input$methodclust)
  

    # input to hierarchical clustering should be dissimilarities of either the reduced dimensional data or the original
    diss_for_clust <- reactive({
     
      if(runPCO() == 1){
        if (methodpco() == "pco") {
            if (scalepco() == 1){
              d <- dist(scale(getPCO()$points[,1:ndim()]))
            } else {
              d <- dist(getPCO()$points[,1:ndim()])
            }
        } else if (methodpco() == "nms"){
          if (scalepco() == 1){
            d <- dist(scale(getNMS()[[ndim()]]$points[,1:ndim()]))
          } else {
            d <- dist(getNMS()[[ndim()]]$points[,1:ndim()])
          }
        } 
  
      } else {
        d <- distMat()
      }
      
      return(d)
      
    })
    
    # run the hierarchical clustering
    hcl <- reactive({
      if (typeclust() == "hier"){
        hcl <- hclust(diss_for_clust(), method=methodclust())
      } else {
        hcl <- NULL
      }
      return(hcl)
    })
    
    
    # k-means clustering
    kscale <- reactive(input$kscale)
    
    # run the kmeans clustering
    kmean <- reactive({
      if (typeclust() == "kmeans"){
        if(runPCO() == 1){
          if (methodpco() == "pco") {
            if (scalepco() == 1){
              dat <- (scale(getPCO()$points[,1:ndim()]))
            } else {
              dat <- (getPCO()$points[,1:ndim()])
            }
          } else if (methodpco() == "nms"){
            if (scalepco() == 1){
              dat <- (scale(getNMS()[[ndim()]]$points[,1:ndim()]))
            } else {
              dat <- (getNMS()[[ndim()]]$points[,1:ndim()])
            }
          } 
          
        } else {
          outTable <- as.data.frame(hot_to_r(input$vtable))
          v_names <- outTable$Variable[outTable$Include == "Yes"]
          dat <- typo_data()[, v_names]
        }
        
        if (kscale() == "range"){
          ranges <- apply(dat, 2, range, na.rm=TRUE)
          rangediff <- apply(ranges, 2, diff)
          dat <- scale(dat, scale = rangediff)
        } else if (kscale() == "scale"){
          dat <- scale(dat)
        } 
        
        kout <- NULL
        for (cl in 1:max(nclust(), 15)){
          kout[[cl]] <- kmeans(dat, cl)
        }

      } else {
        kout <- NULL
      }
      
      return(kout)
      
    })
    
    
    # extract the distinct clusters to define the typology groups
    grpTypo <- reactive({
        if (typeclust() == "hier"){
          hdend <- as.dendrogram(hcl())
          grp <- cutree(hdend, k=nclust())
        } else {
          grp <- kmean()[[nclust()]]$cluster
        }
      })
  

  
    # Plot the clusters
    output$clusterOut <- renderPlot({
      if (typeclust() == "hier"){
      
        hdend <- as.dendrogram(hcl())
        hdend <- color_branches(hdend, k=nclust(), col=gg_color_hue(max(grpTypo())))
        hdend <- color_labels(hdend, k=nclust(), col=gg_color_hue(max(grpTypo())))
        plot(hdend)
        
      } else {
        
        # plot the entropy of the kmeans clustering
        between.ss <- NULL
        tot.withinss <- NULL
        totss <- NULL
        for (cl in 1:max(nclust(), 15)){
          between.ss[cl] <- kmean()[[cl]]$betweenss
          tot.withinss[cl] <- kmean()[[cl]]$tot.withinss
          totss[cl] <- kmean()[[cl]]$totss 
        }
        par(mfrow=c(1, 2))
        plot(between.ss, type="b", ylab="Between Group Sums of Squares", xlab="Number of clusters")
        abline(h=between.ss[nclust()], col=2)
        abline(v=nclust(), col=2)
        plot(tot.withinss, type="b", ylab="Within Group Sums of Squares", xlab="Number of clusters")
        abline(h=tot.withinss[nclust()], col=2)
        abline(v=nclust(), col=2)
      }
    })
  
    # Summarise the dissimilarity between groups using analysis of similarity
    output$anod <- renderPrint({
      
      anod <- adonis2(diss_for_clust() ~ factor(grpTypo()))
      print(anod)
      
    })
    
    

  
    # ******************
    # Validation -------
  
    range01 = function(x){abs(x)/max(abs(x))}
    
    # plot a heatmap of the mean (continuous) and proportion (binary) in each cluster
    output$heatmap <- renderPlot({
      outTable <- as.data.frame(hot_to_r(input$vtable))
      v_names <- outTable$Variable[outTable$Include == "Yes"]
      
      if (length(v_names) > 0){
        dat <- typo_data()[, v_names]
        dat$Typology <- factor(grpTypo())
        
        # summarise by mean and divide by max
        dat_mean <- dat %>% group_by(Typology) %>% summarise_at(v_names, mean) 
        dat_meanscale <- dat_mean %>% summarise_at(v_names, range01)
        dat_meanscale$Typology <- dat_mean$Typology
        dat_meanscale = gather(dat_meanscale, Metric, Value, all_of(v_names))
        
        ggplot(dat_meanscale, aes(Metric, Typology)) +
          geom_tile(aes(fill = Value), colour = "black", size = 0.35) +
          scale_fill_viridis(limits = c(0, 1), option = "B", breaks = c(0, 0.5, 1), labels = c("low", "medium", "high")) +
          labs(fill = "Score") + theme_classic() +
          xlab("") + ylab("") + 
          theme(axis.text.x = element_text(color = "black", size = 12, angle = 45 , vjust = 1, hjust = 1),
                axis.text.y = element_text(color = "black", size = 14),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_blank(),
                panel.background = element_blank(),
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.background = element_blank(),
                legend.key.size = unit(1.2, "cm"),
                legend.title = element_blank(),
                text = element_text(size = 14)) 

      }
      
      
    })
    
    
    # plot each variable by the cluster
    output$validation1 <- renderPlot({
      outTable <- as.data.frame(hot_to_r(input$vtable))
      v_names <- outTable$Variable[outTable$Include == "Yes" & outTable$Binary == "No"]

      if (length(v_names) > 0){
        dat <- typo_data()[, v_names]
      dat$Typology <- factor(grpTypo())
      
      # stack the data
      typo_datastack <- reshape(dat, direction="long", varying = list(1:(ncol(dat)-1)), timevar="Variable", times=names(dat)[1:(ncol(dat)-1)], v.names="y")
  

      ggplot(typo_datastack, aes(x=Typology, y=y, fill=Typology))   + facet_wrap(~Variable, scales="free_y", ncol=4) + 
        geom_point(shape = 21, size = 3, position = position_jitter(width = 0.2), color = "black", fill = "grey")+ 
        geom_boxplot(size = 0.9, width = 0.4, alpha = 0.5) + 
        theme(axis.text = element_text( face = "bold"), axis.title = element_text( face  = "bold"), text = element_text(size = 14))

      }
      
    })
    
    # same for the binary variables
    output$validation2 <- renderPlot({
      outTable <- as.data.frame(hot_to_r(input$vtable))
      v_names <- outTable$Variable[outTable$Include == "Yes" & outTable$Binary == "Yes"]
      
      if (length(v_names) > 0){
        dat <- typo_data()[, v_names]
        
        dat$Typology <- factor(grpTypo())
        
        # stack the data
        typo_datastack <- reshape(dat, direction="long", varying = list(1:(ncol(dat)-1)), timevar="Variable", times=names(dat)[1:(ncol(dat)-1)], v.names="y")
        
        ggplot(typo_datastack, aes(x=Typology, y=y, colour=Typology))   + facet_wrap(~Variable, scales="free_y", ncol=4) + 
          geom_point(shape = 21, size = 3, position = position_jitter(width = 0.2, height=0.2))+ 
          theme(axis.text = element_text(face = "bold"), axis.title = element_text(face  = "bold"),text = element_text(size = 14))
        
      }
      
    })
    
    
    
    
    # Plot by cluster: spider plot of continuous variables
    output$validation3 <- renderPlot({
      
      outTable <- as.data.frame(hot_to_r(input$vtable))
      v_names <- outTable$Variable[outTable$Include == "Yes" & outTable$Binary == "No"]
      if (length(v_names) > 0){
        dat <- typo_data()[, v_names]
     
        # transform all data to its rank
        dat_rank <- data.frame(apply(dat, 2, rank)) / nrow(dat)
        
        dat_rank$Typology <- factor(grpTypo())
        
        # summarise ranks by each typology
        dat_radar <- dat_rank %>% group_by(Typology) %>% summarise_at(v_names, quantile)
        dat_radar <- data.frame(Quantile=rep(seq(0, 1, 0.25), times = max(grpTypo())), dat_radar)
        
        g <- NULL
        for (k in 1:max(grpTypo())){
          g[[k]] <- ggradar(dat_radar %>% filter(Typology == k & Quantile %in% c(0.25,0.5,0.75)) %>% select(-"Typology"), plot.title=paste("Cluster", k), legend.title="Quartile",legend.text.size = 12, base.size=12,group.line.width = 1,group.point.size = 2)
        }
        
        ggarrange(plotlist = g, common.legend=TRUE)
      }
    })
    
    # and of the binary variables
    output$validation4 <- renderPlot({
      
      outTable <- as.data.frame(hot_to_r(input$vtable))
      v_names <- outTable$Variable[outTable$Include == "Yes" & outTable$Binary == "Yes"]
      if(length(v_names) > 0){
        dat <- typo_data()[, v_names]
        
        dat_rank <- dat / nrow(dat)
        dat_rank$Typology <- factor(grpTypo())
        
        # summarise proportion of 1's in each typology
        dat_radar <- dat_rank %>% group_by(Typology) %>% summarise_at(v_names, sum)
        dat_radar <- data.frame(Proportion="Proportion", dat_radar)
        # gmax <- (1/max(grpTypo())) * 2.1
        gmax <- max(dat_radar[, v_names])
        gmid <- gmax / 2
        
        g <- NULL
        for (k in 1:max(grpTypo())){
          g[[k]] <- ggradar(dat_radar %>% filter(Typology == k) %>% select(-"Typology"), plot.title=paste("Cluster", k), legend.title="Proportion", legend.text.size = 12, base.size=12,group.line.width = 1,group.point.size = 2, grid.max=gmax, grid.mid=gmid, values=c("0%", paste(round(gmid * 100), "%", sep=""), paste(round(gmax * 100), "%", sep="")))
        }
        
        ggarrange(plotlist = g, common.legend=TRUE)
      }

    })
    
    
    
    # plot the reduced dimensional axes related to the original continuous variables
    output$pcorelate1 <- renderPlot({
      
      if(runPCO() == 1){
        if (methodpco() == "pco") {
          if (scalepco() == 1){
            dfout<- scale(getPCO()$points[,1:ndim()])
          } else {
            dfout <- getPCO()$points[,1:ndim()]
          }
        } else if (methodpco() == "nms"){
          if (scalepco() == 1){
            dfout <- scale(getNMS()[[ndim()]]$points[,1:ndim()])
          } else {
            dfout <- getNMS()[[ndim()]]$points[,1:ndim()]
          }
        } 
       
        # for each retained dimension, plot against the input variables
        outTable <- as.data.frame(hot_to_r(input$vtable))
        v_names <- outTable$Variable[outTable$Include == "Yes" & outTable$Binary == "No"]
        if(length(v_names) > 0){
          dat <- typo_data()[, v_names]
          dat$Typology <- factor(grpTypo())
          # stack the data
          typo_datastack <- reshape(dat, direction="long", varying = list(1:(ncol(dat)-1)), timevar="Variable", times=names(dat)[1:(ncol(dat)-1)], v.names="y")
          
          # repeat the pc data
          dfout_stack <- dfout
          if (length(v_names) > 1){
            for (i in 2:length(v_names)){
              dfout_stack <- rbind(dfout_stack, dfout)
            }
          }
  
          g <- NULL
          for (j in 1:ndim()){
            typo_datastack_j <- data.frame(pc=dfout_stack[,j], typo_datastack)
            g[[j]] <- ggplot(typo_datastack_j, aes(x=y, y=pc, colour=Typology))   + facet_wrap(~Variable, scales="free", ncol=4) + 
            geom_point(shape = 21, size = 3)+ 
            theme(axis.text = element_text(face = "bold"), axis.title = element_text(face  = "bold"),text = element_text(size = 14)) +
              ggtitle(paste("Relating dimension reduction of axis",j)) + labs(y=paste("Axis", j), x="")
          }
          
          ggarrange(plotlist = g, common.legend=TRUE, ncol=1, nrow=length(g))
        } 
      
      }
      
    })
    
 
    # plot the reduced dimensional axes related to the original binary variables
    output$pcorelate2 <- renderPlot({
      
      if(runPCO() == 1){
        if (methodpco() == "pco") {
          if (scalepco() == 1){
            dfout<- scale(getPCO()$points[,1:ndim()])
          } else {
            dfout <- getPCO()$points[,1:ndim()]
          }
        } else if (methodpco() == "nms"){
          if (scalepco() == 1){
            dfout <- scale(getNMS()[[ndim()]]$points[,1:ndim()])
          } else {
            dfout <- getNMS()[[ndim()]]$points[,1:ndim()]
          }
        } 
        
        # for each retained dimension, plot against the input variables
        outTable <- as.data.frame(hot_to_r(input$vtable))
        v_names <- outTable$Variable[outTable$Include == "Yes" & outTable$Binary == "Yes"]
        if(length(v_names) > 0){
          dat <- typo_data()[, v_names]
          dat$Typology <- factor(grpTypo())
          # stack the data
          typo_datastack <- reshape(dat, direction="long", varying = list(1:(ncol(dat)-1)), timevar="Variable", times=names(dat)[1:(ncol(dat)-1)], v.names="y")
          
          # repeat the pc data
          dfout_stack <- dfout
          if (length(v_names) > 1){
            for (i in 2:length(v_names)){
              dfout_stack <- rbind(dfout_stack, dfout)
            }
          }
          
          g <- NULL
          for (j in 1:ndim()){
            typo_datastack_j <- data.frame(pc=dfout_stack[,j], typo_datastack)
            g[[j]] <- ggplot(typo_datastack_j, aes(x=y, y=pc, colour=Typology, group=factor(y)))   + facet_wrap(~Variable, scales="free", ncol=4) + 
              geom_point(shape = 21, size = 3, position = position_jitter(width = 0.2, height=0.2))+ 
              geom_boxplot(size = 0.9, width = 0.4, alpha = 0.68, colour="grey") + 
              theme(axis.text = element_text(face = "bold"), axis.title = element_text(face  = "bold"),text = element_text(size = 14)) +
              ggtitle(paste("Relating dimension reduction of axis",j)) + labs(y=paste("Axis", j), x="")
          }
          
          ggarrange(plotlist = g, common.legend=TRUE, ncol=1, nrow=length(g))
        } 
      
      }
      
    })

    # *******************
    # save grouping -----
    output$savegrp <- downloadHandler(
      filename = function() {
        paste("typo_data", "_withTypology.csv", sep = "")
      },
      content = function(file) {
        typo_data()$Typology <- factor(grpTypo())
        write.csv(typo_data(), file, row.names = FALSE)
      }
    )

})
