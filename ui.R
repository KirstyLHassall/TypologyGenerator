# ********************************************
#
# The user interface for the R-Shiny App
# TypologyGenerator
#
# ********************************************

library(shiny)
library(shinyjs)
library(shinythemes)
library(rhandsontable)
library(shinyWidgets)


shinyUI(fluidPage(

    # Application title
    navbarPage("Typology Generator", theme = shinytheme("flatly"),
    
        tabPanel("Construction",       
                 
                 # Input: Select a file ----
                 fileInput("file", "Choose CSV File",
                           multiple = FALSE,
                           accept = c(".csv")),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
           
            conditionalPanel("output.fileUploaded", 
                      
                         h3("Variable Categories"),
                         
                         numericInput("nCategory", "Number of Categories:", 3, min = 1, max = 10),
                         
                         fluidRow(
                             column(3, uiOutput("category"), offset = 1),
                             column(3, uiOutput("catweights"), offset = 1)
                         ),
                         
                         h3("Variable Selection"),
                         rHandsontableOutput('vtable'),
                         h3(""),
                         switchInput(inputId = "weightplot", value = FALSE, onLabel="Show Plot", offLabel = "Hide Plot", size="small"),
                         conditionalPanel('input.weightplot', 
                                          plotOutput("weights"),
                                          plotOutput("weights2", width="60%")
                                          ),
                         
                         h3("Dimension Reduction"),
                         selectInput("pco", label="Reduce dimensionality?", choices=list("Yes" = 1, "No" = 0), selected = 0),
                         
                         conditionalPanel("input.pco == 1",
                                          column(3, selectInput("methodpco", "Method of dimension reduction", list("Metric MDS"="pco", "Non-metric MDS"="nms"), selected="pco")),
                                          column(3, selectInput("scalepco", label="Scale orthogonalised variables?", choices=list("Yes" = 1, "No" = 0), selected = 0)),
                                          column(3,numericInput("ndim", "Number of dimensions", 2)),
                                          fluidRow(column(9, plotOutput("pcoDim")))
                         ),
                         
                         fluidRow(
                             column(12, plotOutput("pcoOut"))
                             ),
                         
                         tags$hr(),
                         
                         h3("Cluster Analysis"),
                         
                         column(3, selectInput("typeclust", label="Cluster Method", choices=list("Hierarchical" = "hier", "k-means"="kmeans"), selected="hier")),
                         
                         conditionalPanel("input.typeclust == 'hier'", 
                         column(3, selectInput("methodclust", label="Linkage method", choices=list("Complete"="complete", "Ward"="ward.D2", "Average (UPGMA)" = "average", "Single"="single"), selected="complete")),
                         ),
                         
                         conditionalPanel("input.typeclust == 'kmeans'", 
                                          column(3, selectInput("kscale", label="Scale variables", choices=list("No"="no", "Range"="range", "Scale" = "scale"), selected="no")),
                         ),
                         
                         fluidRow(
                             column(3, numericInput("nclust","Number of Clusters", 4, min=1, max=100)),
                             column(9, plotOutput("clusterOut"))
                         ),
                         
                         
                         fluidRow(
                             column(9, verbatimTextOutput("anod"))
                         ),

                         
                         downloadButton('savegrp', 'Save Typology'),
                         h5(""),
                         
                         fluidRow(column(12, h3(" ")))
                         
        ),
     ),
            
     
     
        tabPanel("Validation",
            wellPanel(
                tabsetPanel(
                    tabPanel(h3("Cluster Constituents"),
                             plotOutput("heatmap"),
                             
                             h4("Continuous Variables"),
                             plotOutput("validation1", height="800px"),
                             h4("Binary Variables"),
                             plotOutput("validation2", height="800px")
                    ),
                    tabPanel(h3("Cluster Shape"),
                             h4("Continuous Variables"),
                             plotOutput("validation3", height="800px"),
                             h4("Binary Variables"),
                             plotOutput("validation4", height="800px")
                    ),
                    tabPanel(h3("Axis Relate"),
                             conditionalPanel("input.pco==1",
                                              h4("Continuous Variables"),
                                              plotOutput("pcorelate1", height="1600px"),
                                              h4("Binary Variables"),
                                              plotOutput("pcorelate2", height="1600px")
                             )
                    )
                )
            ),

        ),
     
     
     
     
        tabPanel("Help",
                 h3("Typologies"),
                 p("Smallholder farming systems make up a highly complex landscape consisting of many sources of variability. By allocating farms to a particular typology we can capture this variability in a simplified categorisation"),
                 p("This app provides a tool for constructing farm typologies, enabling the user to fine-tune each part of the process according to their needs."),
                 
                 h3("Data"),
                 helpText("Data can be loaded into the app from a standard CSV format."),
                 p("It is assumed that the data have been appropriately processed (transformed, amalgamated categories etc) as described in Hassall et al."),
                 
                 h3("Variable Categories"),
                 helpText("How many different categories of variables do you have in the dataset?"),
                 p("It is common to have categories of variables that explain similar characteristics of smaller-holder farms. Examples may include structural characteristics, functional, nutritional or resource endownment."),
                 helpText("Should these categories have equal weight?"),
                 p("If yes, then each variable is rescaled to ensure the overall contribution of each category is the same regardless of how many variables make up each category."),
                 p("If no, define the relative weight of each category. For instance, does your study want to focus on a particular aspect, in which case you may want to increase the weight for this type of variable."),
                 
                 
                 
                 h3("Variable Selection"),
                 helpText("The interactive table enables users to refine the data selection and dissimilarity construction."),
                 helpText("NOTE: the interactive table can be a little slow to respond. If too many edits are made in quick succession, the app can crash."),
                 tags$ul(
                   tags$li("Each variable in the uploaded data can either be included or excluded in defining the allocation to distinct typologies. To deselect a variable, select 'No' in the column 'Include'."), 
                   tags$li("Each variable in the uploaded data can either be continuous or binary. Selection here, will impact the resulting graphics produced in the validation tab."), 
                   tags$li("Each variable can be assigned to a different category as defined in the previous step. This is done using the dropdown options in the 'Category' column. By changing the categories, the subsequent 'Main Weights' column will be updated."), 
                   tags$li("Within each category of variables, all variables are given equal weight. This can be amended to reflect a belief that one or more variables should have greater relative influence in the column 'Individual Weights'."),
                   tags$li("The column 'Overall Weight' is updated automatically to show the relative weights between all variables in the data. The associated 'normalised' column is the same with the added constraint that the sum of all weights is equal to 1.")
                 ),
                 helpText("The overall weights are visually represented. Click the switch button to show/hide the figures. NB, they can take a little time to reload when changes are made to the table."),

                 
                 
                 h3("Define Dissimilarity"),
                 helpText("Select the appropriate dissimilarity measure for each variable"),
                 p("The following dissimilarity measures are available for quantitative variables;"),  
                 column(8,p("Scaled Euclidean: calculated using",code("vegan::vegdist       method='gower'"))),
                 column(4,switchInput(inputId = "seuc", value = FALSE, onLabel="Show", offLabel = "Hide", size="small", inline=TRUE)),
                 conditionalPanel('input.seuc',
                                  p("d[jk] = (1/M) abs(x[j]-x[k])/(max(x)-min(x)), for variable x and observations j and k.")
                 ),
                 column(8,p("Euclidean: calculated using",code("vegan::vegdist       method='euclidean'"))),
                 column(4,switchInput(inputId = "euc", value = FALSE, onLabel="Show", offLabel = "Hide", size="small", width="auto")),
                 conditionalPanel('input.euc',
                                  p("d[jk] = sqrt(x[j]-x[k])^2), for variable x and observations j and k.")
                 ),
                 column(8,p("Bray-Curtis: calculated using",code("vegan::vegdist       method='bray'"))),
                 column(4,switchInput(inputId = "bray", value = FALSE, onLabel="Show", offLabel = "Hide", size="small")),
                 conditionalPanel('input.bray',
                                  p("d[jk] = abs(x[j]-x[k])/(x[j]+x[k]), for variable x and observations j and k.")
                 ),
                 p(""),
                 p("And for binary/qualitative variables;"),
                 column(8,p("Simple Matching Coefficient: calculated using",code("ade4::dist.binary       method=2"))),
                 column(4,switchInput(inputId = "smc", value = FALSE, onLabel="Show", offLabel = "Hide", size="small")),
                 conditionalPanel('input.smc',
                                  p("d[jk] = 1, if x[j]=x[k],"),
                                  p("d[jk] = 0, otherwise.")
                 ),
                 column(8,p("Jaccard: calculated using",code("ade4::dist.binary       method=1"))),
                 column(4,switchInput(inputId = "jacc", value = FALSE, onLabel="Show", offLabel = "Hide", size="small")),
                 conditionalPanel('input.jacc',
                                  p("d[jk] = 1, if x[j]=x[k] = 1,"),
                                  p("d[jk] = 0, otherwise.")
                 ),
                 column(8,p("Bray-Curtis: calculated using",code("vegan::vegdist       method='bray', binary=TRUE"))),
                column(4,switchInput(inputId = "braybin", value = FALSE, onLabel="Show", offLabel = "Hide", size="small")),
                 conditionalPanel('input.braybin',
                                  p("d[jk] = 1, if x[j]=1 or x[k] = 1, but x[j]!=x[k],"),
                                  p("d[jk] = 0, otherwise.")
                 ),
                
                p(""),
                p("The overall dissimilarity is calculated as,"),
                p("D[jk] = sqrt(sum(w[i] x d[i][jk]^2)), where w[i] is the weight and d[i] is the dissimilarity measure associated with variable i"),
                
                 
                 
                 
                 
                h3("Dimension Reduction"),
                helpText("Should the dimensionality of the data be reduced before running the cluster algorithms?"),
                helpText("If yes, two methods of dimension reduction are available;"),
                column(8,p("Metric Multidimensional Scaling (aka, principal coordinates analysis)",code("stats::cmdscale"))),
                column(4,switchInput(inputId = "pcodetail", value = FALSE, onLabel="Show", offLabel = "Hide", size="small")),
                column(12,
                       conditionalPanel('input.pcodetail',
                                 p("Multidimensional scaling takes a set of dissimilarities and returns a set of points such that the distances between the points are approximately equal to the dissimilarities."),
                                 p("A set of Euclidean distances on n points can be represented exactly in at most n - 1 dimensions. cmdscale follows the analysis of Mardia (1978), and returns the best-fitting k-dimensional representation, where k may be less than the argument k."),
                                 p("Metric MDS assumes the input dissimilarities are metric. If a non-metric dissimilarity has been used, consider using the non-metric MDS option. Similarly, when missing values are present, a metric multi-dimensional scaling may produce negative eigenvalues. Where these eignevalues are large, a non-metric MDS may be more appropriate."),
                                 p("A plot of the accumulated proportion of variance accounted for by each dimension is given to aid the user to define the appropriate number of dimensions to retain in the subsequent cluster analysis. This a subjective choice and should be investigated in the resulting clusters."),
                                 p("Should the MDS axes be scaled to have equal importance in the subsequent cluster analysis. If yes, each axis is divided by the standard deviation. This can be useful if, for instance the first axis represents a certain characteristic of the data but accounts for a large proportion of variance (e.g. if many variables are highly correlated) and the second axis represents a different feature but only accounts for a small amount of variation (potentially due to the fact only a small number of variables reflect this characteristic). By scaling these, each axis has equal influence on the resulting cluster analysis."),
                                 p("Note, principal components analysis is a special case of metric MDS, where (non-scaled) Euclidean distance is used as the dissimilarity measure.")
                )),
                
                column(8,p("Non-metric Multidimensional Scaling",code("MASS::isoMDS"))),
                column(4,switchInput(inputId = "nmsdetail", value = FALSE, onLabel="Show", offLabel = "Hide", size="small")),
                fluidRow(
                  column(12,conditionalPanel('input.nmsdetail',
                                 p("Kruskal's Non-metric Multidimensional Scaling is one example of non-metric MDS."),
                                 p("This chooses a k-dimensional configuration to minimize the stress, the square root of the ratio of the sum of squared differences between the input distances and those of the configuration to the sum of configuration distances squared."),
                                 p("An iterative algorithm is used, which will usually converge in around 10 iterations. It can be slow for large datasets. Further, since for the resulting configuration is only determined up to rotations and reflections, the result can vary considerably from machine to machine."),
                                 p("A plot of the MDS stress for by each dimension is given to aid the user to define the appropriate number of dimensions to retain in the subsequent cluster analysis. This a subjective choice and should be investigated in the resulting clusters."),
                                 p("Should the MDS axes be scaled to have equal importance in the subsequent cluster analysis. If yes, each axis is divided by the standard deviation. This can be useful if, for instance the first axis represents a certain characteristic of the data but accounts for a large proportion of variance (e.g. if many variables are highly correlated) and the second axis represents a different feature but only accounts for a small amount of variation (potentially due to the fact only a small number of variables reflect this characteristic). By scaling these, each axis has equal influence on the resulting cluster analysis.")
                ))),
                
                
                 
                
                h3("Cluster Analysis"),
                helpText("Cluster analysis is used to allocate farms to distinct typologies."),
                p("Two cluster methods are available;"),
                column(8,p("Hierarchical Clustering: calculated using",code("stats::hclust"))),
                column(4,switchInput(inputId = "hierdetail", value = FALSE, onLabel="Show", offLabel = "Hide", size="small")),
                conditionalPanel('input.hierdetail',
                                 p("This function performs a hierarchical cluster analysis using a set of dissimilarities for the n objects being clustered. Initially, each object is assigned to its own cluster and then the algorithm proceeds iteratively, at each stage joining the two most similar clusters, continuing until there is just a single cluster."),
                                 p("A number of different clustering methods are provided. Ward's minimum variance method aims at finding compact, spherical clusters. The complete linkage method finds similar clusters. The single linkage method (which is closely related to the minimal spanning tree) adopts a 'friends of friends' clustering strategy. The average method can be regarded as aiming for clusters with characteristics somewhere between the single and complete link methods."),
                                 p("Two different algorithms are found in the literature for Ward clustering. The one used here is 'ward.D2' and implements Ward's (1963) clustering criterion.")),
                
                column(8,p("K-means Clustering: calculated using",code("stats::kmeans"))),
                column(4,switchInput(inputId = "kdetail", value = FALSE, onLabel="Show", offLabel = "Hide", size="small")),
                conditionalPanel('input.kdetail',
                                 p("The data given (either the original input data or the reduced dimensional projection) are clustered by the k-means method, which aims to partition the observations into k groups such that the sum of squares from points to the assigned cluster centres is minimized."),
                                 p("The algorithm of Hartigan and Wong (1979) is used by default. Note that some authors use k-means to refer to a specific algorithm rather than the general method: most commonly the algorithm given by MacQueen (1967) but sometimes that given by Lloyd (1957) and Forgy (1965). The Hartigan-Wong algorithm generally does a better job than either of those."),
                                 p("If k-means clustering is implemented on the original input data, it is",em('strongly'),"advised to scale the input variables. This can either be done by dividing each column of data by the range or by the standard deviation. Both options are available."),
                                 p("If k-means clustering is implemented on the reduced dimensional projection, scaling is not necessary.")
                ),
                
                helpText("How many typologies should the data be grouped into?"),
                p("When implementing a hierarchical clustering, distinct groups are obtained by 'cutting' the resulting dendrogram into a defined number of groups. This is a subjective choice and is usually done by eye."),
                p("For k-means clustering, the number of groups is defined in advance of the clustering. This is again a subjective choice and different values can be inputted to investigate how the allocations differ. The aim of k-means clustering is to minimise the within group sums of squares (equivalently to maximise the between group sums of squares). This criterion is plotted to help the user select an appropriate number of clusters, ideally identifying an 'elbow' in the criterion as the number of clusters change."),
                
                
                h3("Validation"),
                p("Typology validation steps will be context specific. The associated pages provide a few key illustrations to i) summarise what the clusters represent in terms of the original variables and ii) to investigate how well each variable is represented in the dimension reduction step."),
                p("Prior to the visual assessment, analysis of the resulting typologies is done through a permutation MANOVA approach as implemented in", code("vegan::adonis2"), ". This enables a user to determine whether or not the resulting typologies define statistically different groups."),
                p("For the visual assessment, firstly, each variable is plotted by cluster group enabling a user to identify which variables are more separated than others."),
                p("Secondly, an overall spider diagram is given for each cluster allowing an assessment of the overall cluster 'shape'. The lines in the spider diagram are the quartiles calculated for each cluster of the ranks of each variable. Explicitly, let", em('z'), "be the ranks of variable", em('x')," and ", em('z[i]'), ", the subset of ", em('z'), " allocated to cluster i. Then for each cluster i, calculate the quartiles of", em('z[i]'), ". For binary variables, the line gives the overall percentage of 1's in each cluster group. These sum to 100 across the cluster groups."),
                p("Thirdly, if a dimension reduction technique has been used, the third tab contains scatter plots of the retained axes against each of the input variables. This allows a user to see how the reduced ordination relates to the original variables."),
               
                h3("Saving Typologies"),
                helpText("Resulting typology groupings can be saved in a standard CSV format."),
                
                
                h5(""),
                fluidRow(column(12, h3(" ")))
                
                ),
     
     
     tabPanel("Workflow", 
              h3("Workflow decision tree"),
              img(src='workflow.png', align = "left", height="60%", width="60%"),
              )
)))
