graphFactor <-
function(dat = dat, datName = NULL,
  howmanyvariables = dim(dat)[2L], 
   howmanyclusters = 10,
    buildSVD = FALSE,
   catDirectoryPrefixName = 'cat.expressions.dir.',
   catDirectorySuffixName = 1.1,
    minimumEventThreshold = 2, 
   seed = 20150523,
    ubFlag = FALSE,
    maximumEventThreshold = 10,
   YourNameHere = 'Your Name Here', YourLocationHere = 'Your Location Here',
     plotToFile = FALSE, showSequence = FALSE, seqDex = NULL){

 if(buildSVD == TRUE){
  svd.dat <- svd(dat)

 }



   # Create a folder to hold the edgelist and cluster assignments.

   cat.destination.1.1 <- paste(catDirectoryPrefixName, Sys.Date(), '.', catDirectorySuffixName, sep = '')

   # The destination folder is given a new name if the suffix already exists on the path.
   while(file.exists(cat.destination.1.1) == TRUE){ 
    catDirectorySuffixName <- catDirectorySuffixName + .1;
    cat.destination.1.1 <- paste(catDirectoryPrefixName, Sys.Date(), '.', catDirectorySuffixName, sep = '')
   }


   dir.create(cat.destination.1.1)



# K-means Clustering of Variable Space
kmeans.models.1.1 <- list()

 if(buildSVD == TRUE){
 for (i in 1:howmanyvariables){
  set.seed(seed)
 kmeans.models.1.1[[i]] <- 
kmeans(svd.dat$u[,i], centers = howmanyclusters)
 } # End 'for (i in 1:howmanyvariables) ...'
} # End 'if(buildSVD == TRUE){...'

 if(buildSVD == FALSE){
 for (i in 1:howmanyvariables){
  set.seed(seed)
 kmeans.models.1.1[[i]] <- 
 # Notice that when 'buildSVD == FALSE' that 
 # the SVD list component u isn't referenced!
kmeans(dat[,i], centers = howmanyclusters)
 } # End 'for (i in 1:howmanyvariables) ...'
} # End 'if(buildSVD == FALSE){...'



 ## This code block uses the elements of a character array to name variables.
 ## The variables to be named are saved to a character vector called 'list.names'.
 ## Each desired variable name, to be iterated by a 'for' loop, is concatenated
 ## with a Boolean test result. The result will be placed into a text file, then read in using 'source'.
 ## The sourced 'cat' object will be interpreted as an R expression and the newly assigned 
 ## variables placed in the desired R 'environment'. - Matthew Bascom

 list.names <- letters[1:howmanyclusters]
 cat.destinationAndPath.1.1 <- 
 paste(cat.destination.1.1, '/','kmeans.cluster.subset.expressions.', Sys.Date(), '.R', sep = '')

 PC.clusters <- list()

for (kmeanModelIteratoR in 1:howmanyvariables){

cat('PC.clusters[[', kmeanModelIteratoR, ']] <- list(', sep = '', file = cat.destinationAndPath.1.1, append = TRUE)
# Iterate the list population after the first 'cat' call.

for (clusterIteratoR in 1:howmanyclusters){

if(clusterIteratoR < howmanyclusters) cat(list.names[clusterIteratoR], ' = c(', 
paste(which(kmeans.models.1.1[[kmeanModelIteratoR]]$cluster == clusterIteratoR), 

collapse = ','), # end 'paste' expression.

'),\n', # end combine 'c' expression.

sep = '', 
file = cat.destinationAndPath.1.1, 
# end conditional 'if' 'cat' expression.
append = TRUE)else cat(list.names[clusterIteratoR], ' = c(', # ELSE ELSE ELSE ELSE ELSE ELSE #

paste(which(kmeans.models.1.1[[kmeanModelIteratoR]]$cluster == clusterIteratoR), 

collapse = ','), # end 'paste' expression.

')\n', # end combine 'c' expression.
# ELSE LINE OMITS A COMMA AFTER ')'.


sep = '', 
file = cat.destinationAndPath.1.1, 
# end conditional 'if' 'cat' alternate expression.
append = TRUE)

}# end 'if' conditioned clusterIteratoR for loop. 

cat(')\n', sep = ',', file = cat.destinationAndPath.1.1, append = TRUE)
# close the list definition with the third 'cat' call.

}



# Very important that this be called:
source(cat.destinationAndPath.1.1, local = TRUE)


# Create an naming array for the naming parameter 'dimnames' of the matrix.
edge.assignment.matrix.names <- matrix(NA, nrow = howmanyvariables, ncol = howmanyclusters)

# The cluster names depend on whether SVD performed.
if(buildSVD == TRUE){
for (i in 1:howmanyvariables){
 for (j in 1:howmanyclusters){
edge.assignment.matrix.names[i,j] <- paste('SVD.L.', i, '.cluster.', j, sep = '')
}
}
}

if(buildSVD == FALSE){
for (i in 1:howmanyvariables){
 for (j in 1:howmanyclusters){
edge.assignment.matrix.names[i,j] <- paste('SVD.L.', i, '.cluster.', j, sep = '')
}
}
}


# 'SVD.L' for Singular Value Decomposition - Left Singular Vectors.
edge.assignment.names.2 <- paste(t(edge.assignment.matrix.names), sep = ',')

#### Edge assignment naming array definition is above.

##############################
# Definition of edge matrix: #
##############################

edge.matrixRowAndCol <- howmanyclusters * howmanyvariables

 edge.assignment.matrix.1.1 <- 
  matrix(data = NA, nrow = edge.matrixRowAndCol, ncol = edge.matrixRowAndCol, byrow = TRUE,
dimnames =
list(edge.assignment.names.2, edge.assignment.names.2))


# This is defined among the function parameters:
# evalq(minimumEventThreshold <- 2, envir = svd.network.env.1.1)

 cluster.pairs <- expand.grid(x = 1:howmanyclusters, y = 1:howmanyclusters)
 variable.pairs <- combn(howmanyvariables, 2)

 edgelist.array <- NULL
 edgelist.fileAndPath <- paste(cat.destination.1.1, '/', 'edgelist.R', sep = '')



# Hard code the condition check for intersections that include ONLY one or two 
# shared event indices.

for(i in 1:ncol(variable.pairs)){
for(j in 1:nrow(cluster.pairs)){



InterDimClusterComparison <- c(edge.assignment.matrix.names[variable.pairs[1L,i], cluster.pairs[j,1L]], 
edge.assignment.matrix.names[variable.pairs[2L,i], cluster.pairs[j,2L]])


intersect.test.value <- length(intersect(PC.clusters[[variable.pairs[1L,i]]][[cluster.pairs[j,1L]]], 
PC.clusters[[variable.pairs[2L,i]]][[cluster.pairs[j,2L]]]))


#############################
# edgelist.R Generated HERE!
#############################

# New for graphfactor.1.2, Tuesday, June 14, 2016, use 
# 'ubFlag' and 'maximumEventThreshold' in the placement of edges.

if(ubFlag == FALSE){

 if(intersect.test.value >= minimumEventThreshold) cat(InterDimClusterComparison, '\n', # the test. If True.
sep = ' ', file = edgelist.fileAndPath, append = TRUE) else NA# if false

} # End 'if(ubFlag == FALSE)...'

if(ubFlag == TRUE){

 if(
  (intersect.test.value >= minimumEventThreshold) & 
   (intersect.test.value <= maximumEventThreshold)) cat(InterDimClusterComparison, '\n', # the test. If True.
sep = ' ', file = edgelist.fileAndPath, append = TRUE) else NA# if false

} # End 'if(ubFlag == TRUE)...'

}
}


scanned.edgelist.1.1 <- scan(edgelist.fileAndPath, what = character())

edgelist.array <- 
matrix(scanned.edgelist.1.1, ncol = 2, byrow = TRUE)

new.graph.1.1 <- graph.edgelist(edgelist.array, directed = FALSE)


##################################################################################

IntraVariableClusters.1.1 <- NULL
IntraVariableClustSizes.1.1 <- NULL

for(modelIterator in 1:howmanyvariables){

 IntraVariableClusters.1.1 <- cbind(
IntraVariableClusters.1.1,
kmeans.models.1.1[[modelIterator]]$cluster)

 IntraVariableClustSizes.1.1 <- cbind(
IntraVariableClustSizes.1.1,
kmeans.models.1.1[[modelIterator]]$size)
}# End for loop for scalable cluster and size column binding.

##################################################################################



# Two plot conditions. If 'showSequence' is TRUE, plot with a legend.
# Otherwise, don't show a legend. (Below.)


if(showSequence == FALSE){
## Else 'showSequence' is FALSE.

par(family = 'serif')
par(bg = 'black')

plot(new.graph.1.1, vertex.shape = 'sphere')

title(
 main = paste('data:', datName, 'variables:', howmanyvariables, 'clusters:', howmanyclusters, 
  'buildSVD:', buildSVD, 'ubFlag:', ubFlag, '\nLower Bound Threshold:', minimumEventThreshold, 
    'Upper Bound Threshold:', maximumEventThreshold, 'seed:', seed, sep = ' '),

           cex.main = 1,   font.main= 1, col.main= "white",
           cex.sub = 0.75, font.sub = 1, col.sub = "white",

 sub = paste(YourNameHere, '\n', Sys.time(), '\n', YourLocationHere))

}# end 'else' (showSequence is FALSE.)





##################################################################################

if(showSequence == TRUE){

# Set Legend Colors.

 set.seed(seed)


 if(howmanyvariables > 4){
  moreColors <- sample(colors(distinct = T), size = (howmanyvariables - 4), replace = T)

 dimensionColors <- 
  c('gold', 
     'springgreen', 
      'rosybrown3', 
       'tomato2', 
        'orangered', moreColors)[1:(howmanyvariables + 1)]
 } else if(howmanyvariables <= 4){

 dimensionColors <- 
  c('gold', 
     'springgreen', 
      'rosybrown3', 
       'tomato2', 
        'orangered')
 }# End if else if speghetti.

##################################################################################


 realizedNodeCount <- length(unclass(new.graph.1.1)[[9]][[3]]$name)

# Define an array to hold the variable-cluster pairs,
# which will come from inspecting the realized graph.

 realizedNodeSpecs <- 
  matrix(NA, nrow = realizedNodeCount, ncol = 3, dimnames = list(NULL, c('Var', 'Clust', 'Size')))

 for(nodeDex in 1:realizedNodeCount){
  realizedNodeSpecs[nodeDex,'Var'] <- 
    substr(unclass(new.graph.1.1)[[9]][[3]]$name[nodeDex], start = 7, stop = 7)
  realizedNodeSpecs[nodeDex,'Clust'] <- 
    substr(unclass(new.graph.1.1)[[9]][[3]]$name[nodeDex], start = 17, stop = 18)
 }

realizedNodeSpecs <- apply(realizedNodeSpecs, MARGIN = 2, FUN = 'as.numeric')
# dim(realizedNodeSpecs)# 31 2
realizedNodeSpecs <- data.frame(realizedNodeSpecs)# Makes 'subset' possible.

# Using the following object, which holds cluster allocations for 
# each event (rows) across variables (columns,) determine which 
# clusters appeared in the network model by comparing it to the 
# semantically extracted components of variable-cluster pairs held 
# in 'realizedNodeSpecs'. Then, color the graph plot.

#IntraVariableClusters.1.1
#realizedNodeSpecs

# 'IntraVariableClusters.1.1': (four events across four variables)
#      [,1] [,2] [,3] [,4]
# [1,]    7    6    8    1
# [2,]    3    7    2    3
# [3,]    1    3    5    5
# [4,]    1    1    2    1


# The coordinates built into 'realizedNodeSpecs' may be used to reference 
# 'IntraVariableClustSizes.1.1'

nodeSizeVec <- NULL
# 'realizedNodeSpecs' column 2 is the Cluster, column 1 is the Variable. (Var, Clust)
# but 'IntraVariableClustSizes.1.1' is (Clust by Var).

# IntraVariableClustSizes.1.1[realizedNodeSpecs[1,2], realizedNodeSpecs[1,1]]

for(spec in 1:dim(realizedNodeSpecs)[1]){
nodeSizeVec[spec] <- 
 IntraVariableClustSizes.1.1[realizedNodeSpecs[spec,2], realizedNodeSpecs[spec,1]]
}

#nodeSizeVec
realizedNodeSpecs[,'Size'] <- nodeSizeVec
#realizedNodeSpecs




# Give the igraph semblance a color vector.
#realizedNodeSpecs_colored.0.1 <- cbind(realizedNodeSpecs, Color = 'gold')
realizedNodeSpecs_colored.0.1 <- cbind(realizedNodeSpecs, Color = 'gold', Frame = 'black')

realizedNodeSpecs_colored.0.1
# levels(realizedNodeSpecs_colored.0.1[,'Color']) <- c('gold', 'springgreen', 'rosybrown3', 'tomato2', 'orangered')

##################################################################################

levels(realizedNodeSpecs_colored.0.1[,'Color']) <- dimensionColors[1:howmanyvariables + 1]

##################################################################################

levels(realizedNodeSpecs_colored.0.1[,'Frame']) <- c('black', 'red')# Node frame color "levels." 
# I highlight the event nodes in red.

 # Strategy Artifact:
 # 1. Recolor the color vector elements that correspond to the first event.
 # 2. Generalize the process.
 # 3. Improve the process for efficiency.





##################################################################################

# When several events are to be given unique plotting treatment.

for (event in seqDex){

 # ...Uniquely define 'realizedNodeSpecs_colored.0.1'
 # as is done for the singular graph condition above.

# Give the igraph semblance a color vector.
#realizedNodeSpecs_colored.0.1 <- cbind(realizedNodeSpecs, Color = 'gold')
realizedNodeSpecs_colored.0.1 <- cbind(realizedNodeSpecs, Color = 'gold', Frame = 'black')

# New implementation for 2.4.R uses 'dimensionColors' variable.
levels(realizedNodeSpecs_colored.0.1[,'Color']) <- c('gold', dimensionColors)
levels(realizedNodeSpecs_colored.0.1[,'Frame']) <- c('black', 'red')

for(modelIterator in 1:howmanyvariables){

BooleanReColor <- 
 subset(realizedNodeSpecs_colored.0.1, realizedNodeSpecs_colored.0.1$Var == modelIterator)$Clust %in% 

IntraVariableClusters.1.1[event, modelIterator]

varTempDex <- which(realizedNodeSpecs_colored.0.1[,'Var'] == modelIterator)

realizedNodeSpecs_colored.0.1[varTempDex[BooleanReColor],'Color'] <- dimensionColors[-1][modelIterator] 

realizedNodeSpecs_colored.0.1[varTempDex[BooleanReColor],'Frame'] <- 'red'
# realizedNodeSpecs_colored.0.1 # It's in there. 

}

# I need two things to access the correct row color, 
# and updated 'varTempDex' and 'BooleanReColor'.
# And I need a new color for the color assignment.

##################################################################################





colorVector <- as.character(realizedNodeSpecs_colored.0.1[,'Color'])
frameVector <- as.character(realizedNodeSpecs_colored.0.1[,'Frame'])

set.seed(201606166)
igraph::V(new.graph.1.1)
igraph::V(new.graph.1.1)$color <- colorVector
igraph::V(new.graph.1.1)$frame.color <- frameVector
igraph::V(new.graph.1.1)$size <- nodeSizeVec + 3

# Include in plot the name of the event.

eventName <- row.names(dat[event,]) # For iterating all the events.



##################################################################################

 # Include the data that corresponds to the event being highlighted in the plot.

  eventData <- dat[event,]
  datNames.text <- colnames(dat)

  leg.text <- NULL
  for(nameDex in 1:howmanyvariables){
   leg.text[nameDex] <- paste(datNames.text[nameDex], eventData[nameDex])

  }







if(plotToFile == TRUE){
 devOffFlag <- 1
# devOffFlag indicates if graphics device should be closed after the call to 'plot'.
# (True only if plotToFile is true.)

png(paste('graphfactorD - 10-08-2016 - Function Viz', event, '.png', sep = ''),
width = 600, height = 600)

} else if(plotToFile == FALSE){
 devOffFlag <- 0
dev.new()
} # End plotToFile Test


#plot.new()

par(family = 'serif')
par(bg = 'black')

plot(new.graph.1.1, vertex.shape = 'sphere')

 legend('bottomleft', legend = leg.text,
  col = dimensionColors[-1], pch = 19, pt.cex = 1.8, cex = 1, text.col = 'white', 
   trace = TRUE, inset = -.05, title = eventName)

title(
 main = paste('data:', datName, 'variables:', howmanyvariables, 'clusters:', howmanyclusters, 
  'buildSVD:', buildSVD, 'ubFlag:', ubFlag, '\nLower Bound Threshold:', minimumEventThreshold, 
    'Upper Bound Threshold:', maximumEventThreshold, 'seed:', seed, 
     '\neventName:', eventName, sep = ' '),

           cex.main = 1,   font.main= 1, col.main= "white",
           cex.sub = 0.75, font.sub = 1, col.sub = "white",

 sub = paste(YourNameHere,'\n', Sys.time(), '\n', YourLocationHere))

# If user decides to plot the image to png file, then the device 
# must be closed after it's populated. That way subsequent images 
# may be written to file and not overwrite previous plots.

if(devOffFlag == 1){
currentDev <- dev.cur()
dev.off(currentDev)
devOffFlag <- 0;
}

}# End for loop that iterates through indices of the seqDex vector.


} 

print(paste("The edgelist and cluster assignments reside on the following path:"))
print(paste(getwd(), '/', cat.destination.1.1, sep = ''))

}
