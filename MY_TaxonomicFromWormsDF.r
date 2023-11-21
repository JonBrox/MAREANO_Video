library(ape)
library(worms)
library(worrms)

traverse <- function(a,i,innerl){
  if(i < (ncol(df))){
    alevelinner <- as.character(unique(df[which(as.character(df[,i])==a),i+1]))
    desc <- NULL
    ##if(length(alevelinner) == 1) (newickout <- traverse(alevelinner,i+1,innerl))
    ##else {
    for(b in alevelinner) desc <- c(desc,traverse(b,i+1,innerl))
    il <- NULL; if(innerl==TRUE) il <- a
    (newickout <- paste("(",paste(desc,collapse=","),")",il,sep=""))
    ##}
  }
  else { (newickout <- a) }
}

df2newick <- function(df, innerlabel=FALSE){
  alevel <- as.character(unique(df[,1]))
  newick <- NULL
  for(x in alevel) newick <- c(newick,traverse(x,1,innerlabel))
  (newick <- paste("(",paste(newick,collapse=","),");",sep=""))
}


Aphia2TaxoTree<-function(df){
  require(worrms)
  rank_levels <- c("AphiaID","ClassLevel", 'Phylum' , 'Subphylum', 'Superclass','Class','Subclass', 'Infraclass','Superorder', 'Order', 'Suborder', 'Infraorder', 'Superfamily', 'Family', 'Subfamily', 'Infrafamily', 'Genus', "Species")
  rank_columns<- data.frame(matrix(ncol=length(rank_levels),nrow=length(df), dimnames=list(NULL, rank_levels), data = NA))
  rank_levels <- data.frame(rank = c(rank_levels), Level = 1:18)
  Taxonomy_df<-matrix(NA, nrow = length(df), ncol = 18)
  colnames(Taxonomy_df)<-rank_levels$rank
  Taxonomy_df[,'AphiaID']<-df
  for(i in 1:length(df)){
    temp_classification<-wm_classification(id = df[i])
    ClassLevel<-temp_classification$rank[nrow(temp_classification)]
    temp_classification<-merge(rank_levels, temp_classification, by = "rank", all.x = TRUE)
    temp_classification<-temp_classification[order(temp_classification$Level), c(1,3,4)]
    Taxonomy_df[i,2:nrow(temp_classification)]<-c(t(temp_classification[,'scientificname']))[2:nrow(temp_classification)]
    Taxonomy_df[i,'ClassLevel']<-ClassLevel
  }
  return(Taxonomy_df)
}



toTree <- function(data, column_order = NULL) {
  
  data <- data %>% data.frame() %>%
    mutate_if(is.factor, as.character) 
  
  data <- replace(data, is.na(data), "NA")
  
  # remove duplicated rows
  isDup <- duplicated(data)
  if (any(isDup)) {
    warning(sum(isDup), " duplicated rows are removed")
    data <- data[!isDup, , drop = FALSE]
  }
  
  # check duplicated leaves
  vleaf <- data[, ncol(data)]
  if (anyDuplicated(vleaf)) {
    stop("Not allowed to have duplicated values in the leaf column: ", 
         colnames(data)[ncol(data)])
  }
  
  # the column order 
  if (!is.null(column_order)) {
    data <- data[, column_order, drop = FALSE]
  }
  column_order <- colnames(data)
  
  # The column should start from high level to low level (the root has the
  # highest level)
  ncat <- apply(data, 2, FUN = function(x) {
    length(unique(x))
  })
  if (is.unsorted(ncat)) {
    stop("The current column order: ", 
         paste(column_order, collapse = " "), ";
             Please order columns from high to low. 
             (High: the root; Low: leaves)")
  }
  
  data <- arrange_all(data.frame(data, stringsAsFactors = FALSE))
  if (ncat[[1]] !=1) {
    warning("The root is added with label 'ALL'")
    data <- cbind(root = rep("ALL", nrow(data)), data)
  }
  
  # decide leaf nodes
  vleaf <- data[, ncol(data)]
  numL <- seq_along(vleaf)
  names(numL) <- vleaf
  
  
  # decide internal nodes
  nc <- ncol(data) - 1
  
  datL1 <- lapply(seq_len(nc), FUN = function(x) {
    xx <- data[, x]
    nam <- colnames(data)[x]
    paste(nam, xx, sep = ":")
  })
  datL2 <- do.call(cbind, datL1)
  
  nodeI <- as.vector(t(datL2))
  nodeI <- setdiff(nodeI, vleaf)
  numI <- length(numL) + seq_along(nodeI)
  names(numI) <- nodeI
  
  # all nodes
  numN <- c(numL, numI)
  
  # create edges
  datL3 <- cbind(datL2, vleaf)
  mat1 <- apply(datL3, 2, FUN = function(x) {
    numN[x]})
  
  nr <- nrow(datL3)
  lx <- lapply(seq_len(nr), FUN = function(x) {
    mx <- mat1[x, ]
    mx <- mx[!is.na(mx)]
    cx <- cbind(head(mx, -1), tail(mx, -1))
    cx})
  
  mat2 <- do.call(rbind, lx)
  mat3 <- mat2[!duplicated(mat2), ]
  
  tt <- table(mat3[, 2])
  if (any(tt > 1)) {
    bt <- tt[tt>1]
    st <- as.numeric(names(bt))
    lt <- gsub(pattern = ".*:",  replacement = "",
               names(numN[st]))
    ct <- gsub(pattern = ":.*",  replacement = "",
               names(numN[st]))
    dt <- data.frame(node = st,
                     Freq = as.vector(bt),
                     column = ct,
                     value = lt)
    stop("The tree can't be built; loops detected in the tree.
             Try 'resolveLoop' to remove loops. ")
  }
  
  # sort node number
  numL <- sort(numL)
  numI <- sort(numI)
  
  # tree
  treeList <- vector("list", 5)
  names(treeList) <- c("edge", "tip.label", "node.label",
                       "edge.length", "Nnode")
  treeList$edge <- matrix(mat3, ncol = 2)
  treeList$tip.label <- names(numL)
  treeList$node.label <- names(numI)
  treeList$edge.length <- rep(0.1, nrow(mat3))
  treeList$Nnode <- length(numI)
  class(treeList) <- "phylo"
  treeList <- reorder(treeList)
  
  treeList
}
