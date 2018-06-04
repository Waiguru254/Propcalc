#' Two Way Tabulation with HTML Table Output 
#' @name samhtml
#' @param factors The factors to be analysed
#' @param  groups The groups to be considered 
#' @return  value
#' @export samhtml
#' 

samhtml <- function (factor, group) {
  levels_factor<- data.frame(table(factor))
  level<- cbind(as.character(levels_factor[,1]))
  lev<- as.character(level)
  numer_factor<- as.numeric(levels_factor$factor, na.rm=FALSE)
  levels_group<- data.frame(table(group))
  numer_group<- levels_group$group
  nm<- length(numer_group)
  coll<- as.character(numer_group)
  row<- factor
  column<- group
  ########################################################
  tapct <- function(row, column, decimal=1, percent=c("both","col","row"), missing = TRUE,sort.group = c(decreasing, 
                                                                                                         "increasing"), cum.percent = !any(is.na(row)),las=0, main = "auto", xlab = "auto", 
                    ylab = "auto", col="auto",  ...) {
    tab <- table(row, column, deparse.level=1,dnn=list(deparse(substitute(row)),deparse(substitute(column)))) 
    if (missing == TRUE) {
      if (is.factor(row)) {
        tab <- table(row, column, deparse.level=1,exclude = NULL, dnn=list(deparse(substitute(row)),deparse(substitute(column)))) }
    }
    # column percent
    cpercent <-tab
    for(i in 1:ncol(tab)) { cpercent[,i] <-paste("",format(round(tab[,i]/colSums(tab)[i]*100, digits=decimal),trim=TRUE),"", sep="")}
    cpercent <- rbind(cpercent, rep("100", ncol(tab)))
    col.1.1 <- cbind(format(c(tab[,1],sum(tab[,1])), trim=TRUE), cpercent[,1])
    for(i in 2:ncol(tab)){
      col.1.1 <- cbind(col.1.1, c(format(tab[,i], trim=TRUE), format(sum(tab[,i]), trim=TRUE)), cpercent[,i])
    }
    cpercent <- col.1.1
    cnames <- character(0)
    for(i in 1:ncol(tab) ){ cnames <- c(cnames, colnames(tab)[i], "%")}
    colnames(cpercent) <- cnames
    rownames(cpercent)[nrow(cpercent)] <- "Total"
    var1 <- deparse(substitute(row))
    if(length(var1)>1){
      string2 <- var1[length(var1)]	
    }else
      if(substring(search()[2],first=1,last=8)!="package:"){
        string2 <-  attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==deparse(substitute(row))]
        if(length(string2)==0){
          string2 <- deparse(substitute(row))
        }
        if(string2==""){
          string2 <- deparse(substitute(row))
        }
      }else{
        string2 <- deparse(substitute(row))
      }
    if(substring(search()[2],first=1,last=8)!="package:"){
      string4 <-  attr(get(search()[2]), "var.labels")[attr(get(search()[2]), "names")==deparse(substitute(column))]
      if(length(string4)==0){
        string4 <- deparse(substitute(column))
      }else{
        if(string4==""){
          string4 <- deparse(substitute(column))
        }
      }
    }else{
      string4 <- deparse(substitute(column))
    }
    
    suppressWarnings(if(percent=="both" | percent=="col"){
      cat("Column percent", "\n")
      names(attr(cpercent,"dimnames")) <- c(string2, string4)
      print.table(cpercent, right=TRUE, print.gap=2)
      cat( "\n")})
    
    cpercent <- tab
    for(i in 1:ncol(tab)) {cpercent[,i] <- tab[,i]/colSums(tab)[i]*100}
    returns<- cpercent
  } 
  
  fun_pct<- tapct(factor, group, graph = FALSE)
  
  column_matrix <- fun_pct
  ###########################################################
  fun_pctt<- round(as.matrix(fun_pct)[,1:nm],2)
  fun_pctt<- as.data.frame.matrix(fun_pctt)
  fun_pt<- as.matrix(fun_pctt)
  colnames(fun_pt)<- paste("Grp", coll, sep = " ")
  #######
  mat<-matrix(,nrow = length(numer_factor), ncol = length(numer_group));
  for (i in 1:length(levels_group$group)){
    for (j in 1:length(levels_factor$factor)){
      mat[j,i]<- (length(subset(group, group== numer_group[i] & factor==level[j])))
      rownames(mat)<- lev
      colnames(mat)<- paste("Grp-", coll, sep = " ")
    } }
  #################################
  #output<-intersect(colnames(mat),colnames(fun_pt))
  output <- merge(mat, fun_pt, by = "row.names", all = TRUE)
  suppressMessages(kableExtra::kable(output, format = "html") %>%
                     kable_styling(bootstrap_options = c("striped", "bordered"),font_size = 10,
                                   full_width = F) %>%
                     add_header_above(c("Factors" = 1, "Groups Counts" = ncol(mat), "Groups Proportions (%)" = ncol(fun_pt))))}
