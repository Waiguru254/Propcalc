#' Binomial Chi-Square Test for Trend in Proportions (The Cochran-Armitage Trend Test)
#' @name Chi.trend.test
#' @param row The Cochran-Armitage Trend Test
#' @return  value
#' @export Chi.trend.test
#' 
Chi.trend.test<- function(binomial,ordinal,table=NULL){
  if(is.null(table)){
    dname="Chi-Square Test for Trend  in Proportions"
    tbl=table(binomial,ordinal)
    N1i=tbl[1,]
    N2i=tbl[2,]
    R1=sum(N1i)
    R2=sum(N2i)
    T=VT1=VT2=0
    for(i in 1:ncol(tbl)){
      T=T+(i-1)*(N1i[i]*R2-N2i[i]*R1)
      VT1=VT1+(i-1)^2*sum(tbl[,i])*(sum(tbl)-sum(tbl[,i]))
      if(i<=(ncol(tbl)-1)){VT2=VT2+(i-1)*i*sum(tbl[,i])*sum(tbl[,i+1])}
    }
    VT=R1*R2/sum(tbl)*(VT1-2*VT2)
    Z=T/sqrt(VT)
  }
  if(!is.null(table)){
    dname="Chi-Square Test for Trend  in Proportions"
        tbl=table
    N1i=tbl[1,]
    N2i=tbl[2,]
    R1=sum(N1i)
    R2=sum(N2i)
    T=VT1=VT2=0
    for(i in 1:ncol(tbl)){
      T=T+(i-1)*(N1i[i]*R2-N2i[i]*R1)
      VT1=VT1+(i-1)^2*sum(tbl[,i])*(sum(tbl)-sum(tbl[,i]))
      if(i<=(ncol(tbl)-1)){VT2=VT2+(i-1)*i*sum(tbl[,i])*sum(tbl[,i+1])}
    }
    VT=R1*R2/sum(tbl)*(VT1-2*VT2)
    Z=T/sqrt(VT)
  }
  Z=as.numeric(Z)
  P=pnorm(q=abs(Z),lower.tail=FALSE)
  structure(list(method="The Cochran-Armitage Trend Test",
                 statistic=c("Z"=round(Z,3)),p.value=round(P*2,4),
                 data.name=dname),class="htest")
}
