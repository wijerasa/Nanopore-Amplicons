fast5_stats<-function(x, readtype=""){

  
  len.sorted<-rev(sort(x))
  total_reads<-length(len.sorted)
  longest_read<-max(len.sorted)
  total_base_pairs<-sum(len.sorted)
  
  mean<-mean(len.sorted)
  median<-median(len.sorted)
  min<-min(len.sorted)
  N25 <- len.sorted[cumsum(len.sorted) >= sum(len.sorted)*0.25][1]
  
  N50 <- len.sorted[cumsum(len.sorted) >= sum(len.sorted)*0.5][1]
  
  N75 <- len.sorted[cumsum(len.sorted) >= sum(len.sorted)*0.75][1]
  x=as.matrix(c(total_reads, total_base_pairs,mean,median,min,longest_read,N25,N50,N75),ncol=1, byrow=TRUE)
  rownames(x) <-c("Total Reads", "Total Base Pairs", "mean","median", 'min', 'max', 'N25', 'N50', 'N75')
  colnames(x)<-readtype
  options(scipen=999)
  summary_stats<-as.table(x)
  
  return(summary_stats)
  
  
  
  
}