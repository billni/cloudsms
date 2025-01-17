rm(list=ls())
func <- function(n) {
			values <- vector()
			sms.url <- 'http://www.bfkdim.com/messages?phone=17131948030&page='
      sms.url <- paste(sms.url, n, sep='')
      tryCatch({
    	html <- htmlTreeParse(sms.url, useInternalNode=T)
    	data.tables <- getNodeSet(html, path = "//tbody//tr")
    	values<- sapply(data.tables, xmlValue, simplify = 'array')
    	values <- gsub("\r","", values)
    	values <- gsub("\n","", values)
    	values <- gsub("\t",",", values)
    	values <- gsub(" ", "", values)
    	values <- gsub(",,," , ",", values)
    	values <- gsub(",," , ",", values)
    	values <- gsub(",,,," , "|", values)
    	values <- gsub(",,", "|", values)
    	values <- gsub("," , "|", values)
    	} ,error=function(e){
    			Sys.sleep(0.5)
    	    cat("ERROR :", conditionMessage(e),"\n")}
    	    ,finally=(return(values))
    	)
    	
}

#main
system.time({
x <- 1:5663
results <- list()
cl <- makeCluster(4) # 初始化四核心集群
clusterEvalQ(cl, {
		require(parallel)
		require(XML)
}) # 导入使用的包
results <- clusterApply(cl, x, func) # 并行执行
stopCluster(cl) # 关闭集群
})