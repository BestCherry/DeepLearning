http://blog.sina.com.cn/s/blog_727a704c0102vo4y.html

options(max.print=10000)
options(digits=10)

#1.读取数据
library(readr) ## 用来读入要分析的问卷
allf <- read_csv("D:/r_path/comment_type_3.csv",col_names = TRUE,quote = '"',locale = locale(encoding = "UTF8")) 
colnames(allf)[1]<-'content'
colnames(allf)[2]<-'type'


#2.分词
#一级清洗——去标点
allf$content <- as.vector(allf$content) #文本内容转化为向量sentence
allf$content <- gsub(pattern = " ", replacement ="", allf$content) #gsub是字符替换函数，去空格
allf$content <- gsub("\t", "", allf$content) #有时需要使用\\\t
allf$content <- gsub(",", "，", allf$content) #文中有英文逗号会报错，所以用大写的“，”
allf$content <- gsub("~|'", "", allf$content) #替换了波浪号（~）和英文单引号（'），它们之间用“|”符号隔开，表示或的关系
allf$content <- gsub("\\\"", "", allf$content) #替换所有的英文双引号（"），因为双引号在R中有特殊含义，所以要使用三个斜杠（\\\）转义
allf$content <- gsub("\\(", "", allf$content)
allf$content <- gsub("\\)", "", allf$content)
allf$content <- gsub("\\\"", "", allf$content)
#二级清洗——去内容
allf$content <- gsub("[[:digit:]]*", "", allf$content) #清除数字[a-zA-Z]
allf$content <- gsub("[a-zA-Z]", "", allf$content) #清除英文字符
allf$content <- gsub("\\.", "", allf$content) #清除全英文的dot符号
allf <- allf[which(!is.na(allf$content)==TRUE&!nchar(allf$content) < 2),]#清除内容的空值和单个词语的文档

# allf$content <- allf$content[!is.na(allf$content)] #清除对应sentence里面的空值（文本内容），要先执行文本名
# allf$content <- allf$content[!nchar(allf$content) < 2] #`nchar`函数对字符计数，英文叹号为R语言里的“非”函数

# 增加一列文档词个数，按词长度升序，看看是否有空值或者什么的
# allf["nchar"]=nchar(allf$content)
# allf[order(allf$nchar),]

# 增加一列doc_id
#allf["doc_id"]=seq(from = 1,to=nrow(allf),by=1)

## 转为dataframe
allf_table <- as.data.frame(allf)



## 用户自定义分词加载专门的问道词典
setwd("D:/r_path")
library(cidian)
decode_scel(scel = "./wendao.scel",cpp = TRUE) 
decode_scel(scel = "./更新问道的术语.scel",cpp = TRUE) 
decode_scel(scel = "./问道.scel",cpp = TRUE) 
decode_scel(scel = "./问道【官方推荐】.scel",cpp = TRUE) 
DICTPATH
## 查看生成的词典
scan(file="./wendao.scel_2019-03-19_09_31_56.dict",
     what=character(),nlines=30,sep='\n',
     encoding='utf-8',fileEncoding='utf-8')

scan(file="./更新问道的术语.scel_2019-03-19_09_32_13.dict",
     what=character(),nlines=30,sep='\n',
     encoding='utf-8',fileEncoding='utf-8')

scan(file="./问道.scel_2019-03-19_09_32_30.dict",
     what=character(),nlines=30,sep='\n',
     encoding='utf-8',fileEncoding='utf-8')

scan(file="./问道【官方推荐】.scel_2019-03-19_09_32_48.dict",
     what=character(),nlines=30,sep='\n',
     encoding='utf-8',fileEncoding='utf-8')

## 把各词典的内容粘贴在dir(show_dictpath())中的user.dict.utf8里面即可



## 分词
library(jiebaR)
#创建分词器 bylines=T 一定要写，否则会报错
cutter <- worker(type="tag",bylines = T,write = T,stop_word = 'D:\\r_path\\chinese_stopword_UTF-8.txt')
allf_table$content <- cutter[allf_table$content]
allf_table <- allf_table[which(allf_table$content!="character(0)"),]

allf_table["doc_id"] <- seq(from = 1,to=nrow(allf_table),by=1)




# fwrite可以写入含有list的文件，会丢失list元素的name，但是需要用notepad++打开才不会乱码
fwrite(allf_table, file ="myDT.csv")



# -- ======================词性拆分的实验start
library(pacman)
p_load(jiebaR,tidyverse)
cn <- allf$content
#构造分词标注器
tag_worker = worker(type = "tag",bylines = T,write = T,stop_word = 'D:\\r_path\\chinese_stopword_UTF-8.txt')    #构造分词标注器
#标注词性
segment(cn,tag_worker) -> cnnn



#构造函数，把每个list下的character转为data_frame，一列是词性，一列是词语
cx_fun <- function(x){
   list(speech=names(x),term=x)
}
# 执行转dataframe的函数
cnnnn <- lapply(cnnn, cx_fun)




# 把list转为data_frame，不要用do.call，do.call的行id会乱七八糟
library (plyr)
cx_data <- ldply (cnnnn, data.frame)


# 拆分转置，一个词一行,记得先把allf_table拆分好的字段中，空的记录去掉，否则和词性的dataframe匹配不上
library(tidyr)
my_data_pre <- allf_table[which(allf_table$content!="character(0)"),] %>% separate_rows(content, sep = ",")

# 把分词和词性结合起来
my_data <- cbind(my_data_pre,cx_data)



## 替换杂七杂八的符号
my_data$content <- gsub('\\"','',my_data$content)
my_data$content <- gsub('\\,','',my_data$content)
my_data$content <- gsub("[[:digit:]]*", "", my_data$content) #清除数字[a-zA-Z]
my_data$content <- gsub("[a-zA-Z]", "", my_data$content) #清除英文字符
my_data$content <- gsub("\\(", "", my_data$content)
my_data$content <- gsub("\\)", "", my_data$content)
my_data$content <- gsub("\\=", "", my_data$content)
my_data$content <- gsub(pattern = " ", replacement ="", my_data$content) #gsub是字符替换函数，去空格
my_data <- my_data[which(nchar(my_data$content) >=1),] #把空格替换成""之后，要把没内容的记录删除掉



# 每篇文档各类词性包含的词语 
speech_data <- sqldf('select doc_id,sum(case when speech like "n%" then 1 else 0 end ) as "speech_n",
      sum(case when speech like "a%" then 1 else 0 end ) as "speech_a",
      sum(case when speech like "v%" then 1 else 0 end ) as "speech_v",
      sum(case when speech not like "n%" and speech not like "a%" and speech not like "v%" then 1 else 0 end ) as "speech_other",
      count(*)as terms
      from my_data group by doc_id order by doc_id')

# 每个文档下的每类词性下的词数，除以该文档总词数
speech_data <- t(apply(speech_data,1,function(x) x / x[length(x)]))

--======================================到这到这到这到这到这到这

# 每篇文档的type
doc_type_data <- sqldf('select doc_id,type from my_data group by doc_id,type order by doc_id asc')

## 统计各单词在各类型文档中出现的文档数
library(sqldf)
## 统计各单词在各类型下出现的文档数
mydata <- sqldf('select content,type,count(distinct doc_id)times from my_data group by content,type order by content asc')
## 统计各类型的文档数
n_t_mydata <- sqldf('select type,count(distinct doc_id)times from my_data group by type')
## 各单词在各类型下出现的次数
n_w_mydata <- sqldf('select content,type,count(1)times from my_data group by content,type')
## 各单词在各文档下出现的次数
n_w_d_mydata <- sqldf('select content,doc_id,count(1)times from my_data group by content,doc_id')


## 所有的词
words <- names(table(mydata$content))

## 所有的type类型
doc_type <- names(table(mydata$type))

## 所有词个数
n_w <- length(words)
## 所有type个数
n_t <- length(doc_type)
## 总文档数
# library(dplyr)
n_dos <- n_distinct(my_data$doc_id) # 本来是用dos计数的，但是因为mydata有删除内容为空的doc，所以有计数的doc和实际的doc编号是不一一对应的
id_doc <- names(table(my_data$doc_id))



## 构建二维矩阵，行为type的id，纵列为词的id，值为chi值
chi <- matrix(numeric(n_w*n_t),n_w); 
#ABCD <- matrix(numeric(n_w*n_t*7),n_w*n_t); #matrix（一共多少变量，每列填充多少）

for(t in doc_type){
  for (w in words) {
    tt <- match(t,doc_type)
    ww <- match(w,words)
    word <- words[ww]
    type <- doc_type[tt]
    A <- if(length(mydata[which(mydata$type==t&mydata$content==w),]$times)==0) 0 else mydata[which(mydata$type==t&mydata$content==w),]$times ##包含w且属于t类的文档数
    B <- sum(mydata[which(mydata$content==w),]$times)-A ## 包含w但是不属于t类的（包含w的文档数-A）
    C <- n_t_mydata[which(n_t_mydata$type==t),]$times-A  ## 属于t类，但是不包含w的文档数
    D <- n_dos-A-B-C
    # 计算在t类文档中词w出现的次数，用到n_w_mydata数据表
    n_w_t <- if(length(n_w_mydata[which(n_w_mydata$content==word&n_w_mydata$type==type),]$times)==0) 0 else n_w_mydata[which(n_w_mydata$content==word&n_w_mydata$type==type),]$times
    # 计算t类的文档总数
    n_t <- n_t_mydata[which(n_t_mydata$type==type),]$times
    alpha_wt <-  n_w_t/n_t
    beat <- A/(B+1) # 引入类见因子，类别 t 中包含特征 w的文档数 除以 包含w但是不属于t类的文档书 
    if(A+B+C+D==n_dos&A*D>B*C) #在卡方统计量公式里删除特征出现与所属类别负相关的情况
    {chi[ww,tt] <-n_dos*((A*D-B*C)^2)*alpha_wt*beat/((A+C)*(A+B)*(B+D)*(C+D))}
    else chi[ww,tt] <- 0
    # ABCD[ww,1] <- words[ww]
    # ABCD[ww,2] <- doc_type[tt]
    # ABCD[ww,3] <- A
    # ABCD[ww,4] <- B
    # ABCD[ww,5] <- C
    # ABCD[ww,6] <- D
    # ABCD[ww,7] <- chi[ww,tt]
  }
}



## alt+L 折叠代码区块
-- =================================循环结构的测试

for(t in doc_type){
  for (w in words) {
    cat(t,w,"\n") # 仅用cat函数打印就好了。不要用sprintf
  }
}

K <- 3  paste0("Topic", seq_len(K)) tips 依次产生 文字+递增序号的效果
-- ==================================
  
  
  ## 提取chi值前5的词作为特征词
  chitt <- chi #为了防止操作错误，我把chi
rownames(chitt) <- words
colnames(chitt) <- doc_type


# 分别排序看看各特征的chi值大小
head(chitt[order(chitt[,11], decreasing = TRUE),],30)

## 编写函数，每列取前15名的chi值作为特征词,且值要大于0
# sortx <- function(x){
#   y <- sort(x,decreasing = T)
#   return(names(y[which(y>0)][1:5]))
# }
# chittt <- as.data.frame(apply(chitt,2,sortx))# 应用到chitt中

# 根据chi值大于4来判断
sorty <- function(x){
  y <- sort(x,decreasing = T)
  return(names(y[which(y>=4)][1:5]))
}

chittt <- as.data.frame(apply(chitt,2,sorty))# 应用到chitt中



library(tidyr) #有gather函数和spread函数
library(dplyr)

keyword_frame <- gather(chittt,key = variable, value = value) #把所有类型的关键词合并在一起
names(keyword_frame)[names(keyword_frame)=="value"]="keyword"
names(keyword_frame)[names(keyword_frame)=="variable"]="doc_type"

keyword <- na.omit(unique(keyword_frame$keyword)) #几类中，特征词一样的需要去重





## 这个是网上看到的，把文档用特征矩阵表示的方法，统计的特征权重是词频数，网址：https://ask.hellobi.com/blog/R_shequ/12259
## 把文档用特征矩阵表示
library(tm)
### 建立语料库，这个是应用其他的
commentseg<-c()
for(i in 1:16)
  commentseg[i]<- gsub('\\,',' ',allf_table$content[i]) %>% paste(.,collapse=" ")

### 替换杂七杂八的东西
commentseg <- gsub("[[:digit:]]*", "", commentseg) #清除数字[a-zA-Z]
commentseg <- gsub("[a-zA-Z]", "", commentseg) #清除英文字符
commentseg <- gsub("\\(", "", commentseg)
commentseg <- gsub("\\)", "", commentseg)
commentseg <- gsub('\\"','',commentseg)
commentseg <- gsub('\\,','',commentseg)
write.csv(commentseg,file="commentseg.csv", fileEncoding = "GBK")

# ss<-read.csv("commentseg.csv",fileEncoding = "GBK",stringsAsFactors = FALSE) #这边是采用写入文件，再读取文件的方法，实际也可以不用这么麻烦，直接读commentseg就好了
# docs<-ss$x %>% tolower()
docs <- commentseg %>% tolower()
d.corpus<-VCorpus(VectorSource(docs))  # 建立语料库

keyword<-keyword$term %>% tolower() # 之前通过tf-idf筛选的核心词汇
dtm <- DocumentTermMatrix(d.corpus,control = list(wordLengths=c(1,Inf),dictionary=keyword))  # 注意wordLengths的取值设定
df_mat<-as.matrix(dtm)  # 转换为矩阵形式
# write.csv(df_mat,file="df_mat.csv")




#计算tf-idf
TF-IDF = 词频（TF）* 逆文档词频（IDF）

TF = 某个词在文档中出现的次数/文章的总词数

IDF = log(语料库的文档总数/(包含该词的文档数 + 1))

那么用一个函数就可以搞定：
tfidf = function(x) t(t(x)/colSums(x))*log(ncol(x)/rowSums(x>0))


## 构建二维矩阵，行为type的id，纵列为词的id，值为chi值
#关键词个数
n_key <- n_distinct(keyword)
word_docs<- matrix(numeric(n_dos*n_key),n_dos)
keyw <- keyword
colnames(word_docs) <- keyw #给列命名
#rownames(word_docs) <- 1:n_dos #给行命名
rownames(word_docs) <- id_doc


for(t in id_doc){
  for (w in keyw) {
    #词w在n_dos出现的次数
    tw <- if(length(n_w_d_mydata[which(n_w_d_mydata$content==w&n_w_d_mydata$doc_id==t),]$times)==0) 0 else n_w_d_mydata[which(n_w_d_mydata$content==w&n_w_d_mydata$doc_id==t),]$times
    #dos的总词数
    tdw <- if(length(n_w_d_mydata[which(n_w_d_mydata$doc_id==t),]$times)==0) 0 else sum(n_w_d_mydata[which(n_w_d_mydata$doc_id==t),]$times)
    #总文档数n_dos
    #出现词W的文档数
    d_w_n <- if(length(n_w_d_mydata[which(n_w_d_mydata$content==w),]$doc_id)==0) 0+1  else   n_distinct(n_w_d_mydata[which(n_w_d_mydata$content==w),]$doc_id)+1
    word_docs[t,w] <- if(tdw==0) 0 else tw/tdw*log(n_dos/d_w_n)
  }
}



for(t in id_doc){
  for (w in keyw) {
    #词w在n_dos出现的次数
    tw <- if(length(n_w_d_mydata[which(n_w_d_mydata$content==w&n_w_d_mydata$doc_id==t),]$times)==0) 0 else n_w_d_mydata[which(n_w_d_mydata$content==w&n_w_d_mydata$doc_id==t),]$times
    #dos的总词数
    tdw <- if(length(n_w_d_mydata[which(n_w_d_mydata$doc_id==t),]$times)==0) 0 else sum(n_w_d_mydata[which(n_w_d_mydata$doc_id==t),]$times)
    word_docs[t,w] <- if(tdw==0) 0 else tw/tdw
  }
}




## 把文档-词矩阵和类别合并在一起，形成建模的数据形式


final_data <- as.data.frame(cbind(type=doc_type_data$type,word_docs))# 把文档词矩阵和各文档的类型合并在一起

final_data[,2:ncol(final_data)] <- lapply(final_data[,2:ncol(final_data)],as.numeric) #因为文字类型的和数值类型的合并，导致数值类型的变成了char类型，需要修改
final_data$type <- as.factor(final_data$type) # 转为fator类型




# 分别选取训练样本(70%)和测试样本(30%)
index <- sample(2,nrow(final_data),replace = TRUE,prob=c(0.7,0.3))
traindata <- final_data[index==1,]
testdata <- final_data[index==2,]

nrow(final_data)
nrow(traindata)
nrow(testdata)

##SVM分类 
library(e1071)

## 各类方法组合
svm_test <- function(x,y){
  type <- c('C-classification','one-classification')
  kernel <- c('linear','polynomial','radial','sigmoid')
  pred <- array(0, dim=c(nrow(x),2,4))
  errors <- matrix(0,2,4)
  dimnames(errors) <- list(type, kernel)
  for(i in 1:2){
    for(j in 1:4){
      pred[,i,j] <- predict(object = svm(x, y, type = type[i], kernel = kernel[j]), newdata = x)
      if(i > 2) errors[i,j] <- sum(pred[,i,j] != 1)
      else errors[i,j] <- sum(pred[,i,j] != as.integer(y))
    }
  }
  return(errors)
}





#查看这12种组合在iris数据集中的优劣表现：
test_data_x <- testdata[,2:ncol(testdata)]
test_data_y <- as.factor(testdata[,1])
svm_test(x=test_data_x,y=test_data_y)

# gamma值和cost值
tune.svm(type ~., data =testdata, gamma = 10^(-100:-1), cost = 10^(0:3))

# 权重 

## 建模测试集
train_svm_model <- svm(type~.,data=testdata,type='C-classification',kernel='linear',cost=10,scale=FALSE)
train_svm_model_pred_1 <- predict(train_svm_model,traindata[,-1])
train_table_1 <- table(pred=train_svm_model_pred_1,true=traindata[,1])
accuracy_1 <- sum(diag(train_table_1))/sum(train_table_1)
accuracy_1



## 结果写入文件
t_result <- allf[which(allf$doc_id %in% names(train_svm_model_pred_1)),]
result  <-  data.frame(t_result,train_svm_model_pred_1)
write.csv(result,file = "rf_result5.csv",row.names = F)


library(rCharts)
nPlot(Petal.Length~Petal.Width,group='Species',data=iris,
      type='scatterChart')

head(testdata)


## 建模final，这个一般随便看看，因为模型本身就是根据final中的test集去建模的
# final_svm_model <- svm(type~.,data=testdata,type='C-classification',kernel='linear',gamma=0.01,cost=100)
# final_svm_model_pred_1 <- predict(final_svm_model,traindata[,-1])
# final_table_1 <- table(pred=final_svm_model_pred_1,true=final_data[,1])
# accuracy_final <- sum(diag(final_table_1))/sum(final_table_1)
# accuracy_final




