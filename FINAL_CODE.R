#------ loading library-------#

library(readxl)

#------ function_corelation------#

covar<-function(x,y)
{
  tvar1<-c(x[1:2201])
  tvar2<-c(y[1:2201])
  sumxy<-0
  sx<-0
  sy<-0
  sqx<-0
  sqy<-0
  for(i in 1:2201)
  {
    sumxy=sumxy+(tvar1[i]*tvar2[i])
    sx<-sx+tvar1[i]
    sy<-sy+tvar2[i]
    sqx<-sqx+(tvar1[i]*tvar1[i])
    sqy<-sqy+(tvar2[i]*tvar2[i])
  }
  sumxy<-2201*sumxy
  temp<-sx*sy
  num<-sumxy-temp
  dem1<-2201*sqx
  temp<-sx*sx
  dem1<-dem1-temp
  dem2<-2201*sqy
  temp<-sy*sy
  dem2<-dem2-temp
  dem1<-sqrt(dem1)
  dem2<-sqrt(dem2)
  result<-num/(dem1*dem2)
  return(result)
}    

#-------function_eculidean-distance----#

ecd<-function(x,y)
{
  
  var1<-c(x[1:2201])
  var2<-c(y[1:2201])
  result<-var1-var2
  mn<-sum(result)/2201
  mn<-abs(mn)
  return(mn)
}

#------function_percentage----#

p<-function(x,y)
{
  b<-list()
  count<-0
  for(i in 1:2201)
  {
    if(x[i]>=y[i]){
      val1<-x[i]
      val2<-y[i]
      var<-val2/val1
      var<-var*100
      b<-c(b,var)
      if((var>=80)|(is.nan(var))){
        count<-count+1
      }
    }
    else if(x[i]<y[i]){
      val1<-x[i]
      val2<-y[i]
      var<-val1/val2
      var<-var*100
      b<-c(b,var)
      if((var>=80)|(is.nan(var))){
        count<-count+1
      }
    }
  }
  count<-count/2201
  count<-count*100
  return(count)
}

#------- choosing file-----#
cat("please choose the dataset to be loaded...(only .xlsx is supported)")
Sys.sleep(2)

#------ importing dataset----#

ds<-read_excel(file.choose(),sheet = 1,col_names = FALSE)

#------extracting data from dataset-----#
ro<-nrow(ds)
co<-ncol(ds)
l<-list()
if(ro<=co)
{
  for(i in 1:ro)
  {
    l[i]<-list(as.double(ds[i,]))
  }
}else{
  for(i in 1:co)
  {
    l[i]<-list(as.double(ds[,i]))
  }
  t<-ro
  ro<-co
  co<-t
}

#------ assing the datas to variables------#

cat("\n choose the file with ref.values (only excel supported)")
ref<-read_excel(file.choose(),sheet = 1,col_names = FALSE)
ref_row<-as.double(ref[1,])           


#-------driver_code------#

for(i in 1:ro)
{
  res1<-FALSE
  res2<-FALSE
  res3<-FALSE
  temp2<-as.double(unlist(l[i]))
  r<-cor(ref_row,temp2)
  cat("\n The corelation coeffient between ref_data and test_data",i," is :",r)
  e<-ecd(ref_row,temp2)
  cat("\n mean of eculidean distance between ref_data and test_data",i," is :",e)
  perc<-p(ref_row,temp2)
  cat("\n percent of test_data matching with ref_data",i," is :",perc)
  if((r>=0.9236)&(r<=1))
  {
    res1<-TRUE
  }
  if((e>=0)&(e<=0.0325))
  {
    res2<-TRUE
  }
  if(perc>75)
  {
    res3<-TRUE
  }
  if(res1&res2&res3)
  {
    cat("\n \n THE TEST_DATA",i," IS MATCHING.")
  }
}







           