
library(rpart)
library(dplyr)

train = fread('../input/train.tsv',sep = '\t')
test = fread('../input/test.tsv',sep = '\t')

# Any results you write to the current directory are saved as output.
head(train)
head(test)
dim(train)
#################
train[train == ''] = NA
train$price[train$price <= 0] <- NA ##############

# removing rows with column's na
dim(train) # no change in rows?
temp = train[!with(train,is.na(name)& is.na(item_condition_id)&is.na(category_name)& 
                    is.na(brand_name)& is.na(price)& is.na(shipping)& is.na(item_description)),]
dim(temp)

sum(is.na(temp$category_name))
sum(is.na(temp$item_condition_id))
sum(is.na(temp$brand_name))
sum(is.na(temp$shipping))
sum(is.na(temp$name))
sum(is.na(temp$item_description))
sum(is.na(temp$price))

temp1 = temp
# changing class 
sapply(temp1, class)
# col = c('item_condition_id','category_name','brand_name','shipping')
# temp1[col] = lapply(temp1[col], factor)#######
temp1$item_condition_id = as.factor(temp1$item_condition_id)
temp1$category_name = as.factor(temp1$category_name)
temp1$brand_name = as.factor(temp1$brand_name)
temp1$shipping = as.factor(temp1$shipping)
temp1$name = as.character(temp1$name)
temp1$item_description = as.character(temp1$item_description)
sapply(temp1, class)

temp1 = temp1[,c('name','item_description', 'item_condition_id', 'shipping',
                 'brand_name','category_name')]
# category NA
temp1$category = sub('.*/', '', temp1$category_name)
uniqCategory = unique(unlist(temp1$category))
uniqCategory = uniqCategory[!is.na(uniqCategory)]
any(is.na(uniqCategory))
uniqCategory = sort(uniqCategory, decreasing = T)

for (i in 1:nrow(temp1)){
  if(is.na(temp1[i,"category"]))
  {
    temp1 [i,"category"] = str_extract(temp1$name, paste(uniqCategory, collapse="|"))
	temp1 [i,"category"] = str_extract(temp1$item_description, paste(uniqCategory, collapse="|"))
  }
}
sum(is.na(temp1$category))  

#  brand NA
uniBrand = unique(unlist(temp1$brand_name))
uniBrand = uniBrand[!is.na(uniBrand)]
any(is.na(uniBrand))
uniBrand = sort(uniBrand, decreasing = T)

for (i in 1:nrow(temp1)){
  if(is.na(temp1[i,"brand_name"]))
  {
    temp1 [i,"brand_name"] = str_extract(temp1$name, paste(uniBrand, collapse="|"))
	temp1 [i,"brand_name"] = str_extract(temp1$item_description, paste(uniBrand, collapse="|"))
  }
}

sum(is.na(temp1$brand_name))



temp1 = temp1[,c('item_condition_id', 'shipping', 'brand_name','category')]


# item_condition_id
item_condition_id.Fit = rpart(item_condition_id ~ as.factor(shipping) + as.factor(brand_name) + as.factor(category), train = temp1, method = "class", na.action = na.rpart)     

for (i in 1:nrow(temp1)){
  if(is.na(temp1[i,"item_condition_id"]))
  {
    temp1 [i,"item_condition_id"] = predict(item_condition_id.Fit, newtrain = temp1[i,], type="class", na.rm=TRUE)
  }
}

# shipping
shipping.Fit = rpart(shipping ~ as.factor(item_condition_id) + as.factor(brand_name) + as.factor(category), train = temp1, method="class", na.action = na.rpart)     


for (i in 1:nrow(temp1)){
  if(is.na(temp1[i,"shipping"]))
  {
    temp1 [i,"shipping"] = predict(shipping.Fit, newtrain = temp1[i,], type="class", na.rm=TRUE)
  }
}

trainF = temp1[,c()]

# linear model
linear.model = lm(price ~ item_condition_id + shipping + brand_name + category, train = trainF, na.action=na.omit)

################ TESTING

test[test == ''] = NA
# removing rows with column's na 
dim(test)
test1 = test[!with(test,is.na(name)& is.na(item_condition_id)&is.na(category_name)& 
                     is.na(brand_name)& is.na(shipping)& is.na(item_description)),]

dim(test1)

sum(is.na(test1$category_name))
sum(is.na(test1$item_condition_id))
sum(is.na(test1$brand_name))
sum(is.na(test1$shipping))
sum(is.na(test1$name))
sum(is.na(test1$item_description))

# changing class 
sapply(test1, class)
test1$item_condition_id = as.factor(test1$item_condition_id)
test1$category_name = as.factor(test1$category_name)
test1$brand_name = as.factor(test1$brand_name)
test1$shipping = as.factor(test1$shipping)
test1$name = as.character(test1$name)
test1$item_description = as.character(test1$item_description)
sapply(test1, class)

# category
test1$category = sub('.*/', '', test1$category_name)
for (i in 1:nrow(test1)){
  if(is.na(test1[i,"category"]))
  {
    test1 [i,"category"] = str_extract(test1$name, paste(uniqCategory, collapse="|"))
	test1 [i,"category"] = str_extract(test1$item_description, paste(uniqCategory, collapse="|"))
  }
}

sum(is.na(test1$category))  

# brand_name
for (i in 1:nrow(test1)){
  if(is.na(test1[i,"brand_name"]))
  {
    test1 [i,"brand_name"] = str_extract(test1$name, paste(uniBrand, collapse="|"))
	test1 [i,"brand_name"] = str_extract(test1$item_description, paste(uniBrand, collapse="|"))
  }
}

col = c('item_condition_id', 'shipping', 'brand_name','category')
test1[col] = lapply(test1[col], factor)

# item_condition_id no NA

# shipping No NA

testF = test1[,c('category', 'brand_name', 'shipping','item_condition_id', 'test_id')]

# fit$xlevels[["y"]] <- union(fit$xlevels[["y"]], levels(test[["y"]])) 

predicted = predict(linear.model, testF, na.rm=TRUE)
testF$price = predicted

final = testF[,c('test_id','price')]

testNA = test[with(test,is.na(name)& is.na(item_condition_id)&is.na(category_name)& 
                      is.na(brand_name)& is.na(shipping)& is.na(item_description)),]
aNA = testNA %>% select(test_id, item_condition_id,shipping,brand_name,category_name) %>% 
  setNames(c("test_id","item_condition_id","shipping","brand_name","category"))
  
aNA$price = 0
final1 = aNA[,c('test_id','price')]

final2 = rbind(final, final1)
final3 = final2[with(final2, order(test_id)), ]
final3

final3.to_csv('lost.csv',index = F)


