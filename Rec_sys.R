
library('data.table')
library('tidyr')
library('dplyr')
library('ggplot2')
library('recommenderlab')


book=fread('cs_books.csv')
str(book)
unique(book$book)

book%>%
  group_by(book)%>%
  summarise(avg_rating=mean(rating, na.rm=T))

no_rating=book[,c(1,2)]

book_type<- unique(unlist(book$book))
class(book_type)


book_list=as.list(as.data.frame(t(book)))



###
spread_data=book%>%
  spread(book,rating)


#excluding user column
book_as_matrix=as.matrix(spread_data[,-1])

#creating suitable object for the package input
book_matrix_lab=as(book_as_matrix,'realRatingMatrix')
getRatingMatrix(book_matrix_lab)


recommenderRegistry$get_entry_names()

recommenderRegistry$get_entry("POPULAR", dataType="realRatingMatrix")
recommenderRegistry$get_entry("IBCF", dataType="realRatingMatrix")
recommenderRegistry$get_entry("UBCF", dataType="realRatingMatrix")

set.seed(800)
#splitting data in train80%/test20% datasets
split_df=evaluationScheme(book_matrix_lab, method='split', train=0.8, given=-1)

#learning recommender model, defined method is POPULAR
POPULAR_model=Recommender(getData(split_df,'train'),'POPULAR')

POPULAR_prdct=predict(POPULAR_model, getData(split_df,'known'), type='ratings')


####
rmse_popular <- calcPredictionAccuracy(POPULAR_prdct, 
                                       getData(split_df, "unknown"))[1]
rmse_popular
###

#UBCF
UBCF_model=Recommender(getData(split_df,'train'),'UBCF', param=list(normalize='center', method='Cosine', nn=25 ))

UBCF_prdct=predict(UBCF_model, getData(split_df,'known'), type='ratings')

image((UBCF_prdct[1:5,1:5]))

RMSE_UBCF=calcPredictionAccuracy(UBCF_prdct, 
                                 getData(split_df, "unknown"))[1]
RMSE_UBCF


###
IBCF_model=Recommender(getData(split_df,'train'),'IBCF',param=list(k=30, normalize='center',normalize_sim_matrix=F,alpha=0.5,na_as_zero=F))

IBCF_prdct=predict(IBCF_model, getData(split_df,'known'), type='ratings')

image((IBCF_prdct[1:5,1:5]))

RMSE_IBCF=calcPredictionAccuracy(IBCF_prdct, 
                                   getData(split_df, "unknown"))[1]
RMSE_IBCF
RMSE_Random
RMSE_UBCF
rmse_popular
#recommending books
as(POPULAR_prdct[1:10,], 'list')
as(IBCF_prdct[1:10,], 'list')
as(UBCF_prdct[1:10,], 'list')
as(Random_prdct[1:10,], 'list')

#predicting ratings
as(IBCF_prdct, 'matrix')[1:10,]
as(UBCF_prdct, 'matrix')[1:10,]
as(Random_prdct, 'matrix')[1:10,]
as(POPULAR_prdct, 'matrix')[1:10,]


####

book_user1=book%>%
  add_row(user=c('user1', 'user1', 'user1'), book=c('Formal methods','Programming language theory','Systems programming'), rating=c(3,5,1))

spread_data=book_user1%>%
  spread(book,rating)
book_as_matrix_user1=as.matrix(spread_data[,-1])

#creating suitable object for the package input
book_matrix_lab=as(book_as_matrix_user1,'realRatingMatrix')
getRatingMatrix(book_matrix_lab)
##book recommending//in each recommending system section the first three lines are 
##book recommendations. Turns out each system gives different weight to different book, 
##hence the next to lines are recommnended with possible ratings  user1 could give them
##POPULAR
pop_model=Recommender(book_matrix_lab, method='POPULAR')
recom_pop=predict(pop_model, book_matrix_lab[74])
as(recom_pop,'list')

pred=predict(pop_model,book_matrix_lab[74], type='ratings')
as(pred,'matrix')
###UBCF
UBCF_model=Recommender(book_matrix_lab, method='UBCF')
recom_UBCF=predict(UBCF_model, book_matrix_lab[74])
as(recom_UBCF,'list')

pred_UBCF=predict(UBCF_model,book_matrix_lab[74], type='ratings')
as(pred_UBCF,'matrix')
##IBCF
IBCF_model=Recommender(book_matrix_lab, method='IBCF')
recom_IBCF=predict(IBCF_model, book_matrix_lab[74])
as(recom_IBCF,'list')

pred_IBCF=predict(IBCF_model,book_matrix_lab[74], type='ratings')
as(pred_IBCF,'matrix')
