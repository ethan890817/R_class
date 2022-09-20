

library(datasets)
data(iris)
head(iris)
fix(iris)

students<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/students.txt',header=T, sep="\t", dec='.') # read data set from url
students$height
students[2,]
students[1:10,]

f <- students$gender=="female"
females <- students[f,]
rownames(females)<-c('Vanessa', 'Vicky', 'Michelle', 'Joyce', 'Victoria')

#
s <- iris$Species=="setosa"
ve <- iris$Species=="versicolor"
vi <- iris$Species=="virginica"
setosa <- iris[s,]
versicolor <- iris[ve,]
virginica <- iris[vi,]
str(setosa)
sf <- sample(1:nrow(females),2)
females[sf,]
ind1 <- order(-students$height)
students[ind1,]

colors <- ifelse(students$gender=="male", "blue", "red")
students$color <- ifelse(students$gender=="male", "blue", "red")
students$gender <- ifelse(students$gender=="male", "green", "yellow")

iris$colors <- ifelse(iris$Species=="setosa","purple",ifelse(iris$Species=="versicolor", "blue", "pink"))
iris$colors

iris[order(-iris$Sepal.Width),]
versicolor2 <- iris[ve,]
iris$colors <- NULL
