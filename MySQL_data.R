library("RMySQL")
chembl = dbConnect(MySQL(), user='root', password='liuchao', dbname='chembl_22', host='127.0.0.1')
dbListTables(chembl)
dbListFields(chembl,'compound_properties')
#fetch compound data
compound_properties<-dbSendQuery(chembl,"select * from compound_properties")
compound_properties<-fetch(compound_properties,n=Inf)
head(compound_properties)
#fetch label data
DRUG_MECHANISM<-dbSendQuery(chembl,"select * from drug_mechanism")
DRUG_MECHANISM<-fetch(DRUG_MECHANISM,n=Inf)
DRUG_MECHANISM<-DRUG_MECHANISM[order(DRUG_MECHANISM[,3]),]
DRUG_MECHANISM<-DRUG_MECHANISM[!duplicated(DRUG_MECHANISM[,3]),]
head(DRUG_MECHANISM)
compound_name<-DRUG_MECHANISM$molregno
compound_name
#select rigt compound corresponding to label
compound_properties<-compound_properties[compound_name,]
head(compound_properties)
#creat dataset
dataset<-cbind(DRUG_MECHANISM$direct_interaction,compound_properties)
colnames(dataset)[1]<-"label"
head(dataset)

dataset<-dataset[,-grep("full_molformula",colnames(dataset))]
dataset<-dataset[,-grep("ro3_pass",colnames(dataset))]


#assign value to the char class in properties
as.numeric(dataset[,14])
for(x in 2:2892){
  if(dataset[x,14]=='NEUTRAL') dataset[x,14]<-1
}

#normaliation
maxs<-apply(dataset,2,max)
mins<-apply(dataset,2,min)
dataset<-as.data.frame(scale(dataset,center = mins,scale = maxs-mins))

#creat negative sample
negative<-dataset[sample(nrow(dataset)),]
dataset<-rbind(negative,dataset)
dim(dataset)

#normalization?
#build model

library('h2o')
h2o.init()
