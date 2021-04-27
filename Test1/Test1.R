Sample<- read.table('Test1_var6.csv',header = TRUE,sep =",");
Sample<-data.frame(Output=Sample$Output,Treatment=as.factor(Sample$Treatment));
head(Sample)

g <- lm(Output ~ Treatment, Sample)
anova(g)

gi <- lm(Output ~ Treatment-1, Sample)
anova(gi)
