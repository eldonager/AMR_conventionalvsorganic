library(metafor)
library(meta)


data1 <- read.csv("conv.csv", header=T, sep=",")

data2 <- data1 %>%
  select(author_year,farm_type, no_isolates_resistant, no_isolates)


data3 <- data2 %>%
  group_by(author_year) %>%
  mutate(percent_resistant = no_isolates_resistant/no_isolates*100)
 




data4 <- aggregate(cbind(no_isolates_resistant,no_isolates)~author_year+farm_type,data = data1, 
          FUN = sum)



###PLO for logit transformation
pes.summary=metaprop(no_isolates_resistant, no_isolates, 
                     author_year, data=data4, sm="PLO")
forest(pes.summary)


##pub ready forest plot
## method="DL" is random effects using the DerSimonian-Laird estimator
pes.summary=metaprop(no_isolates_resistant, no_isolates, author_year, data=data4, sm="PLO",
                     method.tau="DL", method.ci="NAsm")
forest(pes.summary,
       xlim=c(0,4),
       pscale=1000,
       rightcols=FALSE,
       leftcols=c("studlab", "event", "n", "effect", "ci"),
       leftlabs=c("Study", "Isolates resistant", "Total", "% resistance", "95% C.I."),
       xlab="Percentage resistance", smlab="",
       weight.study="random", squaresize=0.5, col.square="navy",
       col.square.lines="navy",
       col.diamond="maroon", col.diamond.lines="maroon",pooled.totals=FALSE,
       comb.fixed=FALSE,
       fs.hetstat=10,
       print.tau2=TRUE,
       print.Q=TRUE,
       print.pval.Q=TRUE,
       print.I2=TRUE,
       digits=2)









