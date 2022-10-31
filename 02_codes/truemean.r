library(tidyverse)

library(dplyr)
#Load data
main_data <- read_csv("conv.csv")




data1 <- main_data%>%
  mutate(
    continent =
      ifelse(continent == "Oceania", 
             "New Zealand", continent
      )
  )





data1 <- main_data %>%
  group_by(country) %>%
  select("continent",
         "doi",
         "farm_type",
         "host",
         "antimicrobial_compound",
         "antimicrobial_class",
         "pathogen",
         "sampling_end_date",
         "who_classification",
         "antimicrobial",
         "percent_resistant",
         "no_isolates_resistant",
         "no_isolates_susceptible",
         "no_isolates_intermidiate",
         "no_isolates" ) 

#changing percentage resistance into numeric class and rounding off  
data1$percent_resistant <- as.numeric(as.character
                                      (data1$percent_resistant))

data1$percent_resistant <- round(data1$percent_resistant, digits = 0)

data2 <- data1 %>%
  select("doi",
         "continent",
         "country",
         "farm_type",
         "host",
         "antimicrobial",
         "antimicrobial_compound",
         "antimicrobial_class",
         "who_classification",
         "pathogen",
         "percent_resistant",
         "no_isolates_resistant",
         "no_isolates_susceptible",
         "no_isolates_intermidiate",
         "no_isolates",
         "sampling_end_date")
        
         
         






data3 <- data2 %>%
  filter(pathogen != "Arcanobacterium pyogenes", pathogen != "Corynebacterium bovis",
         pathogen != "Environmental streptococci")


#changing percentage resistance into numeric class and rounding off  
data3$percent_resistant <- as.numeric(as.character
                                      (data3$percent_resistant))

data3$percent_resistant <- round(data3$percent_resistant, digits = 0)


s1<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1016/j.prevetmed.2019.104755") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
            probRes =  totRes /(totInt + totSus+totRes)) %>%
                  dplyr::mutate(meanRes = sum(1*probRes)) %>%
                  dplyr::mutate(mean = round(meanRes *100, 0))%>%
                  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s2<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "www.researchgate.net/publication/288728634") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s3<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "http://dx.doi.org/10.1155/2015/618752") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s4<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "PMID: 22468028") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()




s5<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1016/j.ijfoodmicro.2013.01.020") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()

#######################################################

s6<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi: 10.3389/fvets.2021.607491") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()





s7<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi: 10.1371/journal.pone.0157049") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s8<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "https://doi.org/10.3389/fmicb.2017.00955") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()

s9<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1111/j.1863-2378.2008.01151.x") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s10<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1111/j.1863-2378.2008.01229.x") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s11<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.2376/0005-9366-127-135") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()

s12<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.3382/ps/pev259") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s13<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi.org/10.3168/jds.2017-12939") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s14<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "https://doi.org/10.1016/j.foodcont.2015.03.022") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s15<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1016/j.psj.2019.10.027") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()

s16<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi: 10.1371/journal.pone.0157049") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s17<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi: 10.3390/ani10071215") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s18<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi.org/10.1016/j.ijfoodmicro.2019.108391") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s19<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi.org/10.3390/antibiotics10111321") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s20<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi: 10.3390/antibiotics9100701") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s21<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1016/j.crmicr.2021.100088") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s22<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "https://doi.org/10.1080/00480169.2008.36801") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s23<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "Pdoi: 10.17221/8721-VETMED") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s24<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "PMID: 24053020") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s25<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi: 10.3382/ps.2013-03729.") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s26<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1016/j.vetmic.2015.05.005") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s27<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.3382/ps/pex275") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s28<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "dx.doi.org/ 10.3382/ps.2013-03175") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s29<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1016/j.meatsci.2009.01.020") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s30<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.4315/0362-028x-70.4.1021") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s31<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.4315/0362-028x-71.12.2537") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s32<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi.org/10.1016/j.foodcont.2012.06.005") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s33<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi.org/10.1080/11358120902907014") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s34<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi: 10.1371/journal.pone.0157049") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s35<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.3390/antibiotics9110834") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s36<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi.org/10.3390/pathogens10121630") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s37<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "https://doi.org/10.3389/fmicb.2017.00955") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s38<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi.org/10.3168/jds.S0022-0302(06)72164-6") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s39<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi.org/10.5713/ab.21.0232") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s40<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "https://doi.org/10.5713/ajas.2010.90345") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s41<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1111/jfs.12588") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s42<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.9775/kvfd.2019.23638") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s43<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1016/j.ijantimicag.2005.09.020") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s44<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1016/j.scitotenv.2013.12.005") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s45<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1089/107662903322541883") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s46<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1089/fpd.2010.0566") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s47<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1089/fpd.2010.0665") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s48<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1089/fpd.2011.0852") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s49<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1089/fpd.2016.2161") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s50<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1089=fpd.2008.0171") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s51<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1089=fpd.2010.0632") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s52<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1128/AEM.01419-13") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s53<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi: 10.1128/AEM.07723-11") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s54<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi: 10.1128/AEM.71.7.4108-4111.2005") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s55<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1128/AEM.72.5.3600-3607.2006") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s56<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1289/ehp.1003350") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s57<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.1637/0005-2086(2007)051[0112:CIOPAA]2.0.C") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s58<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.2460/javma.228.7.1074") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s59<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.2460/javma.231.2.275") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s60<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.2460/javma.236.2.201") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()



s61<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.3168/jds.S0022-0302(06)72271-8") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s62<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.3390/ani10122217") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s63<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.4315/0362-028x-67.7.1433") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s64<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.4315/0362-028x-68.11.2402") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s65<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi: 10.4315/0362-028x-69.3.482.") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s66<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.4315/0362-028x-69.4.743") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s67<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.4315/0362-028x-72.6.1165") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()

s68<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.4315/0362-028X.JFP-14-322") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()

s69<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "DOI: 10.4315/0362-028X.JFP-19-269") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s70<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi:10.2134/jeq2015.05.0245") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s71<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi.org/10.1016/j.envres.2021.110954") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s72<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "doi.org/10.1016/j.foodcont.2021.108738") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()

s73<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "https://doi.org/10.1016/j.japr.2021.100158") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()


s74<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "https://doi.org/10.1111/j.1365-2672.2007.03681.x") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()




s75<- data3 %>%
  dplyr::group_by(doi, farm_type,  antimicrobial_compound)%>%
  dplyr::filter(doi == "https://doi.org/10.2460/javma.2005.226.589") %>%
  dplyr::mutate(totRes = sum(no_isolates_resistant, na.rm = TRUE),
                totInt = sum(no_isolates_intermidiate, na.rm = T),
                totSus = sum(no_isolates_susceptible), na.rm = T,
                N = sum(totSus + totInt),
                probRes =  totRes /(totInt + totSus+totRes)) %>%
  dplyr::mutate(meanRes = sum(1*probRes)) %>%
  dplyr::mutate(mean = round(meanRes *100, 0))%>%
  dplyr::mutate (variance = sum((1^2)*meanRes) - (meanRes^2)) %>%
  dplyr::mutate (stdevD = sqrt(variance))%>%
  dplyr::select(continent, country, farm_type, host, antimicrobial,
                antimicrobial_compound, antimicrobial_class, who_classification,
                mean, percent_resistant,pathogen,variance, stdevD, totRes, totInt, totSus,N,
                sampling_end_date) %>%
  distinct() %>% as.data.frame()







#########################################################
mean.data <- rbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10,
            s11, s12, s13, s14, s15, s16, s17, s18, s19, s20,
            s21, s22, s23, s24, s25, s26, s27, s28, s29, s30,
            s31, s32, s33, s34, s35, s36, s37, s38, s39, s40,
            s41, s42, s43, s44, s45, s46, s47, s48, s49, s50,
            s51, s52, s53, s54, s55, s56, s57, s58, s59, s60,
            s61, s62, s63, s64, s65, s66, s67, s68, s69, s70,
            s71, s72, s73, s74)




mean.data1 <- mean.data%>%
  filter(mean<=100)



###Exporting data to csv file in the conv folder
write.table(mean.data1, file = "mean.data.csv", row.names = FALSE,
            sep = ",")
