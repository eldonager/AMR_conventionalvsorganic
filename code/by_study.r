library(tidyverse)
library(ggplot2)

#Load data
main_data <- read_csv("conv.csv", na = "empty")

#changing percentage resistance into numeric class and rounding off  
main_data$percent_resistant <- as.numeric(as.character
                                          (main_data$percent_resistant))

main_data$percent_resistant <- round(main_data$percent_resistant, digits = 2)

#study by Kenji et al.
kenji <- main_data %>%
filter(author == "Kenji et al.") %>%
  select("country",
         "iso_3",
         "locality",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "percent_intermediate",
         "percent_susceptible",
         "no_isolates",
         "sampling_start_date", 
         "sampling_end_date"
         )

P1 <- ggplot(kenji, aes(x = antimicrobial, 
                        y = percent_resistant, 
                         fill = farm_type
                        )
             ) +
          geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli from feacal sample of dairy cattle in Wisconsin",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Kenji et al.") + 
      theme_bw() + 
       theme_classic() + 
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P1

# study by Tamang et al

Tamang <- main_data %>%
  filter(author == "Tamang et al.") %>%
  select("country",
         "iso_3",
         "locality",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "percent_intermediate",
         "percent_susceptible",
         "no_isolates",
         "sampling_start_date", 
         "sampling_end_date"
  )

P2 <- ggplot(Tamang, aes(x = antimicrobial, 
                        y = percent_resistant, 
                        fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of salmonella from feacal sample of pigs in South Korea",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Tamang et al.") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P2

#study by Suriya

suriya <- main_data %>%
  filter(author == "Suriyasathaporn") %>%
  select("country",
         "iso_3",
         "locality",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "percent_intermediate",
         "percent_susceptible",
         "no_isolates",
         "sampling_start_date", 
         "sampling_end_date"
  )

P3 <- ggplot(suriya, aes(x = antimicrobial, 
                        y = percent_resistant, 
                        fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of pathogens from samples from dairy farms in Thailand",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Suriyasathaporn et al.") +
  facet_wrap(~pathogen, ncol = 4)+
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P3

#Study by Omega.


Omega <- main_data %>%
  filter(author == "Omega et al.") %>%
  select("country",
         "iso_3",
         "locality",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "percent_intermediate",
         "percent_susceptible",
         "no_isolates",
         "sampling_start_date", 
         "sampling_end_date"
  )

P4 <- ggplot(Omega, aes(x = antimicrobial, 
                         y = percent_resistant, 
                         fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli from soil samples in Conventional vs organic farms in New Zealand in 2017 and 2018",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Omega et al.") + 
  facet_wrap(~sampling_start_date)
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P4

#study by sjostrom
Sjostrom <- main_data %>%
filter(author == "Sjostrom et al.") %>%
  select("country",
         "iso_3",
         "locality",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "percent_intermediate",
         "percent_susceptible",
         "no_isolates",
         "sampling_start_date", 
         "sampling_end_date"
  )

P5 <- ggplot(Sjostrom, aes(x = antimicrobial_compound, 
                        y = percent_resistant, 
                        fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli from fecal and manure samples in Conventional vs organic farms in Sweden",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Sjostrom et al.") + 
  facet_wrap(~host)
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P5

##Study by Kempf

Kempf <- main_data %>%
  filter(author == "Kempf et al.") %>%
  select("country",
         "iso_3",
         "locality",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "percent_intermediate",
         "percent_susceptible",
         "no_isolates",
         "sampling_start_date", 
         "sampling_end_date"
  )

P6 <- ggplot(Kempf, aes(x = antimicrobial, 
                         y = percent_resistant, 
                         fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of C.coli from feacal and colon sample of pigs in France",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Kempf et al.") + 
  facet_wrap(~sample_type)+
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P6

##Study by Osterberg
Osterberg <- main_data %>%
  filter(author == "Osterberg et al.") %>%
  select("country",
         "iso_3",
         "locality",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "percent_intermediate",
         "percent_susceptible",
         "no_isolates",
         "sampling_start_date", 
         "sampling_end_date"
  )

P7 <- ggplot(Osterberg, aes(x = antimicrobial, 
                        y = percent_resistant, 
                        fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli from feacal and colon sample of pigs in Denmark, France, Italy and Sweden in 2012 1nd 2013",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Osterberg et al.") + 
  facet_wrap(~country)+
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P7

#Study by Rollo et al.

Rollo <- main_data %>%
  filter(author == "Omega et al.") %>%
  select("country",
         "iso_3",
         "locality",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "percent_intermediate",
         "percent_susceptible",
         "no_isolates",
         "sampling_start_date", 
         "sampling_end_date"
  )

P8 <- ggplot(Rollo, aes(x = antimicrobial, 
                        y = percent_resistant, 
                        fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli from soil samples in Conventional vs organic farms in New Zealand in 2017 and 2018",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Rollo et al.") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P8

