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
  filter(author == "Rollo et al.") %>%
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



##Study by Horsny et al.

Horsny <- main_data %>%
  filter(author == "Horsny et al.") %>%
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

P9 <- ggplot(Horsny, aes(x = antimicrobial, 
                        y = percent_resistant, 
                        fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of campylobacter spp from organic turkey farms in Germany",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Horsny et al.") + 
  facet_wrap(~strain)+
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P9


##study by Econoumou et al.

Economou <- main_data %>%
  filter(author == "Economou et al.") %>%
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

P10 <- ggplot(Economou, aes(x = antimicrobial, 
                        y = percent_resistant, 
                        fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of campylobacter spp from chicken meat samples from conventional farms in Greece",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Economou et al.") + 
  facet_wrap(~strain, nrow = 2)
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P10

##study by Tifosky et al.

Tifosky <- main_data %>%
  filter(author == "Tikofsky et al.") %>%
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

P11 <- ggplot(Tifosky, aes(x = antimicrobial, 
                         y = percent_resistant, 
                         fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of S.aureus spp from milk samples from conventional and organic farms in the USA",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Tifosky et al")
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P11

## study by Luangtongkum et al.

Luangtongkum <- main_data %>%
  filter(author == "Luangtongkum et al.") %>%
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

P12 <- ggplot(Luangtongkum, aes(x = antimicrobial, 
                           y = percent_resistant, 
                           fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of campylobacter spp from chicken meat samples from conventional and organic farms in the USA",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Luangtongkum et al")
theme_bw() +
  facet_wrap(~strain)
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P12

##study by Musa et al.
Musa <- main_data %>%
  filter(author == "Musa et al.") %>%
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

P13 <- ggplot(Musa, aes(x = antimicrobial, 
                                y = percent_resistant, 
                                fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli from cloacal and skin chicken samples from conventional and organic farms in Italy",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Musa et al")+
theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P13

#Study by kim et al in korea

Kim <- main_data %>%
  filter(author == "Kim et al.", country == "South Korea") %>%
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

P14 <- ggplot(Kim, aes(x = antimicrobial, 
                        y = percent_resistant, 
                        fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of Enterococcus from Chicken meat samples from conventional and organic farms in S.korea",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "kim et al, S.korea")+
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P14

#Tadesse et al study

Tadesse <- main_data %>%
  filter(author == "Tadesse et al") %>%
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

P15 <- ggplot(Tadesse, aes(x = antimicrobial, 
                       y = percent_resistant, 
                       fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of camylobacter in pigs from conventional and organic farms Wisconsin",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Tadesse at al")+
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P15

##Sudy by Schwaiger et al.

Schwaiger <- main_data %>%
  filter(author == "Schwaiger et al.") %>%
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


P16 <- ggplot(Schwaiger, aes(x = antimicrobial, 
                           y = percent_resistant, 
                           fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli in chicken cloacal swabs from conventional and organic farms in Bavaria",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Schwaiger at al")+
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P16

#study by sanchez et al.

Sanchez <- main_data %>%
  filter(author == "Sanchez et al") %>%
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


P17 <- ggplot(Sanchez, aes(x = antimicrobial, 
                             y = percent_resistant, 
                             fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli in chicken meat isolates from conventional and organic farms in California",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Sanchez at al")+
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P17

## Study by Kim, USA

Kim1 <- main_data %>%
  filter(author == "Kim et al.", country == "United States of America") %>%
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


P18 <- ggplot(Kim1, aes(x = antimicrobial, 
                           y = percent_resistant, 
                           fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli in chicken ceacal isolates from conventional and organic farms in USA",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Kim at al, USA")+
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P18

#Study by Kilonzo-Nthenge


Kilonzo <- main_data %>%
  filter(author == "Kilonzo-Nthenge et al") %>%
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


P19 <- ggplot(Kilonzo, aes(x = antimicrobial, 
                        y = percent_resistant, 
                        fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of Enterococcus in chicken meat isolates from conventional and organic farms in Tennessee",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Kilonzo-Nthenge at al, USA")+
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P19

##study by Bailley

Bailley <- main_data %>%
  filter(author == "Bailley et al") %>%
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


P20 <- ggplot(Bailley, aes(x = antimicrobial, 
                           y = percent_resistant, 
                           fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of salmonella in chicken fecal isolates from conventional and organic farms in USA",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Bailley et al")+
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P20

##Study by Lestari et al.

Lestari <- main_data %>%
  filter(author == "Lestari et al.") %>%
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


P21 <- ggplot(Lestari, aes(x = antimicrobial, 
                           y = percent_resistant, 
                           fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of salmonella in chicken carcas isolates from conventional and organic farms in USA",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Lestari et al")+
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P21

## study by Helbert et al

Helbert <- main_data %>%
  filter(author == "Helbert et al.") %>%
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


P22 <- ggplot(Helbert, aes(x = antimicrobial, 
                           y = percent_resistant, 
                           fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of campylobacter spp in cattle feacal isolates from conventional and organic farms in USA",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Helbert et al")+
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P22

##study by Gebreyes et al

Gebreyes <- main_data %>%
  filter(author == "Gebreyes et al.") %>%
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


P23 <- ggplot(Gebreyes, aes(x = antimicrobial, 
                           y = percent_resistant, 
                           fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of salmonella in pig's feacal isolates from conventional and organic farms in USA",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Gebreyes et al")+
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P23

##study by Miranda et al

Miranda <- main_data %>%
  filter(author == "Miranda et al") %>%
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


P24 <- ggplot(Miranda, aes(x = antimicrobial, 
                            y = percent_resistant, 
                            fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of enterococcus in chicken from conventional and organic farms in Spain",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Miranda et al")+
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P24


##study by Thakur et al.

Thakur <- main_data %>%
  filter(author == "Thakur et al.") %>%
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


P25 <- ggplot(Thakur, aes(x = antimicrobial, 
                           y = percent_resistant, 
                           fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of campylobacter spp in pig feacal samples from conventional and organic farms in USA",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Thakur et al")+
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P25

##study by Ray et al

Ray <- main_data %>%
  filter(author == "Ray et al") %>%
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


P26 <- ggplot(Ray, aes(x = antimicrobial, 
                          y = percent_resistant, 
                          fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of salmonella spp in Cattle feacal samples from conventional and organic farms in USA",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Ray et al")+
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P26

##Study by Much

Much <- main_data %>%
  filter(author == "Much et al") %>%
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


P27 <- ggplot(Much, aes(x = antimicrobial, 
                       y = percent_resistant, 
                       fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli in Chicken caecal samples from conventional and organic farms in Austria",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Much et al")+
  facet_wrap(~sampling_start_date, nrow = 3)
  theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P27

##study by Siemon et al

Siemon <- main_data %>%
  filter(author == "Siemon et al.") %>%
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


P28 <- ggplot(Siemon, aes(x = antimicrobial, 
                        y = percent_resistant, 
                        fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of salmonella in chicken feacal samples from conventional and organic farms in USA",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Siemon et al")+
  facet_wrap(~sampling_start_date, nrow = 3)
theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P28

##study by Cui et al


Cui <- main_data %>%
  filter(author == "Cui et al.") %>%
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


P29 <- ggplot(Cui, aes(x = antimicrobial, 
                          y = percent_resistant, 
                          fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of Campylobacter in chicken meat samples from conventional and organic farms in USA",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Cui et al")+
  facet_wrap(~strain)
theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P29

##study by Kassem et al

Kassem <- main_data %>%
  filter(author == "Kassem et al.") %>%
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


P30 <- ggplot(Kassem, aes(x = antimicrobial, 
                       y = percent_resistant, 
                       fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of Campylobacter in chicken  from conventional and organic farms in USA",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type", caption = "Kassem et al")+
theme_bw() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P30