## Download + subset global OBIS data
## Date created: 10 Mar 2026
## Date updated: 23 Mar 2026


# download global OBIS data for <100 m (in 1-10 GB chunks, subset by year)
# subset data for marine inverts + fish within 50 km of shore

library(robis)


#data_chunk1 = occurrence(enddepth = 100, verbose = T,
#                         enddate="2000-12-31")
#write.csv(data_chunk1,file="OBIS_data_1900-2000.csv")

data_chunk1_sub = subset(data_chunk1, 
                         data_chunk1$marine==T & 
                           data_chunk1$kingdom == "Animalia" & 
                           data_chunk1$shoredistance > 0 & 
                           data_chunk1$shoredistance < 500000)

data_chunk1_sub2 = subset(data_chunk1_sub, 
                          data_chunk1_sub$phylum != "Chordata" |
                            data_chunk1_sub$superclass == "Pisces")

#write.csv(data_chunk1_sub2,file="OBIS_animals_1900-2000.csv")

#data_chunk2 = occurrence(enddepth = 100, verbose = T,
#                         startdate = "2001-01-01", enddate="2012-12-31")
#write.csv(data_chunk2,file="OBIS_data_2001-2012.csv")

#data_chunk2 = read.csv("OBIS_animals_2001-2012.csv")

data_chunk2_sub = subset(data_chunk2, 
                         data_chunk2$phylum != "Chordata" |
                           data_chunk2$superclass == "Pisces")

data_chunk2_sub2 = subset(data_chunk2_sub, 
                          data_chunk2_sub$marine==T & 
                            data_chunk2_sub$kingdom == "Animalia" & 
                            data_chunk2_sub$shoredistance > 0 & 
                            data_chunk2_sub$shoredistance < 500000)

#write.csv(data_chunk2_sub2,file="OBIS_animals_2001-2012.csv")

#data_chunk3 = occurrence(enddepth = 100, verbose = T,
#                         startdate = "2013-01-01", enddate="2016-12-31")
#write.csv(data_chunk3,file="OBIS_data_2013-2016.csv")

data_chunk3_sub = subset(data_chunk3, 
                         data_chunk3$marine==T & 
                           data_chunk3$kingdom == "Animalia" & 
                           data_chunk3$shoredistance > 0 & 
                           data_chunk3$shoredistance < 500000)

data_chunk3_sub2 = subset(data_chunk3_sub, 
                          data_chunk3_sub$phylum != "Chordata" |
                            data_chunk3_sub$superclass == "Pisces")

#write.csv(data_chunk3_sub2,file="OBIS_animals_2012-2016.csv")


data_chunk4 = occurrence(enddepth = 100, verbose = T,
                         startdate = "2017-01-01",enddate = "2017-12-31")
#write.csv(data_chunk4,file="OBIS_data_2017.csv")

data_chunk4_sub = subset(data_chunk4, 
                         data_chunk4$marine==T & 
                           data_chunk4$kingdom == "Animalia" & 
                           data_chunk4$shoredistance > 0 & 
                           data_chunk4$shoredistance < 500000)

data_chunk4_sub2 = subset(data_chunk4_sub, 
                          data_chunk4_sub$phylum != "Chordata" |
                            data_chunk4_sub$superclass == "Pisces")


#write.csv(data_chunk4_sub2,file="OBIS_animals_2017.csv")


data_chunk5 = occurrence(enddepth = 100, verbose = T,
                         startdate = "2018-01-01",enddate = "2019-12-31")
#write.csv(data_chunk5,file="OBIS_data_2018-2019.csv")

data_chunk5_sub = subset(data_chunk5, 
                         data_chunk5$marine==T & 
                           data_chunk5$kingdom == "Animalia" & 
                           data_chunk5$shoredistance > 0 & 
                           data_chunk5$shoredistance < 500000)

data_chunk5_sub2 = subset(data_chunk5_sub, 
                          data_chunk5_sub$phylum != "Chordata" |
                            data_chunk5_sub$superclass == "Pisces")

#write.csv(data_chunk5_sub2,file="OBIS_animals_2018-2019.csv")



data_chunk6 = occurrence(enddepth = 100, verbose = T,
                         startdate = "2020-01-01",enddate = "2022-12-31")
#write.csv(data_chunk6,file="OBIS_data_2020-2022.csv")

data_chunk6_sub = subset(data_chunk6, 
                         data_chunk6$marine==T & 
                           data_chunk6$kingdom == "Animalia" & 
                           data_chunk6$shoredistance > 0 & 
                           data_chunk6$shoredistance < 500000)

data_chunk6_sub2 = subset(data_chunk6_sub, 
                          data_chunk6_sub$phylum != "Chordata" |
                            data_chunk6_sub$superclass == "Pisces")

#write.csv(data_chunk6_sub2,file="OBIS_animals_2020-2022.csv")


data_chunk7 = occurrence(enddepth = 100, verbose = T,
                         startdate = "2023-01-01")
#write.csv(data_chunk7,file="OBIS_data_2023-MAR2026.csv")

data_chunk7_sub = subset(data_chunk7, 
                         data_chunk7$marine==T & 
                           data_chunk7$kingdom == "Animalia" & 
                           data_chunk7$shoredistance > 0 & 
                           data_chunk7$shoredistance < 500000)

data_chunk7_sub2 = subset(data_chunk7_sub, 
                          data_chunk7_sub$phylum != "Chordata" |
                            data_chunk7_sub$superclass == "Pisces")

#write.csv(data_chunk7_sub2,file="OBIS_animals_2023-MAR2026.csv")

