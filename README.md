# perturbation

Table 1. Indicators, their description and sources.

| Higher-level indicator | Univariate spatial layer                                                                                                                                                          | Rationale for inclusion                                                                                                                                                                                                                                                                                                              | References                                                                                                                                                                    | Spatial layer details                                                                                                                                                                                                                                                                                                                     | Spatial layer source                                                                                               |
|------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------|
| Landscape change       | Anthropogenic stressor intensity: 1) Built up area; 2) Energy production and mining; 3) Transportation corridors; 4) Agriculture and harvest and 5) Human intrusion and pollution | Coronavirus shedding may be higher in human-dominated areas.                                                                                                                                                                                                                                                                         | https://doi.org/10.1093/ve/vex012                                                                                                                                             | This layer summarizes land use intensity by human modification in 2017 by1)  urban and built-up area, 2) energy production and mining, 3) transportation corridors, 4) agriculture and harvest of natural areas and 5) human intrusion and pollution (~1km).                                                                              | https://zenodo.org/record/3901815#.YvRsgXYlubg                                                                     |
| Landscape change       | Forest quality                                                                                                                                                                    | Emerging infectious disease risk is elevated in forested tropical regions experiencing land-use changes and where wildlife biodiversity (mammal species richness) is high.                                                                                                                                                           | https://www.nature.com/articles/s41467-017-00923-8                                                                                                                            | Forest quality, where highest values inticate highest quality  (low=0, high=10) for 2019 (1 km).                                                                                                                                                                                                                                          | https://www.nature.com/articles/s41467-020-19493-3                                                                 |
| Landscape change       | Transition potential (risk of cover loss based on past threats)                                                                                                                   | Theory on land-use induced spillover; Agricultural land-uses exacerbate many infectious diseases in Southeast Asia (malaria, Schistosomiasis, Spotted fever, hookworms).                                                                                                                                                             | https://www.nature.com/articles/s41467-019-12333-z ; https://www.sciencedirect.com/science/article/pii/S2542519621000310 ; https://www.nature.com/articles/s43016-021-00285-x | Regional (continental) models inform the risk of a forest becoming removed in the future, based on neural network models using historical data (2001-2014) (Hewson et al. 2019) from low (0) to high risk (1). Here we use continental models and not global, because regional model had better performance than the global model (~1 km) | https://futureclimates.conservation.org/riskstreecoverloss.html                                                    |
| Secondary host         | Pigs                                                                                                                                                                              | Covs with origing tracing to bats causing disease in pigs                                                                                                                                                                                                                                                                            | https://doi.org/10.1038/s41586-018-0010-9                                                                                                                                     | We used Areal-weighted GLW model (6_Pg_2010_Aw.tif) from Gilbert's livestock of the world estimates for 2010 (~10 km).                                                                                                                                                                                                                    | https://www.nature.com/articles/sdata2018227                                                                       |
| Secondary host         | Cattle                                                                                                                                                                            | Recent evidence from Germany                                                                                                                                                                                                                                                                                                         | https://wwwnc.cdc.gov/eid/article/28/9/22-0125_article                                                                                                                        | We used Areal-weighted GLW model (6_Ct_2010_Aw.tif) from Gilberts livestock of the world estimates for 2010 (~10 km).                                                                                                                                                                                                                     | https://www.nature.com/articles/sdata20182277                                                                      |
| Secondary host         | Wild mammals                                                                                                                                                                      | EID risk is elevated in forested tropical regions experiencing land-use changes and where wildlife biodiversity (mammal species richness) is high.                                                                                                                                                                                   | https://www.nature.com/articles/s41467-017-00923-8                                                                                                                            | IUCN data (~30 km), Search on 2022-04-04 at 21:38:52,  Mollweide projection was warped to WGS84 using the mode to fill in distortions.                                                                                                                                                                                                    | https://www.iucnredlist.org/resources/other-spatial-downloads#SR_2021_3                                            |
| Primary host           | Bat hosts                                                                                                                                                                         | Peak of sarbecovirus hosts in Asia; Both the evolutionary and ecological aspects of emergence risk are higher in southeast Asia—a fact that will only become more relevant, as bats track shifting climates and exchange viruses with other species, creating a hotspot of elevated cross-species transmission unique to the region. | Muylaert et al. 2022 ; https://ecoevorxiv.org/8mgv6/                                                                                                                          | Average values used from the 2 sources. Sánchez data (1 km Areas of habitat) was resampled to match Muylaert et al. (2022) resolution (0.25 dd).                                                                                                                                                                                          | https://royalsocietypublishing.org/doi/10.1098/rspb.2022.0397 ; https://www.nature.com/articles/s41467-022-31860-w |
| Exposure - spread      | Population counts for 2020                                                                                                                                                        | Population size is a crucial transmissibility factor for SARS-like disease spread                                                                                                                                                                                                                                                    | Reju's work; https://www.nature.com/articles/s41598-021-97578-9 ; https://academic.oup.com/jtm/article/27/3/taaa038/5807719?login=false                                       | Worlpop unconstrained global mosaics of population counts for 2020 (1 km native resolution) was resampled to match working resolution of 0.25 dd                                                                                                                                                                                          | https://hub.worldpop.org/geodata/listing?id=64                                                                     |
| People vulnerability   | Travel time to health care                                                                                                                                                        | City remoteness (and hence access to healthcare) are key to understand zoonotic disease outbreaks                                                                                                                                                                                                                                    | https://www.science.org/doi/10.1126/sciadv.abo5774                                                                                                                            | Travel time to health care (motorized, in hours, 1km) was used                                                                                                                                                                                                                                                                            | https://www.nature.com/articles/s41591-020-1059-1                                                                  |
