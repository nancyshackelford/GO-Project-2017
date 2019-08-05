# Ten years of pulling: Ecosystem recovery after long-term weed management in Garry oak savanna

## *Conservation Science and Practice,* in press

NAS 05-08-2019

### Database description --

This database includes all data and code used in the 2019 publication. The publication has full methods descriptions, including detailed information on survey timing, methods, and staff. Comments, feedback, and collaboration are welcome.

*** 

## Code files

All code is provided both as downloadable .R files and as markdown files for online viewing.

| Name | Description | 
| :---: | :--- |
| <b>Functional groups and FR calculation</b> | Uses species list and trait data to assign functional groups and calculate functional redundancy for each group within each site within each year (2007 and 2017). | 
| <b>Response diversity and turnover</b> | Uses species list and trait data to calculate response diversity in each site in each year (2007 and 2017); uses species list to calculate total turnover within each site between years. |
| <b>Data exploration</b> | Plots all response variables against predictors for visual exploration. | 
| <b>Statistical models</b> | Fits all models used in publication with published plots. |
| <b>Bootstrapping for Appendix 4</b> | Explores roll of non-random species losses and additions in functional redundancy results through simulated random species changes. | 

*** 

## Data-sets

All data is provided as comma separated files with accompanying meta-data descriptions.

| Name | Collector | Source | Description | 
| :---: | :---: | :--- | :--- |
| <b>Trait_data</b> | Sean M. Murray; Nancy Shackelford | multiple online sources (see publication for details) | all response and effect traits for species identified in all patches in 2007 and 2017 surveys |
| <b>Raw_site_data</b> | Patrick L. Lilley; Nancy Shackelford | field surveys | presence-absence data for all patches in 2007 and 2017 surveys |
| <b>Site_data</b> | Patrick L. Lilley; Nancy Shackelford; Sean M. Murray | site-level predictor and response data compiled from raw site data, trait data, and processing code | compiled dataset |
| <b>Quadrat_data</b> | Joseph R. Bennett; Sean M. Murray | field surveys | cover data for quadrat survey for all patches in 2009 and 2017. Quadrat number within each site depended on area of patch. Full data is unpublished; 2009 data is published in Bennett et al., 2013<sup>*</sup> |
| <b>Group_data</b> | Patrick L. Lilley; Nancy Shackelford; Sean M. Murray | group-level predictor and response data compiled from raw site data, trait data, and processing code | compiled dataset |

<sup>*</sup> Bennett, J. R., Vellend, M., Lilley, P. L., Cornwell, W. K., & Arcese, P. (2013). Abundance, rarity and invasion debt among exotic species in a patchy ecosystem. Biological Invasions, 15(3), 707-716.