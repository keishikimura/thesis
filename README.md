## Overview

The code in this replication package produces the analysis in the University of Chicago honors thesis "Breakthrough Inventions and the Geographic Concentration of AI Innovation" by Keishi Kimura.

## Data Availability and Provenance Statements

### Statement about Rights

I certify that the author of this thesis has legitimate access to and permission to use the data used in this thesis. 

### Summary of Availability

All data **are** publicly available.

### Details on each Data Source

| Data.Name  | Data.Files | Location | Provided | Citation | Link | Notes |
| -- | -- | -- | -- | -- | -- | -- | 
| “Artificial Intelligence Patent Dataset” | ai_model_predictions.tsv | data/raw/uspto/ | FALSE | Giczy et al. (2021) | [USPTO](https://www.uspto.gov/ip-policy/economic-research/research-datasets/artificial-intelligence-patent-dataset) | |
| “USPTO PatentsView Data” | g_application.tsv; g_gov_interest_org.tsv; g_inventor_disambiguated.tsv; g_location_disambiguated; g_patent.tsv; g_persistent_inventor.tsv; g_us_patent_citation.tsv | data/raw/uspto/ | FALSE | USPTO (2024) | [PatentsView](https://patentsview.org/download/data-download-tables) | Updated Feb. 2024 |
| “Startup Patenting Dataset” | _patent_ocpb_augmented.csv | data/raw/uspto/ | FALSE | Ewens and Marx (2023) | [Reliance on Science](https://zenodo.org/records/10250505) | |
| “Combined MSA CBSA FIPS County Crosswalk 2005, 2011-2017” | cbsatocountycrosswalk.csv | data/raw/nber/ | FALSE | U.S. Census (2017) | [NBER](https://www.nber.org/research/data/ssa-federal-information-processing-series-fips-core-based-statistical-area-cbsa-and-metropolitan-and)||
| “QCEW County-MSA-CSA Crosswalk, 2013-2022” | qcew-county-msa-csa-crosswalk.xlsx | data/raw/census/ | FALSE | U.S. Bureau of Labor Statistics (2022) | [BLS](https://www.bls.gov/cew/classifications/areas/county-msa-csa-crosswalk.htm)||
| “U.S. Census Regional Divisions” | regions.csv | data/raw/census/ | FALSE | U.S. Census (2014) | [Github](https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv)| |
| “Population in Metropolitan and Micropolitan Statistical Areas: 1990 and 2000” | tab05b.csv | data/raw/census/ | FALSE | U.S. Census (2003) | [U.S. Census](https://www.census.gov/data/tables/2000/dec/phc-t-29.html)| Table 5b|

Rename and save each file in its respective directory.

## Generated Datasets

| Data file | Code | Provided | 
|-----------|--------|----------|
| `AI/data/generated/AIpatentCBSA.csv` | `01_patents.R` | No |
| `AI/data/generated/AIpatentCSA.csv` | `01_patents.R` | No |
| `AI/data/generated/AIpatentMSA.csv`| `01_patents.R` | No |
| `AI/data/generated/populationMSA.csv`| `02_population.R` | No |
| `AI/data/generated/region_crosswalk.csv`| `03_regions.R` | No |
| `AI/data/generated/citation_outside.csv`| `04_citations.R` | No |
| `AI/data/generated/cum_citations_outside.csv`| `05_citations2.R` | No |

## Computational Requirements

### Software Requirements

- R 4.3.2
  - Run `renv::restore()` in the R console to restore the project library locally after packages automatically install. For more information on `renv`, refer to https://rstudio.github.io/renv/articles/collaborating.html.

### Memory, Runtime, Storage Requirements

#### Summary

Approximate time needed to reproduce the data on a standard (2024) desktop machine: 10-60 minutes

Approximate time needed to reproduce the analyses on a standard (2024) desktop machine: <10 minutes

Approximate storage space needed: 40 GB

#### Details

The code was last run on a **Apple MacBook Air with M1 chip, running macOS Sonoma version 14.3.1 with 63GB of free storage space**. 

## Description of programs/code

- Programs in `AI/01_dataprep` will extract and reformat all datasets referenced above.
- Programs in `AI/02_analysis` generate all tables and figures in the main body of the article. Output files are called appropriate names (`tab4.tex`, `fig1.png`) and should be easy to correlate with the manuscript.
- Programs in `AI/03_appendix` will generate all tables and figures in the appendix.

## Instructions to Replicators

- Run `chmod +x setup.sh` and `./setup.sh` in Terminal to set up data and output directories.
- Download the data files referenced above. Each should be renamed as above and stored in the prepared subdirectories of `AI/data/`.
- Run files in `AI/01_dataprep` in ordered sequence to generate cleaned datasets.
- Run files in `AI/02_analysis` to generate the main output.
- Run files in `AI/03_appendix` to generate figures in the appendix.

## List of tables and programs

The provided code reproduces all tables and figures in the paper. Some tables are manually reformatted and combined.

| Figure/Table #    | Program                  | Line Number | Output file                      |
|-------------------|--------------------------|-------------|----------------------------------|
| Table 1           | 02_analysis/main.R       | 1203; 1209   | tab1a.tex; tab1b.tex                 |
| Table 2           | 02_analysis/main.R       | 1131; 1380  | tab2a.tex; tab2b.tex                        |
| Table 3           | 02_analysis/main.R       | 641         | tab3.tex                       |
| Table 4           | 02_analysis/main.R       | 859         | tab4.tex                       |
| Table 5           | 02_analysis/main.R       | 950         | tab5.tex                       |
| Table C1          | 02_analysis/main.R       | 687         | tabC1.tex                      |
| Table C2          | 02_analysis/main.R       | 769         | tabC2.tex                      |
| Table C3          | 02_analysis/main.R       | 969         | tabC3.tex                      |
| Figure 1          | 02_analysis/timeseries.R |  47         | fig1.png                       |
| Figure 2          | 02_analysis/main.R       | 464          | fig2.png                      |
| Figure 3          | 02_analysis/main.R       | 1439; 1450   | fig3a.png; fig3b.png         |
| Figure 4          | 02_analysis/main.R       | 1542; 1552   | fig4a.png; fig4b.png         |
| Figure A1         | 03_appendix/truncation.R | 104          | figA1.png            |
| Figure A2         | 03_appendix/truncation.R | 117            | figA2.png            |
| Figure A3         | 03_appendix/truncation.R | 152            | figA3.png            |
| Figure A4         | 03_appendix/truncation.R | 186-199           | figA4a.png; figA4b.png; figA4c.png; figA4d.png; figA4e.png            |
| Figure B1         | 02_analysis/main.R       | 467-474  | figB1a.png; figB1b.png; figB1c.png; figB1d.png            |
| Figure B2         | 02_analysis/main.R       | 476-483  | figB2a.png; figB2b.png; figB2c.png; figB2d.png            |
| Figure B3         | 02_analysis/main.R       | 485-492  | figB3a.png; figB3b.png; figB3c.png; figB3d.png            |
| Figure B4         | 02_analysis/main.R       | 494-501  | figB4a.png; figB4b.png; figB4c.png; figB4d.png            |
| Figure C1         | 02_analysis/main.R       | 1059          | figC1.png            |

# References

Alexander Giczy, Nicholas Pairolero, and Andrew A. Toole. “Identifying Artificial Intelligence (AI) Invention: A Novel AI Patent Dataset”. In: The Journal of Technology
Transfer (Aug. 2021).

Michael Ewens and Matt Marx. Entrepreneurial Patents. Working Paper. 2023.

U.S. Bureau of Labor Statistics. QCEW County-MSA-CSA Crosswalk (For NAICS-
Based Data). 2022. url: https://www.bls.gov/cew/classifications/areas/county-
msa-csa-crosswalk.htm.

U.S. Census Bureau. Census Core-Based Statistical Area (CBSA) to Federal Information Processing Series (FIPS) County Crosswalk. 2017. National Bureau of Economic Research. url: https://www.nber.org/research/data/ssa-federal-information-processing-series-fips-core-based-statistical-area-cbsa-and-metropolitan-and.

U.S. Census Bureau. Ranking Tables for Population of Metropolitan Statistical Areas, Micropolitan Statistical Areas, Combined Statistical Areas, New England City and Town Areas, and Combined New England City and Town Areas: 1990 and 2000. Dec.
2003. url: https://www.census.gov/data/tables/2000/dec/phc-t-29.html.

USPTO. PatentsView. https://www.patentsview.org/download/. Updated February 13, 2024. 2024.
