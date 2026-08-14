# PS152 fish dataset workflow

This repository contains the data transformation workflow and metadata used to prepare the PS152 fish dataset for publication.

Files in `data/02_output/` are published via IPT at:  
<https://ipt.biodiversity.aq/resource?r=ps152-wobec-fish>

GitHub issue for this dataset: https://github.com/biodiversity-aq/wobec/issues/18

## Repository structure

```text
PS152_fish/
├── README.md                         # project overview and folder guide
├── PS152-fish_chiara.Rproj           # RStudio project file
├── .Rprofile                         # project startup (activates renv)
├── renv.lock                         # frozen R package versions
├── renv/                             # renv bootstrap files
├── src/
│   └── transform-data.R              # ETL script creating Darwin Core outputs
├── data/
│   ├── 01_input/
│   │   └── FINAL_wobec-sample-logger_PS152_4_FISH_20-07-26.xlsx
│   └── 02_output/
│       ├── event.txt                 # Darwin Core Event table
│       ├── occurrence.txt            # Darwin Core Occurrence table
│       └── emof.txt                  # Extended MeasurementOrFact table
└── metadata/
    ├── README.txt                    # IPT metadata template notes
    └── eml-wobec-template-for-IPT.xml# EML template for IPT publication
```
