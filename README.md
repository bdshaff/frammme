# Framework for MMM projects (frammme)
Functions to Generate an MMM Project Framework

# Example

```{r}
library(frammme)


create_mmm_project(path = "~/Desktop/",
                   FiscalYear = "FY19",
                   modeling_start_date = "2015-04-01",
                   modeling_end_date = "2019-09-30",
                   add_drake_workflow = TRUE,
                   open_proj = TRUE,
                   init_packrat = FALSE)
```

Result

```
.
├── FY19-Modeling.Rproj
├── R
├── data
│   ├── categorization
│   ├── fit_curves
│   └── processed_data
├── drake_workflow
│   ├── make.R
│   └── wR
│       ├── load_fit_curves.R
│       ├── packages.R
│       ├── plan.R
│       └── script_functions.R
├── modeling
│   ├── ResponseCurves
│   │   ├── ALT
│   │   │   └── Altima-FY19.R
│   │   ├── ARM
│   │   │   └── Armada-FY19.R
│   │   ├── FRO
│   │   │   └── Frontier-FY19.R
│   │   ├── LEF
│   │   │   └── LEAF-FY19.R
│   │   ├── MAX
│   │   │   └── Maxima-FY19.R
│   │   ├── MUR
│   │   │   └── Murano-FY19.R
│   │   ├── NV
│   │   │   └── NV-FY19.R
│   │   ├── PTH
│   │   │   └── Pathfinder-FY19.R
│   │   ├── RGE
│   │   │   └── ROGUE-FY19.R
│   │   ├── RGS
│   │   │   └── RogueSport-FY19.R
│   │   ├── SEN
│   │   │   └── Sentra-FY19.R
│   │   ├── TTN
│   │   │   └── Titan-FY19.R
│   │   └── VER
│   │       └── Versa-FY19.R
│   ├── SalesModels
│   │   ├── ALT
│   │   │   └── Altima-FY19.R
│   │   ├── ARM
│   │   │   └── Armada-FY19.R
│   │   ├── FRO
│   │   │   └── Frontier-FY19.R
│   │   ├── LEF
│   │   │   └── LEAF-FY19.R
│   │   ├── MAX
│   │   │   └── Maxima-FY19.R
│   │   ├── MUR
│   │   │   └── Murano-FY19.R
│   │   ├── NV
│   │   │   └── NV-FY19.R
│   │   ├── PTH
│   │   │   └── Pathfinder-FY19.R
│   │   ├── RGE
│   │   │   └── ROGUE-FY19.R
│   │   ├── RGS
│   │   │   └── RogueSport-FY19.R
│   │   ├── SEN
│   │   │   └── Sentra-FY19.R
│   │   ├── TTN
│   │   │   └── Titan-FY19.R
│   │   └── VER
│   │       └── Versa-FY19.R
│   └── SubModels
│       ├── KBA
│       │   └── FY19-AllModels
│       │       └── KBA-FY19.R
│       ├── MA
│       │   └── FY19-AllModels
│       │       └── MA-FY19.R
│       ├── OAO
│       │   └── FY19-AllModels
│       │       └── OAO-FY19.R
│       ├── PC
│       │   └── FY19-AllModels
│       │       └── PC-FY19.R
│       ├── PI
│       │   └── FY19-AllModels
│       │       └── PI-FY19.R
│       └── SRC
│           └── FY19-AllModels
│               └── SRC-FY19.R
└── output
    ├── ResponseCurves
    │   ├── ALT
    │   ├── ARM
    │   ├── FRO
    │   ├── LEF
    │   ├── MAX
    │   ├── MUR
    │   ├── NV
    │   ├── PTH
    │   ├── RGE
    │   ├── RGS
    │   ├── SEN
    │   ├── TTN
    │   └── VER
    ├── SalesModels
    │   ├── ALT
    │   ├── ARM
    │   ├── FRO
    │   ├── LEF
    │   ├── MAX
    │   ├── MUR
    │   ├── NV
    │   ├── PTH
    │   ├── RGE
    │   ├── RGS
    │   ├── SEN
    │   ├── TTN
    │   └── VER
    └── SubModels
            ├── KBA
        ├── MA
        ├── OAO
        ├── PC
        ├── PI
        └── SRC
```
