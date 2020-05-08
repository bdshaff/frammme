# Framework for MMM projects (frammme)
Functions to Generate an MMM Project Framework.

The main function of this package is `create_mmm_project` which takes parameters as arguments and creates a folder with an RStudio project following the MMM modeling framework.

# Example

This example creates an MMM modeling project directory at the location pointed to by the `path` argument.
The `add_drake_workflow` adds a subdirectory with a drake workflow set up for execution.

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

The result of the call to `create_mmm_project` creates a directory with the following directory tree.
Model scripts are automatically added and ready for execution via the helper functions `create_submodel_script`, `create_salesmodel_script`, and `create_responsecurve_script`.

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

# Next Steps

1)	Initiate the Project 
    a.	Initiate a git repository
2)	Add the data
3)	Add the R code
4)	Initiate packrat
5)	Adjust Strings in sub-models
6)	Add variable categorizations to each sales-model folder
7)	Adjust Strings in sales-models
