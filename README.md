# Framework for MMM projects (frammme)
Functions to Generate an MMM Project Framework

# Example

```
library(frammme)


create_mmm_project(path = "~/Desktop/",
                   FiscalYear = "FY19",
                   modeling_start_date = "2015-04-01",
                   modeling_end_date = "2019-09-30",
                   add_drake_workflow = TRUE,
                   open_proj = TRUE,
                   init_packrat = FALSE)
```
