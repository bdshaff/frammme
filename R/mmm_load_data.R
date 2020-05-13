mmm_load_data = function(nmp, rgn, in_data_fmi, in_data_kpi){

  FMI =
    read.csv(in_data_fmi, stringsAsFactors = FALSE) %>%
    filter(region %in% rgn) %>%
    filter(vehicle %in% nmp) %>%
    mutate(week = ymd(week)) %>%
    mutate(tier = tolower(tier),
           metric = tolower(metric))

  KPI =
    read.csv(in_data_kpi, stringsAsFactors = FALSE) %>%
    filter(region %in% rgn) %>%
    filter(vehicle %in% nmp) %>%
    mutate(month = ymd(month),
           value = as.numeric(value))

  if(length(nmp) == 1){
    fmi_dataP =
      reshape2::dcast(FMI, region + week ~ vehicle + var + type + tier, value.var = 'value',
                        fun.aggregate = sum) %>% as.data.frame(.)
  }else{
    fmi_dataP =
      reshape2::dcast(FMI, vehicle + week ~ var + type + tier, value.var = 'value',
                        fun.aggregate = sum) %>% as.data.frame(.)
  }

  fmi_dataP[is.na(fmi_dataP)]  = 0

  if(length(nmp) == 1){
    nonfmi_dataP =
      reshape2::dcast(KPI, region + month ~ vehicle + var, value.var = "value",
                        fun.aggregate = sum) %>% as.data.frame(.)
  }else{
    nonfmi_dataP =
      reshape2::dcast(KPI, vehicle + month ~ var, value.var = "value",
                        fun.aggregate = sum) %>% as.data.frame(.)
  }

  return(list(FMI = FMI,
              KPI = KPI,
              fmi_dataP = fmi_dataP,
              nonfmi_dataP = nonfmi_dataP))

}
