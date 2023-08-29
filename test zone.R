fil <- file.path("//cmvm.datastore.ed.ac.uk/cmvm/mvmsan/rdsvsshared/Fieldsec/BVMS Years/Final Year/MiniCEx Analyses/AY 2023-2024/App Data - Save Your MiniCEx Spreadsheet Here/FY MiniCE 2023_2024.xlsx")
edit <- file.path("//cmvm.datastore.ed.ac.uk/cmvm/mvmsan/rdsvsshared/Fieldsec/BVMS Years/Final Year/MiniCEx Analyses/AY 2023-2024/App Data - Save Your MiniCEx Spreadsheet Here/FY MiniCE 2023_2024_RowsToEdit.xlsx")


d <- mcex_read(fil)



dd <- mcex_edit(edit, d)


max(dd$DateEvent)

dd %>% 
  filter(!is.na(DateEvent)) %>% 
  max(DateEvent)


dd %>% 
  filter(!is.na(DateEvent)) %>% 
  summarise(max = max(DateEvent))
