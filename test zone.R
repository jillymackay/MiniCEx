fil <- file.path("//cmvm.datastore.ed.ac.uk/cmvm/mvmsan/rdsvsshared/Fieldsec/BVMS Years/Final Year/MiniCEx Analyses/AY 2023-2024/App Data - Save Your MiniCEx Spreadsheet Here/FY MiniCE 2023_2024_dashboard build.xlsx")


d <- mcex_read(fil)




library(renv)
renv::dependencies()
renv::init()
