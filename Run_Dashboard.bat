
@echo off
title Traffic Accident ML Dashboard
cd /d "D:\Traffic_Accident_ML"

echo Starting Traffic Accident ML Dashboard...
echo.

"C:\Program Files\R\R-4.6.1\bin\R.exe" --vanilla -e "setwd('D:/Traffic_Accident_ML'); shiny::runApp('dashboard')"

echo.
echo Dashboard process ended. Check the messages above.
pause

