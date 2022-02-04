#### How to upload your data?

1. Browse to your target directory
2. Select the type of input, i.e. the manufacturer
3. Select how many rows should be skipped (_Skip_) and which separator is used (_Separator_)
4. Check your data (including extracted sensor depths)

If nothing is selected, a default file is used. This file contains data of a....


#### Current input types

- **Raw**: raw temperature measurements
   + required columns: `i*U`, `i_L`, `i*S` with `U`, `L` and `S` representing temperature measurements in the upper, lower and side sensor
- **Delta**: data sets with calculated temperature differences, e.g. Sap Flow Analyzer output
   + required columns: `dTSym*i` and `dTas*i`
   
_Note:_   
   + _`i` is an integer representing thermometer position, e.g. `3*U`_
   + _`*` is an separator, e.g. '', ' ', '.'_
   + _All types require columns with: `date` (DD.MM.YYYY) and `time` (HH:MM:SS) **OR** `datetime` (DD.MM.YYYY HH:MM:SS)_
   + _Column names are not case sensitive_

