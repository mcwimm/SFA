#### How to upload your data?

1. Browse to your target directory
2. Select the type of input, i.e. the manufacturer
3. Select how many rows should be skipped (_Skip_) and which separator is used (_Separator_)
4. Check your data (including extracted sensor depths)

If nothing is selected, a default file is used. This file contains data of a....


#### Current input types

- **Raw**: raw temperature measurements
   + required columns: `i*U`, `i*L`, `i*S` with `U`, `L` and `S` representing temperature measurements in the upper, lower and side sensor
- **Delta**: data sets with calculated temperature differences
   + required columns: `dTSym*i` and `dTas*i`
- **Processed**: data processed with SFA (long format), i.e. include _K_, _SFS_, _SFD_, etc.
   + _read_ mode: data can be visualized
   + _write_ mode: data can be modified
   + required columns: `dTas`, `dTSym`, `dTsa`, `dTsym.dTas`
   
_Note:_   
   + _`i` is an integer representing thermometer position, e.g. `3*U`_
   + _`*` is an separator, e.g. '', ' ', '.'_
   + _All types require columns with: `date` (DD.MM.YYYY) and `time` (HH:MM:SS) **OR** `datetime` (DD.MM.YYYY HH:MM:SS)_
   + _Column names are not case sensitive_

