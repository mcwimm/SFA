#### How to upload your data?

1. Browse to the target directory and select a file
2. Select the type of input, e.g., raw data or processed data
3. If the upload does not work, specify a character string that occurs in the header line of the selected file, e.g. 'dTSym' (case sensitive, default is 'ime')
4. Check your data (including extracted sensor depths)

If nothing is selected, a default file is used. 

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

