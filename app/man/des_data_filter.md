**Remove NA**
- Removes _all_ rows containing NA values (default filter)

**Time filters**

- Select the range of dates and times included
- To select night times, start is > end, e.g. 22 - 6, otherwise start is < end, e.g. 10 - 20


**Temperature ranges**

- For each measured temperature difference a minimum and maximum value can be set. If no value is defined the limit is infinite.
- **Remove outliers**
   + The filter will be applied to 'Variable' and 'Color/ Group' selected in the box 'Figures' (above).
   + Outliers are defined as described in [R-bloggers: How to Remove Outliers in R](https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/), that is, outliers are defined by the 25<sup>th</sup> or 75<sup>th</sup> percentiles and the interquartile range IQR.
   + IQR = Q<sub>75%</sub> - Q<sub>25%</sub>  
   + Lower bound: Q<sub>25%</sub> - 1.5 * IQR   
   + Upper bound: Q<sub>75%</sub> + 1.5 * IQR 
   + Note: removes NA rows if present in selected 'Variable' and 'Color/ Group'

**Sensor positions**

- Specify sensor positions to be used in the analysis