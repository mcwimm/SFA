**'Remove outlier' filter**

The filter will be applied to the shown (selected) 'Variable' in the box above ('Figures').

Outliers are defined as described in [R-bloggers: How to Remove Outliers in R](https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/), that is, outliers are defined by the 25<sup>th</sup> or 75<sup>th</sup> percentiles and the interquartile range IQR.

IQR = Q<sub>75%</sub> - Q<sub>25%</sub>  
Lower bound: Q<sub>25%</sub> - 1.5 * IQR   
Upper bound: Q<sub>75%</sub> + 1.5 * IQR 