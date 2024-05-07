

# Calculate the total sofa score from Acutelines df

## Description

Calculate the total sofa score from Acutelines df

## Usage

<pre><code class='language-R'>sofa_total(df, column_mapping, return_df = FALSE)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_total_:_df">df</code>
</td>
<td>
data frame
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_total_:_column_mapping">column_mapping</code>
</td>
<td>
map df columns to SOFA items. Follow this strucure:
</td>
</tr>
</table>

## Value

df column with sofa score per row

## Examples

``` r
library(acutelines.datatoolbox)

column_mapping <- c(
 PaO2 = "SOFA_24h_PaO2",
 SpO2 = "SOFA_24h_SpO2",
 FiO2 = "SOFA_24h_FiO2",
 oxygen_supply = "SOFA_24h_oxygen_supply",
 platelets = "SOFA_24h_platelets",
 bilirubin = "SOFA_24h_bilirubin",
 MAP = "SOFA_24h_MAP",
 dopamine = "SOFA_24h_dopamine",
 dobutamine = "SOFA_24h_dobutamine",
 epinephrine = "SOFA_24h_epinephrine",
 norepinephrine = "SOFA_24h_norepineprhine",
 norepinephrine_amp = "SOFA_24h_norepinephrine_amp",
 GCS = "SOFA_24h_GCS",
 creatinine = "SOFA_24h_creatinine"
 )
result <- sofa_total(df, column_mapping)
```
