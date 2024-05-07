

SOFA score calculation helper functions Acutelines 2024

## Description

Revisions: v1 STH Initial script v2 TvdA Revising calculations v3 RvW
Refactored code to functions, added Brown equation Convert SpO2 to PaO2
based on Brown / Ellis equation DOI: 10.1016/j.chest.2016.01.003 DOI:
0.1152/jappl.1979.46.3.599

## Usage

<pre><code class='language-R'>spo2_to_pao2(spo2)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="spo2_to_pao2_:_spo2">spo2</code>
</td>
<td>
SpO2 (%) to be converted
</td>
</tr>
</table>

## Value

pao2 Calculated PaO2 (kPa)
