

Calculate the sofa score for cardiovacular Note: due to inconsistent
registration of norepinephrine dosages, when norephenrine is present 3.5
points are given. Researchers a free to either round this to 4
(overesitmate), 3 (underesitmate) or leave it as is

## Description

Calculate the sofa score for cardiovacular Note: due to inconsistent
registration of norepinephrine dosages, when norephenrine is present 3.5
points are given. Researchers a free to either round this to 4
(overesitmate), 3 (underesitmate) or leave it as is

## Usage

<pre><code class='language-R'>sofa_cardiovascular(
  map,
  dopamine,
  dobutamine,
  epinephrine,
  norepinephrine,
  norepinephrine_amp
)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_cardiovascular_:_map">map</code>
</td>
<td>
Mean Arterial Pressure
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_cardiovascular_:_dopamine">dopamine</code>
</td>
<td>
Dopamine dosage
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_cardiovascular_:_dobutamine">dobutamine</code>
</td>
<td>
Dobutamine dosage
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_cardiovascular_:_epinephrine">epinephrine</code>
</td>
<td>
Epinephrine dosage
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_cardiovascular_:_norepinephrine">norepinephrine</code>
</td>
<td>
Norepinephrine dosage
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_cardiovascular_:_norepinephrine_amp">norepinephrine_amp</code>
</td>
<td>
Norepinephrine ampul administration (1=TRUE, 0=FALSE)
</td>
</tr>
</table>

## Value

sofa score
