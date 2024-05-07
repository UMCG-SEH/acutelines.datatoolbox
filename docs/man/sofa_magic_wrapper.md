

Magic wrapper to automatically calculate the SOFA score on multiple
intervals (eg. hours, days) assuming the data is exported using
Acutelines SOFA SQL snippet. Note: this function assumes data is
exported using the Acutelines SOFA export snippet, if not manually
define columns and use sofa_total().

## Description

Magic wrapper to automatically calculate the SOFA score on multiple
intervals (eg. hours, days) assuming the data is exported using
Acutelines SOFA SQL snippet. Note: this function assumes data is
exported using the Acutelines SOFA export snippet, if not manually
define columns and use sofa_total().

## Usage

<pre><code class='language-R'>sofa_magic_wrapper(
  df,
  interval = 24,
  timespan = 72,
  return_df = FALSE,
  naming.cols = "SOFA_&lt;interval&gt;h_&lt;variable&gt;",
  naming.result = "SOFAscore_&lt;interval&gt;h_&lt;variable&gt;"
)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_magic_wrapper_:_df">df</code>
</td>
<td>
dataframe containing all the data
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_magic_wrapper_:_interval">interval</code>
</td>
<td>
interval length, defaults to 24h
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_magic_wrapper_:_timespan">timespan</code>
</td>
<td>
number of intervals\*interval length, defaults to 72h
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_magic_wrapper_:_return_df">return_df</code>
</td>
<td>
Return all elements of the sofa score in the DF instead of only totals
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_magic_wrapper_:_naming.cols">naming.cols</code>
</td>
<td>
naming scheme to use to select columns. <interval> will be replaced by
the interval and <variable> by the variable name
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="sofa_magic_wrapper_:_naming.result">naming.result</code>
</td>
<td>
naming scheme to assign to column with total score
</td>
</tr>
</table>

## Value

df with sofa scores
