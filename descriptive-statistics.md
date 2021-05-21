---
title: "Untitled"
author: "Tony Carilli"
date: 'May 20, 2021'
output: 
  html_document:
    toc: false
    toc_float: true
    df_print: paged
    keep_md: true
    code_folding: hide
---




```r
df <-
  here::here("data", "covid_data_final.csv") %>%
  read_csv() %>%
  select(
    state,
    median_household_income,
    percent_adults_with_ba,
    percent_in_fair_or_poor_health,
    percent_black,
    density,
    percent_rural,
    percent_65,
    average_temperature,
    governor_political_affiliation
  ) %>% 
  group_by(state) %>% 
  distinct(
    state,
    median_household_income,
    percent_adults_with_ba,
    percent_in_fair_or_poor_health,
    percent_black,
    density,
    percent_rural,
    percent_65,
    average_temperature,
    governor_political_affiliation
  ) %>% 
  filter(state != "VI") %>% 
  ungroup()
```


```r
df <- # data
  here::here("data", "covid_data_final.csv") %>% 
  read_csv() %>% 
  filter(date >= "2020-03-01")
```



```r
df %>% 
  pivot_longer(-c(state, governor_political_affiliation)) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~ name, scales = "free_x")
```


```r
df %>% 
  select(-c(state, governor_political_affiliation)) %>% 
  pastecs::stat.desc() %>% 
  rownames_to_column("stat") %>% 
  filter(stat %in% c("nbr.val",
                          "min",
                          "max",
                          "mean", 
                          "median",
                          "std.dev")) %>% 
  gt() %>% 
  tab_header(title = "Descriptive Statistics",
             subtitle = "Controls") %>% 
  cols_label(stat = "Statistic", 
             median_household_income = "Median Household Income",
             percent_adults_with_ba = "Adults w/ BA (%)",
             percent_in_fair_or_poor_health = "Fair or Poor Health (%)",
             percent_black = "Black (%)",
             density = "Pop Density",
             percent_rural = "Rural Pop (%)",
             percent_65 = "Age > 65 (%)",
             average_temperature = "Average Temperature"
  ) %>% 
  cols_align("center") 
```


 

```r
df %>% 
  select(governor_political_affiliation) %>% 
  summarytools::freq()
```

```
Frequencies  
df$governor_political_affiliation  
Type: Character  

                    Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
---------------- ------- --------- -------------- --------- --------------
        Democrat   10000     49.02          49.02     49.02          49.02
      Republican   10400     50.98         100.00     50.98         100.00
            <NA>       0                               0.00         100.00
           Total   20400    100.00         100.00    100.00         100.00
```


```r
df %>% 
  select(cases_new, stringency_index, median_household_income, 
         percent_adults_with_ba, percent_in_fair_or_poor_health,
         percent_black, density, percent_rural, percent_65, average_temperature,
         state_level_index) %>% 
  as.data.frame() %>% 
  stargazer::stargazer(type = "html")
```


<table style="text-align:center"><tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Statistic</td><td>N</td><td>Mean</td><td>St. Dev.</td><td>Min</td><td>Pctl(25)</td><td>Pctl(75)</td><td>Max</td></tr>
<tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">cases_new</td><td>20,400</td><td>237.000</td><td>297.000</td><td>-135</td><td>48.5</td><td>308.0</td><td>4,513</td></tr>
<tr><td style="text-align:left">stringency_index</td><td>20,399</td><td>49.000</td><td>16.600</td><td>0.000</td><td>39.400</td><td>60.200</td><td>88.000</td></tr>
<tr><td style="text-align:left">median_household_income</td><td>20,400</td><td>56,031.000</td><td>9,314.000</td><td>40,528</td><td>48,900</td><td>62,520</td><td>76,067</td></tr>
<tr><td style="text-align:left">percent_adults_with_ba</td><td>20,400</td><td>30.000</td><td>6.060</td><td>19.600</td><td>26.500</td><td>32.900</td><td>55.400</td></tr>
<tr><td style="text-align:left">percent_in_fair_or_poor_health</td><td>20,400</td><td>16.500</td><td>3.050</td><td>12.000</td><td>14.100</td><td>18.400</td><td>24.400</td></tr>
<tr><td style="text-align:left">percent_black</td><td>20,400</td><td>10.900</td><td>10.600</td><td>0.400</td><td>3.100</td><td>15.400</td><td>47.400</td></tr>
<tr><td style="text-align:left">density</td><td>20,400</td><td>407.000</td><td>1,492.000</td><td>1.290</td><td>43.100</td><td>231.000</td><td>10,795.000</td></tr>
<tr><td style="text-align:left">percent_rural</td><td>20,400</td><td>25.900</td><td>14.700</td><td>0.000</td><td>12.100</td><td>35.200</td><td>61.300</td></tr>
<tr><td style="text-align:left">percent_65</td><td>20,400</td><td>14.500</td><td>1.740</td><td>9.560</td><td>13.800</td><td>15.400</td><td>19.000</td></tr>
<tr><td style="text-align:left">average_temperature</td><td>19,600</td><td>63.500</td><td>9.190</td><td>50.800</td><td>55.200</td><td>69.500</td><td>82.500</td></tr>
<tr><td style="text-align:left">state_level_index</td><td>20,400</td><td>-0.000</td><td>0.990</td><td>-2.150</td><td>-0.706</td><td>0.787</td><td>2.080</td></tr>
<tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr></table>


```r
df %>% 
  select(cases_new, stringency_index, median_household_income, 
         percent_adults_with_ba, percent_in_fair_or_poor_health,
         percent_black, density, percent_rural, percent_65, average_temperature,
         state_level_index) %>% 
  pastecs::stat.desc() %>% 
  rownames_to_column("stat") %>% 
  filter(stat %in% c("nbr.val",
                          "min",
                          "max",
                          "mean", 
                          "median",
                          "std.dev")) %>% 
  pivot_longer(everything(),
    values_to = "Statistic",
    names_to = "Variable"
  )
  gt() %>% 
  tab_header(title = "Descriptive Statistics") %>% 
  cols_label(stat = "Statistic",
             cases_new = "New Cases",
             stringency_index = "Stringency Index",
             median_household_income = "Median Household Income",
             percent_adults_with_ba = "Adults w/ BA (%)",
             percent_in_fair_or_poor_health = "Fair or Poor Health (%)",
             percent_black = "Black (%)",
             density = "Pop Density",
             percent_rural = "Rural Pop (%)",
             percent_65 = "Age > 65 (%)",
             average_temperature = "Average Temperature",
             state_level_index = "Social Capital"
  ) %>% 
  cols_align("center") 
```


```r
df %>%
  select(
    cases_new,
    stringency_index,
    median_household_income,
    percent_adults_with_ba,
    percent_in_fair_or_poor_health,
    percent_black,
    density,
    percent_rural,
    percent_65,
    average_temperature,
    state_level_index
  ) %>%
  pastecs::stat.desc() %>%
  rownames_to_column("stat") %>%
  filter(stat %in% c("nbr.val",
                     "min",
                     "max",
                     "mean",
                     "median",
                     "std.dev")) %>%
  pivot_longer(-stat) %>%
  pivot_wider(names_from = "stat") %>%
  select(-name) %>% 
  mutate(row_names = c("New Cases", 
                       "Stringency Index",
                       "Median Household Income",
                       "% Adults w/ BA",
                       "% Fair or Poor Health",
                       "% Black",
                       "Pop. Density",
                       "% Rural",
                       "% > 65",
                       "Average Temperature",
                       "Social Capital Index")) %>% 
  gt(rowname_col = "row_names") %>%
  tab_header(title = "Descriptive Statistics") %>%
  cols_label(
    nbr.val = "N",
    min = "Min",
    max = "Max",
    median = "Median",
    mean = "Mean",
    std.dev = glue::glue("Standard \n Deviation")
  ) %>%
  cols_align("center") %>% 
  fmt_number(columns = c("mean","median"),
             decimals = 2)
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#feeuxmkfpe .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#feeuxmkfpe .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#feeuxmkfpe .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#feeuxmkfpe .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#feeuxmkfpe .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#feeuxmkfpe .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#feeuxmkfpe .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#feeuxmkfpe .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#feeuxmkfpe .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#feeuxmkfpe .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#feeuxmkfpe .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#feeuxmkfpe .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#feeuxmkfpe .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#feeuxmkfpe .gt_from_md > :first-child {
  margin-top: 0;
}

#feeuxmkfpe .gt_from_md > :last-child {
  margin-bottom: 0;
}

#feeuxmkfpe .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#feeuxmkfpe .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#feeuxmkfpe .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#feeuxmkfpe .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#feeuxmkfpe .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#feeuxmkfpe .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#feeuxmkfpe .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#feeuxmkfpe .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#feeuxmkfpe .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#feeuxmkfpe .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#feeuxmkfpe .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#feeuxmkfpe .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#feeuxmkfpe .gt_left {
  text-align: left;
}

#feeuxmkfpe .gt_center {
  text-align: center;
}

#feeuxmkfpe .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#feeuxmkfpe .gt_font_normal {
  font-weight: normal;
}

#feeuxmkfpe .gt_font_bold {
  font-weight: bold;
}

#feeuxmkfpe .gt_font_italic {
  font-style: italic;
}

#feeuxmkfpe .gt_super {
  font-size: 65%;
}

#feeuxmkfpe .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="feeuxmkfpe" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="7" class="gt_heading gt_title gt_font_normal" style>Descriptive Statistics</th>
    </tr>
    <tr>
      <th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">N</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Min</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Max</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Median</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Standard 
Deviation</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left gt_stub">New Cases</td>
      <td class="gt_row gt_center">20400</td>
      <td class="gt_row gt_center">-134.75</td>
      <td class="gt_row gt_center">4513.08</td>
      <td class="gt_row gt_center">132.37</td>
      <td class="gt_row gt_center">237.00</td>
      <td class="gt_row gt_center">297.24</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Stringency Index</td>
      <td class="gt_row gt_center">20399</td>
      <td class="gt_row gt_center">0.00</td>
      <td class="gt_row gt_center">87.96</td>
      <td class="gt_row gt_center">48.61</td>
      <td class="gt_row gt_center">49.01</td>
      <td class="gt_row gt_center">16.57</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Median Household Income</td>
      <td class="gt_row gt_center">20400</td>
      <td class="gt_row gt_center">40528.00</td>
      <td class="gt_row gt_center">76067.00</td>
      <td class="gt_row gt_center">54,384.00</td>
      <td class="gt_row gt_center">56,031.06</td>
      <td class="gt_row gt_center">9313.59</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">% Adults w/ BA</td>
      <td class="gt_row gt_center">20400</td>
      <td class="gt_row gt_center">19.60</td>
      <td class="gt_row gt_center">55.40</td>
      <td class="gt_row gt_center">29.00</td>
      <td class="gt_row gt_center">30.04</td>
      <td class="gt_row gt_center">6.06</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">% Fair or Poor Health</td>
      <td class="gt_row gt_center">20400</td>
      <td class="gt_row gt_center">12.00</td>
      <td class="gt_row gt_center">24.40</td>
      <td class="gt_row gt_center">15.80</td>
      <td class="gt_row gt_center">16.48</td>
      <td class="gt_row gt_center">3.05</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">% Black</td>
      <td class="gt_row gt_center">20400</td>
      <td class="gt_row gt_center">0.40</td>
      <td class="gt_row gt_center">47.40</td>
      <td class="gt_row gt_center">7.10</td>
      <td class="gt_row gt_center">10.93</td>
      <td class="gt_row gt_center">10.63</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Pop. Density</td>
      <td class="gt_row gt_center">20400</td>
      <td class="gt_row gt_center">1.29</td>
      <td class="gt_row gt_center">10794.58</td>
      <td class="gt_row gt_center">106.26</td>
      <td class="gt_row gt_center">407.01</td>
      <td class="gt_row gt_center">1491.83</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">% Rural</td>
      <td class="gt_row gt_center">20400</td>
      <td class="gt_row gt_center">0.00</td>
      <td class="gt_row gt_center">61.34</td>
      <td class="gt_row gt_center">25.80</td>
      <td class="gt_row gt_center">25.90</td>
      <td class="gt_row gt_center">14.74</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">% &gt; 65</td>
      <td class="gt_row gt_center">20400</td>
      <td class="gt_row gt_center">9.56</td>
      <td class="gt_row gt_center">19.01</td>
      <td class="gt_row gt_center">14.57</td>
      <td class="gt_row gt_center">14.54</td>
      <td class="gt_row gt_center">1.74</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Average Temperature</td>
      <td class="gt_row gt_center">19600</td>
      <td class="gt_row gt_center">50.76</td>
      <td class="gt_row gt_center">82.45</td>
      <td class="gt_row gt_center">62.75</td>
      <td class="gt_row gt_center">63.51</td>
      <td class="gt_row gt_center">9.19</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">Social Capital Index</td>
      <td class="gt_row gt_center">20400</td>
      <td class="gt_row gt_center">-2.15</td>
      <td class="gt_row gt_center">2.08</td>
      <td class="gt_row gt_center">&minus;0.09</td>
      <td class="gt_row gt_center">&minus;0.00</td>
      <td class="gt_row gt_center">0.99</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->


```r
df %>%
    plm::plm(
      cases_new ~ stringency_index_lag * state_level_index +
        I(stringency_index_lag ^ 2),
      data = .,
      effect = "twoways",
      index = "state",
      method = "within"
    ) %>% 
  broom::tidy() %>% 
  pull(estimate) ->
  coefs
```



```r
sc_index <- seq(-2.15, 2.15, by = .1)
stringency_index <- 0:90
```



```r
new <- tibble()

for (i in seq_along(sc_index)) {
  for (j in seq_along(stringency_index)) {
    new[i, j] <- coefs[1] * stringency_index[j] +
      coefs[2] * stringency_index[j] ^ 2 +
      coefs[3] * stringency_index[j] * sc_index[i]
  }
}


new %>% 
  select(min = "...1",
         q1 = "...39",
         q2 = "...49",
         q3 = "...60",
         max = "...89") %>% 
  mutate(sc_index = sc_index) %>% 
  pivot_longer(-sc_index,
               names_to = "stringency_index",
               values_to = "new_cases") %>% 
  ggplot(aes(x = sc_index, y = new_cases, color = stringency_index)) + 
  geom_line()
```

![](descriptive-statistics_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
# new %>% 
#   select("...50", "...89", "...1", "...39", "...60") %>% 
#   mutate(sc_index = sc_index) %>% 
#   ggplot(aes(y = ...50, x = sc_index)) +
#   geom_line() +
#   geom_line(aes(x = sc_index, y = ...89), color = "blue") +
#   geom_line(aes(x = sc_index, y = ...1), color = "red") +
#   geom_line(aes(x = sc_index, y = ...39), color = "green") +
#   geom_line(aes(x = sc_index, y = ...60), color = "purple")
```


```r
new
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["...1"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["...2"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["...3"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["...4"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["...5"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["...6"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["...7"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["...8"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["...9"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["...10"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["...11"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["...12"],"name":[12],"type":["dbl"],"align":["right"]},{"label":["...13"],"name":[13],"type":["dbl"],"align":["right"]},{"label":["...14"],"name":[14],"type":["dbl"],"align":["right"]},{"label":["...15"],"name":[15],"type":["dbl"],"align":["right"]},{"label":["...16"],"name":[16],"type":["dbl"],"align":["right"]},{"label":["...17"],"name":[17],"type":["dbl"],"align":["right"]},{"label":["...18"],"name":[18],"type":["dbl"],"align":["right"]},{"label":["...19"],"name":[19],"type":["dbl"],"align":["right"]},{"label":["...20"],"name":[20],"type":["dbl"],"align":["right"]},{"label":["...21"],"name":[21],"type":["dbl"],"align":["right"]},{"label":["...22"],"name":[22],"type":["dbl"],"align":["right"]},{"label":["...23"],"name":[23],"type":["dbl"],"align":["right"]},{"label":["...24"],"name":[24],"type":["dbl"],"align":["right"]},{"label":["...25"],"name":[25],"type":["dbl"],"align":["right"]},{"label":["...26"],"name":[26],"type":["dbl"],"align":["right"]},{"label":["...27"],"name":[27],"type":["dbl"],"align":["right"]},{"label":["...28"],"name":[28],"type":["dbl"],"align":["right"]},{"label":["...29"],"name":[29],"type":["dbl"],"align":["right"]},{"label":["...30"],"name":[30],"type":["dbl"],"align":["right"]},{"label":["...31"],"name":[31],"type":["dbl"],"align":["right"]},{"label":["...32"],"name":[32],"type":["dbl"],"align":["right"]},{"label":["...33"],"name":[33],"type":["dbl"],"align":["right"]},{"label":["...34"],"name":[34],"type":["dbl"],"align":["right"]},{"label":["...35"],"name":[35],"type":["dbl"],"align":["right"]},{"label":["...36"],"name":[36],"type":["dbl"],"align":["right"]},{"label":["...37"],"name":[37],"type":["dbl"],"align":["right"]},{"label":["...38"],"name":[38],"type":["dbl"],"align":["right"]},{"label":["...39"],"name":[39],"type":["dbl"],"align":["right"]},{"label":["...40"],"name":[40],"type":["dbl"],"align":["right"]},{"label":["...41"],"name":[41],"type":["dbl"],"align":["right"]},{"label":["...42"],"name":[42],"type":["dbl"],"align":["right"]},{"label":["...43"],"name":[43],"type":["dbl"],"align":["right"]},{"label":["...44"],"name":[44],"type":["dbl"],"align":["right"]},{"label":["...45"],"name":[45],"type":["dbl"],"align":["right"]},{"label":["...46"],"name":[46],"type":["dbl"],"align":["right"]},{"label":["...47"],"name":[47],"type":["dbl"],"align":["right"]},{"label":["...48"],"name":[48],"type":["dbl"],"align":["right"]},{"label":["...49"],"name":[49],"type":["dbl"],"align":["right"]},{"label":["...50"],"name":[50],"type":["dbl"],"align":["right"]},{"label":["...51"],"name":[51],"type":["dbl"],"align":["right"]},{"label":["...52"],"name":[52],"type":["dbl"],"align":["right"]},{"label":["...53"],"name":[53],"type":["dbl"],"align":["right"]},{"label":["...54"],"name":[54],"type":["dbl"],"align":["right"]},{"label":["...55"],"name":[55],"type":["dbl"],"align":["right"]},{"label":["...56"],"name":[56],"type":["dbl"],"align":["right"]},{"label":["...57"],"name":[57],"type":["dbl"],"align":["right"]},{"label":["...58"],"name":[58],"type":["dbl"],"align":["right"]},{"label":["...59"],"name":[59],"type":["dbl"],"align":["right"]},{"label":["...60"],"name":[60],"type":["dbl"],"align":["right"]},{"label":["...61"],"name":[61],"type":["dbl"],"align":["right"]},{"label":["...62"],"name":[62],"type":["dbl"],"align":["right"]},{"label":["...63"],"name":[63],"type":["dbl"],"align":["right"]},{"label":["...64"],"name":[64],"type":["dbl"],"align":["right"]},{"label":["...65"],"name":[65],"type":["dbl"],"align":["right"]},{"label":["...66"],"name":[66],"type":["dbl"],"align":["right"]},{"label":["...67"],"name":[67],"type":["dbl"],"align":["right"]},{"label":["...68"],"name":[68],"type":["dbl"],"align":["right"]},{"label":["...69"],"name":[69],"type":["dbl"],"align":["right"]},{"label":["...70"],"name":[70],"type":["dbl"],"align":["right"]},{"label":["...71"],"name":[71],"type":["dbl"],"align":["right"]},{"label":["...72"],"name":[72],"type":["dbl"],"align":["right"]},{"label":["...73"],"name":[73],"type":["dbl"],"align":["right"]},{"label":["...74"],"name":[74],"type":["dbl"],"align":["right"]},{"label":["...75"],"name":[75],"type":["dbl"],"align":["right"]},{"label":["...76"],"name":[76],"type":["dbl"],"align":["right"]},{"label":["...77"],"name":[77],"type":["dbl"],"align":["right"]},{"label":["...78"],"name":[78],"type":["dbl"],"align":["right"]},{"label":["...79"],"name":[79],"type":["dbl"],"align":["right"]},{"label":["...80"],"name":[80],"type":["dbl"],"align":["right"]},{"label":["...81"],"name":[81],"type":["dbl"],"align":["right"]},{"label":["...82"],"name":[82],"type":["dbl"],"align":["right"]},{"label":["...83"],"name":[83],"type":["dbl"],"align":["right"]},{"label":["...84"],"name":[84],"type":["dbl"],"align":["right"]},{"label":["...85"],"name":[85],"type":["dbl"],"align":["right"]},{"label":["...86"],"name":[86],"type":["dbl"],"align":["right"]},{"label":["...87"],"name":[87],"type":["dbl"],"align":["right"]},{"label":["...88"],"name":[88],"type":["dbl"],"align":["right"]},{"label":["...89"],"name":[89],"type":["dbl"],"align":["right"]},{"label":["...90"],"name":[90],"type":["dbl"],"align":["right"]},{"label":["...91"],"name":[91],"type":["dbl"],"align":["right"]}],"data":[{"1":"0","2":"-2.69","3":"-5.34","4":"-7.92","5":"-10.5","6":"-12.9","7":"-15.4","8":"-17.8","9":"-20.1","10":"-22.3","11":"-24.6","12":"-26.7","13":"-28.8","14":"-30.9","15":"-32.9","16":"-34.9","17":"-36.8","18":"-38.6","19":"-40.4","20":"-42.2","21":"-43.9","22":"-45.5","23":"-47.1","24":"-48.6","25":"-50.1","26":"-51.5","27":"-52.9","28":"-54.2","29":"-55.5","30":"-56.7","31":"-57.9","32":"-59.0","33":"-60.0","34":"-61.1","35":"-62.0","36":"-62.9","37":"-63.8","38":"-64.5","39":"-65.3","40":"-66.0","41":"-66.6","42":"-67.2","43":"-67.7","44":"-68.2","45":"-68.6","46":"-69.0","47":"-69.3","48":"-69.6","49":"-69.8","50":"-70.0","51":"-70.1","52":"-70.1","53":"-70.1","54":"-70.1","55":"-70.0","56":"-69.8","57":"-69.6","58":"-69.4","59":"-69.1","60":"-68.7","61":"-68.3","62":"-67.8","63":"-67.3","64":"-66.7","65":"-66.1","66":"-65.4","67":"-64.6","68":"-63.9","69":"-63.0","70":"-62.1","71":"-61.2","72":"-60.2","73":"-59.1","74":"-58.0","75":"-56.9","76":"-55.7","77":"-54.4","78":"-53.1","79":"-51.7","80":"-50.3","81":"-48.8","82":"-47.3","83":"-45.7","84":"-44.1","85":"-42.4","86":"-40.7","87":"-38.9","88":"-37.0","89":"-35.1","90":"-33.2","91":"-31.2"},{"1":"0","2":"-2.75","3":"-5.45","4":"-8.09","5":"-10.7","6":"-13.2","7":"-15.7","8":"-18.1","9":"-20.5","10":"-22.9","11":"-25.1","12":"-27.4","13":"-29.5","14":"-31.6","15":"-33.7","16":"-35.7","17":"-37.7","18":"-39.6","19":"-41.4","20":"-43.2","21":"-45.0","22":"-46.7","23":"-48.3","24":"-49.9","25":"-51.5","26":"-52.9","27":"-54.4","28":"-55.8","29":"-57.1","30":"-58.4","31":"-59.6","32":"-60.7","33":"-61.9","34":"-62.9","35":"-63.9","36":"-64.9","37":"-65.8","38":"-66.6","39":"-67.4","40":"-68.2","41":"-68.9","42":"-69.5","43":"-70.1","44":"-70.7","45":"-71.1","46":"-71.6","47":"-71.9","48":"-72.3","49":"-72.5","50":"-72.8","51":"-72.9","52":"-73.0","53":"-73.1","54":"-73.1","55":"-73.1","56":"-73.0","57":"-72.8","58":"-72.6","59":"-72.3","60":"-72.0","61":"-71.7","62":"-71.3","63":"-70.8","64":"-70.3","65":"-69.7","66":"-69.1","67":"-68.4","68":"-67.7","69":"-66.9","70":"-66.0","71":"-65.2","72":"-64.2","73":"-63.2","74":"-62.2","75":"-61.1","76":"-59.9","77":"-58.7","78":"-57.5","79":"-56.1","80":"-54.8","81":"-53.4","82":"-51.9","83":"-50.4","84":"-48.8","85":"-47.2","86":"-45.5","87":"-43.7","88":"-42.0","89":"-40.1","90":"-38.2","91":"-36.3"},{"1":"0","2":"-2.81","3":"-5.56","4":"-8.27","5":"-10.9","6":"-13.5","7":"-16.1","8":"-18.5","9":"-21.0","10":"-23.4","11":"-25.7","12":"-28.0","13":"-30.2","14":"-32.4","15":"-34.5","16":"-36.6","17":"-38.6","18":"-40.6","19":"-42.5","20":"-44.3","21":"-46.1","22":"-47.9","23":"-49.6","24":"-51.2","25":"-52.8","26":"-54.4","27":"-55.9","28":"-57.3","29":"-58.7","30":"-60.0","31":"-61.3","32":"-62.5","33":"-63.7","34":"-64.8","35":"-65.9","36":"-66.9","37":"-67.8","38":"-68.8","39":"-69.6","40":"-70.4","41":"-71.2","42":"-71.9","43":"-72.5","44":"-73.1","45":"-73.6","46":"-74.1","47":"-74.6","48":"-74.9","49":"-75.3","50":"-75.5","51":"-75.8","52":"-75.9","53":"-76.0","54":"-76.1","55":"-76.1","56":"-76.1","57":"-76.0","58":"-75.8","59":"-75.6","60":"-75.4","61":"-75.1","62":"-74.7","63":"-74.3","64":"-73.9","65":"-73.3","66":"-72.8","67":"-72.1","68":"-71.5","69":"-70.7","70":"-70.0","71":"-69.1","72":"-68.2","73":"-67.3","74":"-66.3","75":"-65.3","76":"-64.2","77":"-63.0","78":"-61.8","79":"-60.6","80":"-59.3","81":"-57.9","82":"-56.5","83":"-55.0","84":"-53.5","85":"-51.9","86":"-50.3","87":"-48.6","88":"-46.9","89":"-45.1","90":"-43.3","91":"-41.4"},{"1":"0","2":"-2.86","3":"-5.68","4":"-8.44","5":"-11.1","6":"-13.8","7":"-16.4","8":"-18.9","9":"-21.4","10":"-23.9","11":"-26.3","12":"-28.6","13":"-30.9","14":"-33.1","15":"-35.3","16":"-37.4","17":"-39.5","18":"-41.5","19":"-43.5","20":"-45.4","21":"-47.3","22":"-49.1","23":"-50.8","24":"-52.5","25":"-54.2","26":"-55.8","27":"-57.3","28":"-58.8","29":"-60.3","30":"-61.7","31":"-63.0","32":"-64.3","33":"-65.5","34":"-66.7","35":"-67.8","36":"-68.9","37":"-69.9","38":"-70.9","39":"-71.8","40":"-72.6","41":"-73.4","42":"-74.2","43":"-74.9","44":"-75.5","45":"-76.1","46":"-76.7","47":"-77.2","48":"-77.6","49":"-78.0","50":"-78.3","51":"-78.6","52":"-78.8","53":"-79.0","54":"-79.1","55":"-79.2","56":"-79.2","57":"-79.2","58":"-79.1","59":"-78.9","60":"-78.7","61":"-78.5","62":"-78.2","63":"-77.8","64":"-77.4","65":"-77.0","66":"-76.5","67":"-75.9","68":"-75.3","69":"-74.6","70":"-73.9","71":"-73.1","72":"-72.3","73":"-71.4","74":"-70.5","75":"-69.5","76":"-68.4","77":"-67.3","78":"-66.2","79":"-65.0","80":"-63.7","81":"-62.4","82":"-61.1","83":"-59.7","84":"-58.2","85":"-56.7","86":"-55.1","87":"-53.5","88":"-51.8","89":"-50.1","90":"-48.3","91":"-46.5"},{"1":"0","2":"-2.92","3":"-5.79","4":"-8.61","5":"-11.4","6":"-14.1","7":"-16.7","8":"-19.3","9":"-21.9","10":"-24.4","11":"-26.8","12":"-29.2","13":"-31.6","14":"-33.9","15":"-36.1","16":"-38.3","17":"-40.4","18":"-42.5","19":"-44.5","20":"-46.5","21":"-48.4","22":"-50.3","23":"-52.1","24":"-53.8","25":"-55.6","26":"-57.2","27":"-58.8","28":"-60.4","29":"-61.9","30":"-63.3","31":"-64.7","32":"-66.0","33":"-67.3","34":"-68.5","35":"-69.7","36":"-70.9","37":"-71.9","38":"-73.0","39":"-73.9","40":"-74.8","41":"-75.7","42":"-76.5","43":"-77.3","44":"-78.0","45":"-78.6","46":"-79.2","47":"-79.8","48":"-80.3","49":"-80.7","50":"-81.1","51":"-81.4","52":"-81.7","53":"-82.0","54":"-82.1","55":"-82.3","56":"-82.3","57":"-82.3","58":"-82.3","59":"-82.2","60":"-82.1","61":"-81.9","62":"-81.7","63":"-81.4","64":"-81.0","65":"-80.6","66":"-80.1","67":"-79.6","68":"-79.1","69":"-78.5","70":"-77.8","71":"-77.1","72":"-76.3","73":"-75.5","74":"-74.6","75":"-73.7","76":"-72.7","77":"-71.7","78":"-70.6","79":"-69.4","80":"-68.2","81":"-67.0","82":"-65.7","83":"-64.3","84":"-62.9","85":"-61.5","86":"-60.0","87":"-58.4","88":"-56.8","89":"-55.1","90":"-53.4","91":"-51.6"},{"1":"0","2":"-2.98","3":"-5.90","4":"-8.78","5":"-11.6","6":"-14.4","7":"-17.1","8":"-19.7","9":"-22.3","10":"-24.9","11":"-27.4","12":"-29.9","13":"-32.3","14":"-34.6","15":"-36.9","16":"-39.1","17":"-41.3","18":"-43.5","19":"-45.5","20":"-47.6","21":"-49.5","22":"-51.5","23":"-53.3","24":"-55.2","25":"-56.9","26":"-58.6","27":"-60.3","28":"-61.9","29":"-63.4","30":"-64.9","31":"-66.4","32":"-67.8","33":"-69.1","34":"-70.4","35":"-71.7","36":"-72.8","37":"-74.0","38":"-75.1","39":"-76.1","40":"-77.1","41":"-78.0","42":"-78.8","43":"-79.7","44":"-80.4","45":"-81.1","46":"-81.8","47":"-82.4","48":"-82.9","49":"-83.4","50":"-83.9","51":"-84.3","52":"-84.6","53":"-84.9","54":"-85.1","55":"-85.3","56":"-85.5","57":"-85.5","58":"-85.6","59":"-85.5","60":"-85.4","61":"-85.3","62":"-85.1","63":"-84.9","64":"-84.6","65":"-84.2","66":"-83.8","67":"-83.4","68":"-82.9","69":"-82.3","70":"-81.7","71":"-81.1","72":"-80.3","73":"-79.6","74":"-78.8","75":"-77.9","76":"-77.0","77":"-76.0","78":"-74.9","79":"-73.9","80":"-72.7","81":"-71.5","82":"-70.3","83":"-69.0","84":"-67.6","85":"-66.2","86":"-64.8","87":"-63.3","88":"-61.7","89":"-60.1","90":"-58.4","91":"-56.7"},{"1":"0","2":"-3.04","3":"-6.02","4":"-8.95","5":"-11.8","6":"-14.6","7":"-17.4","8":"-20.1","9":"-22.8","10":"-25.4","11":"-28.0","12":"-30.5","13":"-32.9","14":"-35.3","15":"-37.7","16":"-40.0","17":"-42.2","18":"-44.4","19":"-46.6","20":"-48.6","21":"-50.7","22":"-52.7","23":"-54.6","24":"-56.5","25":"-58.3","26":"-60.0","27":"-61.8","28":"-63.4","29":"-65.0","30":"-66.6","31":"-68.1","32":"-69.6","33":"-71.0","34":"-72.3","35":"-73.6","36":"-74.8","37":"-76.0","38":"-77.2","39":"-78.2","40":"-79.3","41":"-80.2","42":"-81.2","43":"-82.0","44":"-82.9","45":"-83.6","46":"-84.3","47":"-85.0","48":"-85.6","49":"-86.2","50":"-86.7","51":"-87.1","52":"-87.5","53":"-87.9","54":"-88.2","55":"-88.4","56":"-88.6","57":"-88.7","58":"-88.8","59":"-88.8","60":"-88.8","61":"-88.7","62":"-88.6","63":"-88.4","64":"-88.2","65":"-87.9","66":"-87.5","67":"-87.1","68":"-86.7","69":"-86.2","70":"-85.6","71":"-85.0","72":"-84.4","73":"-83.7","74":"-82.9","75":"-82.1","76":"-81.2","77":"-80.3","78":"-79.3","79":"-78.3","80":"-77.2","81":"-76.1","82":"-74.9","83":"-73.7","84":"-72.4","85":"-71.0","86":"-69.6","87":"-68.2","88":"-66.7","89":"-65.1","90":"-63.5","91":"-61.8"},{"1":"0","2":"-3.09","3":"-6.13","4":"-9.12","5":"-12.1","6":"-14.9","7":"-17.8","8":"-20.5","9":"-23.3","10":"-25.9","11":"-28.5","12":"-31.1","13":"-33.6","14":"-36.1","15":"-38.5","16":"-40.8","17":"-43.1","18":"-45.4","19":"-47.6","20":"-49.7","21":"-51.8","22":"-53.8","23":"-55.8","24":"-57.8","25":"-59.6","26":"-61.5","27":"-63.2","28":"-65.0","29":"-66.6","30":"-68.2","31":"-69.8","32":"-71.3","33":"-72.8","34":"-74.2","35":"-75.5","36":"-76.8","37":"-78.1","38":"-79.3","39":"-80.4","40":"-81.5","41":"-82.5","42":"-83.5","43":"-84.4","44":"-85.3","45":"-86.1","46":"-86.9","47":"-87.6","48":"-88.3","49":"-88.9","50":"-89.5","51":"-90.0","52":"-90.4","53":"-90.8","54":"-91.2","55":"-91.5","56":"-91.7","57":"-91.9","58":"-92.0","59":"-92.1","60":"-92.1","61":"-92.1","62":"-92.0","63":"-91.9","64":"-91.7","65":"-91.5","66":"-91.2","67":"-90.9","68":"-90.5","69":"-90.1","70":"-89.6","71":"-89.0","72":"-88.4","73":"-87.8","74":"-87.0","75":"-86.3","76":"-85.5","77":"-84.6","78":"-83.7","79":"-82.7","80":"-81.7","81":"-80.6","82":"-79.5","83":"-78.3","84":"-77.1","85":"-75.8","86":"-74.4","87":"-73.1","88":"-71.6","89":"-70.1","90":"-68.6","91":"-67.0"},{"1":"0","2":"-3.15","3":"-6.24","4":"-9.29","5":"-12.3","6":"-15.2","7":"-18.1","8":"-20.9","9":"-23.7","10":"-26.4","11":"-29.1","12":"-31.7","13":"-34.3","14":"-36.8","15":"-39.3","16":"-41.7","17":"-44.0","18":"-46.4","19":"-48.6","20":"-50.8","21":"-52.9","22":"-55.0","23":"-57.1","24":"-59.1","25":"-61.0","26":"-62.9","27":"-64.7","28":"-66.5","29":"-68.2","30":"-69.9","31":"-71.5","32":"-73.1","33":"-74.6","34":"-76.0","35":"-77.5","36":"-78.8","37":"-80.1","38":"-81.4","39":"-82.6","40":"-83.7","41":"-84.8","42":"-85.8","43":"-86.8","44":"-87.7","45":"-88.6","46":"-89.5","47":"-90.2","48":"-91.0","49":"-91.6","50":"-92.2","51":"-92.8","52":"-93.3","53":"-93.8","54":"-94.2","55":"-94.5","56":"-94.8","57":"-95.1","58":"-95.3","59":"-95.4","60":"-95.5","61":"-95.5","62":"-95.5","63":"-95.4","64":"-95.3","65":"-95.1","66":"-94.9","67":"-94.6","68":"-94.3","69":"-93.9","70":"-93.5","71":"-93.0","72":"-92.4","73":"-91.8","74":"-91.2","75":"-90.5","76":"-89.7","77":"-88.9","78":"-88.1","79":"-87.2","80":"-86.2","81":"-85.2","82":"-84.1","83":"-83.0","84":"-81.8","85":"-80.6","86":"-79.3","87":"-77.9","88":"-76.5","89":"-75.1","90":"-73.6","91":"-72.1"},{"1":"0","2":"-3.21","3":"-6.36","4":"-9.46","5":"-12.5","6":"-15.5","7":"-18.4","8":"-21.3","9":"-24.2","10":"-26.9","11":"-29.7","12":"-32.4","13":"-35.0","14":"-37.6","15":"-40.1","16":"-42.5","17":"-45.0","18":"-47.3","19":"-49.6","20":"-51.9","21":"-54.1","22":"-56.2","23":"-58.3","24":"-60.4","25":"-62.4","26":"-64.3","27":"-66.2","28":"-68.0","29":"-69.8","30":"-71.5","31":"-73.2","32":"-74.8","33":"-76.4","34":"-77.9","35":"-79.4","36":"-80.8","37":"-82.2","38":"-83.5","39":"-84.7","40":"-85.9","41":"-87.1","42":"-88.2","43":"-89.2","44":"-90.2","45":"-91.1","46":"-92.0","47":"-92.8","48":"-93.6","49":"-94.3","50":"-95.0","51":"-95.6","52":"-96.2","53":"-96.7","54":"-97.2","55":"-97.6","56":"-97.9","57":"-98.3","58":"-98.5","59":"-98.7","60":"-98.8","61":"-98.9","62":"-99.0","63":"-99.0","64":"-98.9","65":"-98.8","66":"-98.6","67":"-98.4","68":"-98.1","69":"-97.8","70":"-97.4","71":"-97.0","72":"-96.5","73":"-95.9","74":"-95.3","75":"-94.7","76":"-94.0","77":"-93.2","78":"-92.4","79":"-91.6","80":"-90.7","81":"-89.7","82":"-88.7","83":"-87.6","84":"-86.5","85":"-85.3","86":"-84.1","87":"-82.8","88":"-81.5","89":"-80.1","90":"-78.7","91":"-77.2"},{"1":"0","2":"-3.26","3":"-6.47","4":"-9.63","5":"-12.7","6":"-15.8","7":"-18.8","8":"-21.7","9":"-24.6","10":"-27.5","11":"-30.2","12":"-33.0","13":"-35.7","14":"-38.3","15":"-40.9","16":"-43.4","17":"-45.9","18":"-48.3","19":"-50.6","20":"-53.0","21":"-55.2","22":"-57.4","23":"-59.6","24":"-61.7","25":"-63.7","26":"-65.7","27":"-67.7","28":"-69.6","29":"-71.4","30":"-73.2","31":"-74.9","32":"-76.6","33":"-78.2","34":"-79.8","35":"-81.3","36":"-82.8","37":"-84.2","38":"-85.6","39":"-86.9","40":"-88.1","41":"-89.3","42":"-90.5","43":"-91.6","44":"-92.6","45":"-93.6","46":"-94.6","47":"-95.5","48":"-96.3","49":"-97.1","50":"-97.8","51":"-98.5","52":"-99.1","53":"-99.7","54":"-100.2","55":"-100.7","56":"-101.1","57":"-101.4","58":"-101.7","59":"-102.0","60":"-102.2","61":"-102.3","62":"-102.4","63":"-102.5","64":"-102.5","65":"-102.4","66":"-102.3","67":"-102.1","68":"-101.9","69":"-101.6","70":"-101.3","71":"-100.9","72":"-100.5","73":"-100.0","74":"-99.5","75":"-98.9","76":"-98.3","77":"-97.6","78":"-96.8","79":"-96.0","80":"-95.2","81":"-94.3","82":"-93.3","83":"-92.3","84":"-91.2","85":"-90.1","86":"-88.9","87":"-87.7","88":"-86.4","89":"-85.1","90":"-83.7","91":"-82.3"},{"1":"0","2":"-3.32","3":"-6.59","4":"-9.80","5":"-13.0","6":"-16.1","7":"-19.1","8":"-22.1","9":"-25.1","10":"-28.0","11":"-30.8","12":"-33.6","13":"-36.3","14":"-39.0","15":"-41.7","16":"-44.2","17":"-46.8","18":"-49.2","19":"-51.7","20":"-54.0","21":"-56.4","22":"-58.6","23":"-60.8","24":"-63.0","25":"-65.1","26":"-67.1","27":"-69.1","28":"-71.1","29":"-73.0","30":"-74.8","31":"-76.6","32":"-78.4","33":"-80.0","34":"-81.7","35":"-83.2","36":"-84.8","37":"-86.2","38":"-87.7","39":"-89.0","40":"-90.3","41":"-91.6","42":"-92.8","43":"-94.0","44":"-95.1","45":"-96.1","46":"-97.1","47":"-98.1","48":"-99.0","49":"-99.8","50":"-100.6","51":"-101.3","52":"-102.0","53":"-102.6","54":"-103.2","55":"-103.7","56":"-104.2","57":"-104.6","58":"-105.0","59":"-105.3","60":"-105.5","61":"-105.8","62":"-105.9","63":"-106.0","64":"-106.1","65":"-106.1","66":"-106.0","67":"-105.9","68":"-105.7","69":"-105.5","70":"-105.2","71":"-104.9","72":"-104.5","73":"-104.1","74":"-103.6","75":"-103.1","76":"-102.5","77":"-101.9","78":"-101.2","79":"-100.4","80":"-99.6","81":"-98.8","82":"-97.9","83":"-96.9","84":"-95.9","85":"-94.9","86":"-93.8","87":"-92.6","88":"-91.4","89":"-90.1","90":"-88.8","91":"-87.4"},{"1":"0","2":"-3.38","3":"-6.70","4":"-9.97","5":"-13.2","6":"-16.4","7":"-19.5","8":"-22.5","9":"-25.5","10":"-28.5","11":"-31.4","12":"-34.2","13":"-37.0","14":"-39.8","15":"-42.5","16":"-45.1","17":"-47.7","18":"-50.2","19":"-52.7","20":"-55.1","21":"-57.5","22":"-59.8","23":"-62.1","24":"-64.3","25":"-66.5","26":"-68.6","27":"-70.6","28":"-72.6","29":"-74.6","30":"-76.5","31":"-78.3","32":"-80.1","33":"-81.9","34":"-83.5","35":"-85.2","36":"-86.8","37":"-88.3","38":"-89.8","39":"-91.2","40":"-92.6","41":"-93.9","42":"-95.1","43":"-96.4","44":"-97.5","45":"-98.6","46":"-99.7","47":"-100.7","48":"-101.6","49":"-102.5","50":"-103.4","51":"-104.2","52":"-104.9","53":"-105.6","54":"-106.2","55":"-106.8","56":"-107.3","57":"-107.8","58":"-108.2","59":"-108.6","60":"-108.9","61":"-109.2","62":"-109.4","63":"-109.5","64":"-109.6","65":"-109.7","66":"-109.7","67":"-109.6","68":"-109.5","69":"-109.4","70":"-109.2","71":"-108.9","72":"-108.6","73":"-108.2","74":"-107.8","75":"-107.3","76":"-106.8","77":"-106.2","78":"-105.6","79":"-104.9","80":"-104.1","81":"-103.3","82":"-102.5","83":"-101.6","84":"-100.6","85":"-99.6","86":"-98.6","87":"-97.5","88":"-96.3","89":"-95.1","90":"-93.8","91":"-92.5"},{"1":"0","2":"-3.43","3":"-6.81","4":"-10.14","5":"-13.4","6":"-16.6","7":"-19.8","8":"-22.9","9":"-26.0","10":"-29.0","11":"-32.0","12":"-34.9","13":"-37.7","14":"-40.5","15":"-43.3","16":"-45.9","17":"-48.6","18":"-51.2","19":"-53.7","20":"-56.2","21":"-58.6","22":"-61.0","23":"-63.3","24":"-65.6","25":"-67.8","26":"-70.0","27":"-72.1","28":"-74.2","29":"-76.2","30":"-78.1","31":"-80.0","32":"-81.9","33":"-83.7","34":"-85.4","35":"-87.1","36":"-88.7","37":"-90.3","38":"-91.9","39":"-93.3","40":"-94.8","41":"-96.2","42":"-97.5","43":"-98.7","44":"-100.0","45":"-101.1","46":"-102.2","47":"-103.3","48":"-104.3","49":"-105.3","50":"-106.2","51":"-107.0","52":"-107.8","53":"-108.5","54":"-109.2","55":"-109.9","56":"-110.4","57":"-111.0","58":"-111.5","59":"-111.9","60":"-112.2","61":"-112.6","62":"-112.8","63":"-113.1","64":"-113.2","65":"-113.3","66":"-113.4","67":"-113.4","68":"-113.3","69":"-113.2","70":"-113.1","71":"-112.9","72":"-112.6","73":"-112.3","74":"-111.9","75":"-111.5","76":"-111.0","77":"-110.5","78":"-109.9","79":"-109.3","80":"-108.6","81":"-107.9","82":"-107.1","83":"-106.3","84":"-105.4","85":"-104.4","86":"-103.4","87":"-102.4","88":"-101.3","89":"-100.1","90":"-98.9","91":"-97.6"},{"1":"0","2":"-3.49","3":"-6.93","4":"-10.31","5":"-13.6","6":"-16.9","7":"-20.1","8":"-23.3","9":"-26.4","10":"-29.5","11":"-32.5","12":"-35.5","13":"-38.4","14":"-41.2","15":"-44.1","16":"-46.8","17":"-49.5","18":"-52.1","19":"-54.7","20":"-57.3","21":"-59.8","22":"-62.2","23":"-64.6","24":"-66.9","25":"-69.2","26":"-71.4","27":"-73.6","28":"-75.7","29":"-77.8","30":"-79.8","31":"-81.7","32":"-83.6","33":"-85.5","34":"-87.3","35":"-89.0","36":"-90.7","37":"-92.4","38":"-94.0","39":"-95.5","40":"-97.0","41":"-98.4","42":"-99.8","43":"-101.1","44":"-102.4","45":"-103.6","46":"-104.8","47":"-105.9","48":"-107.0","49":"-108.0","50":"-108.9","51":"-109.8","52":"-110.7","53":"-111.5","54":"-112.2","55":"-112.9","56":"-113.6","57":"-114.2","58":"-114.7","59":"-115.2","60":"-115.6","61":"-116.0","62":"-116.3","63":"-116.6","64":"-116.8","65":"-117.0","66":"-117.1","67":"-117.1","68":"-117.1","69":"-117.1","70":"-117.0","71":"-116.8","72":"-116.6","73":"-116.4","74":"-116.1","75":"-115.7","76":"-115.3","77":"-114.8","78":"-114.3","79":"-113.7","80":"-113.1","81":"-112.4","82":"-111.7","83":"-110.9","84":"-110.1","85":"-109.2","86":"-108.2","87":"-107.2","88":"-106.2","89":"-105.1","90":"-103.9","91":"-102.7"},{"1":"0","2":"-3.55","3":"-7.04","4":"-10.48","5":"-13.9","6":"-17.2","7":"-20.5","8":"-23.7","9":"-26.9","10":"-30.0","11":"-33.1","12":"-36.1","13":"-39.1","14":"-42.0","15":"-44.8","16":"-47.7","17":"-50.4","18":"-53.1","19":"-55.8","20":"-58.4","21":"-60.9","22":"-63.4","23":"-65.8","24":"-68.2","25":"-70.5","26":"-72.8","27":"-75.1","28":"-77.2","29":"-79.4","30":"-81.4","31":"-83.4","32":"-85.4","33":"-87.3","34":"-89.2","35":"-91.0","36":"-92.7","37":"-94.4","38":"-96.1","39":"-97.7","40":"-99.2","41":"-100.7","42":"-102.1","43":"-103.5","44":"-104.8","45":"-106.1","46":"-107.3","47":"-108.5","48":"-109.6","49":"-110.7","50":"-111.7","51":"-112.7","52":"-113.6","53":"-114.4","54":"-115.2","55":"-116.0","56":"-116.7","57":"-117.3","58":"-117.9","59":"-118.5","60":"-119.0","61":"-119.4","62":"-119.8","63":"-120.1","64":"-120.4","65":"-120.6","66":"-120.8","67":"-120.9","68":"-120.9","69":"-121.0","70":"-120.9","71":"-120.8","72":"-120.7","73":"-120.5","74":"-120.2","75":"-119.9","76":"-119.6","77":"-119.1","78":"-118.7","79":"-118.2","80":"-117.6","81":"-117.0","82":"-116.3","83":"-115.6","84":"-114.8","85":"-114.0","86":"-113.1","87":"-112.1","88":"-111.1","89":"-110.1","90":"-109.0","91":"-107.8"},{"1":"0","2":"-3.60","3":"-7.15","4":"-10.65","5":"-14.1","6":"-17.5","7":"-20.8","8":"-24.1","9":"-27.3","10":"-30.5","11":"-33.7","12":"-36.7","13":"-39.8","14":"-42.7","15":"-45.6","16":"-48.5","17":"-51.3","18":"-54.1","19":"-56.8","20":"-59.4","21":"-62.0","22":"-64.6","23":"-67.1","24":"-69.5","25":"-71.9","26":"-74.2","27":"-76.5","28":"-78.8","29":"-80.9","30":"-83.1","31":"-85.1","32":"-87.2","33":"-89.1","34":"-91.0","35":"-92.9","36":"-94.7","37":"-96.5","38":"-98.2","39":"-99.8","40":"-101.4","41":"-103.0","42":"-104.5","43":"-105.9","44":"-107.3","45":"-108.6","46":"-109.9","47":"-111.1","48":"-112.3","49":"-113.4","50":"-114.5","51":"-115.5","52":"-116.5","53":"-117.4","54":"-118.3","55":"-119.1","56":"-119.8","57":"-120.5","58":"-121.2","59":"-121.8","60":"-122.3","61":"-122.8","62":"-123.2","63":"-123.6","64":"-123.9","65":"-124.2","66":"-124.5","67":"-124.6","68":"-124.7","69":"-124.8","70":"-124.8","71":"-124.8","72":"-124.7","73":"-124.6","74":"-124.4","75":"-124.1","76":"-123.8","77":"-123.5","78":"-123.1","79":"-122.6","80":"-122.1","81":"-121.5","82":"-120.9","83":"-120.2","84":"-119.5","85":"-118.7","86":"-117.9","87":"-117.0","88":"-116.1","89":"-115.1","90":"-114.1","91":"-113.0"},{"1":"0","2":"-3.66","3":"-7.27","4":"-10.82","5":"-14.3","6":"-17.8","7":"-21.2","8":"-24.5","9":"-27.8","10":"-31.0","11":"-34.2","12":"-37.4","13":"-40.4","14":"-43.5","15":"-46.4","16":"-49.4","17":"-52.2","18":"-55.0","19":"-57.8","20":"-60.5","21":"-63.2","22":"-65.8","23":"-68.3","24":"-70.8","25":"-73.3","26":"-75.7","27":"-78.0","28":"-80.3","29":"-82.5","30":"-84.7","31":"-86.8","32":"-88.9","33":"-90.9","34":"-92.9","35":"-94.8","36":"-96.7","37":"-98.5","38":"-100.3","39":"-102.0","40":"-103.6","41":"-105.2","42":"-106.8","43":"-108.3","44":"-109.7","45":"-111.1","46":"-112.5","47":"-113.7","48":"-115.0","49":"-116.2","50":"-117.3","51":"-118.4","52":"-119.4","53":"-120.3","54":"-121.3","55":"-122.1","56":"-122.9","57":"-123.7","58":"-124.4","59":"-125.1","60":"-125.7","61":"-126.2","62":"-126.7","63":"-127.1","64":"-127.5","65":"-127.9","66":"-128.1","67":"-128.4","68":"-128.6","69":"-128.7","70":"-128.7","71":"-128.8","72":"-128.7","73":"-128.6","74":"-128.5","75":"-128.3","76":"-128.1","77":"-127.8","78":"-127.4","79":"-127.0","80":"-126.6","81":"-126.1","82":"-125.5","83":"-124.9","84":"-124.2","85":"-123.5","86":"-122.7","87":"-121.9","88":"-121.0","89":"-120.1","90":"-119.1","91":"-118.1"},{"1":"0","2":"-3.72","3":"-7.38","4":"-10.99","5":"-14.5","6":"-18.1","7":"-21.5","8":"-24.9","9":"-28.3","10":"-31.6","11":"-34.8","12":"-38.0","13":"-41.1","14":"-44.2","15":"-47.2","16":"-50.2","17":"-53.1","18":"-56.0","19":"-58.8","20":"-61.6","21":"-64.3","22":"-67.0","23":"-69.6","24":"-72.1","25":"-74.6","26":"-77.1","27":"-79.5","28":"-81.8","29":"-84.1","30":"-86.4","31":"-88.5","32":"-90.7","33":"-92.8","34":"-94.8","35":"-96.8","36":"-98.7","37":"-100.6","38":"-102.4","39":"-104.1","40":"-105.9","41":"-107.5","42":"-109.1","43":"-110.7","44":"-112.2","45":"-113.6","46":"-115.0","47":"-116.4","48":"-117.6","49":"-118.9","50":"-120.1","51":"-121.2","52":"-122.3","53":"-123.3","54":"-124.3","55":"-125.2","56":"-126.1","57":"-126.9","58":"-127.6","59":"-128.3","60":"-129.0","61":"-129.6","62":"-130.2","63":"-130.7","64":"-131.1","65":"-131.5","66":"-131.8","67":"-132.1","68":"-132.4","69":"-132.5","70":"-132.7","71":"-132.7","72":"-132.8","73":"-132.7","74":"-132.7","75":"-132.5","76":"-132.3","77":"-132.1","78":"-131.8","79":"-131.5","80":"-131.1","81":"-130.6","82":"-130.1","83":"-129.5","84":"-128.9","85":"-128.3","86":"-127.6","87":"-126.8","88":"-126.0","89":"-125.1","90":"-124.2","91":"-123.2"},{"1":"0","2":"-3.77","3":"-7.49","4":"-11.16","5":"-14.8","6":"-18.3","7":"-21.8","8":"-25.3","9":"-28.7","10":"-32.1","11":"-35.4","12":"-38.6","13":"-41.8","14":"-44.9","15":"-48.0","16":"-51.1","17":"-54.0","18":"-57.0","19":"-59.8","20":"-62.7","21":"-65.4","22":"-68.2","23":"-70.8","24":"-73.4","25":"-76.0","26":"-78.5","27":"-81.0","28":"-83.4","29":"-85.7","30":"-88.0","31":"-90.3","32":"-92.4","33":"-94.6","34":"-96.7","35":"-98.7","36":"-100.7","37":"-102.6","38":"-104.5","39":"-106.3","40":"-108.1","41":"-109.8","42":"-111.4","43":"-113.1","44":"-114.6","45":"-116.1","46":"-117.6","47":"-119.0","48":"-120.3","49":"-121.6","50":"-122.8","51":"-124.0","52":"-125.2","53":"-126.3","54":"-127.3","55":"-128.3","56":"-129.2","57":"-130.1","58":"-130.9","59":"-131.6","60":"-132.4","61":"-133.0","62":"-133.6","63":"-134.2","64":"-134.7","65":"-135.1","66":"-135.5","67":"-135.9","68":"-136.2","69":"-136.4","70":"-136.6","71":"-136.7","72":"-136.8","73":"-136.8","74":"-136.8","75":"-136.7","76":"-136.6","77":"-136.4","78":"-136.2","79":"-135.9","80":"-135.5","81":"-135.1","82":"-134.7","83":"-134.2","84":"-133.6","85":"-133.0","86":"-132.4","87":"-131.7","88":"-130.9","89":"-130.1","90":"-129.2","91":"-128.3"},{"1":"0","2":"-3.83","3":"-7.61","4":"-11.33","5":"-15.0","6":"-18.6","7":"-22.2","8":"-25.7","9":"-29.2","10":"-32.6","11":"-35.9","12":"-39.2","13":"-42.5","14":"-45.7","15":"-48.8","16":"-51.9","17":"-55.0","18":"-57.9","19":"-60.9","20":"-63.8","21":"-66.6","22":"-69.4","23":"-72.1","24":"-74.7","25":"-77.4","26":"-79.9","27":"-82.4","28":"-84.9","29":"-87.3","30":"-89.7","31":"-92.0","32":"-94.2","33":"-96.4","34":"-98.5","35":"-100.6","36":"-102.7","37":"-104.6","38":"-106.6","39":"-108.5","40":"-110.3","41":"-112.1","42":"-113.8","43":"-115.4","44":"-117.1","45":"-118.6","46":"-120.1","47":"-121.6","48":"-123.0","49":"-124.3","50":"-125.6","51":"-126.9","52":"-128.1","53":"-129.2","54":"-130.3","55":"-131.3","56":"-132.3","57":"-133.2","58":"-134.1","59":"-134.9","60":"-135.7","61":"-136.4","62":"-137.1","63":"-137.7","64":"-138.3","65":"-138.8","66":"-139.2","67":"-139.6","68":"-140.0","69":"-140.3","70":"-140.5","71":"-140.7","72":"-140.8","73":"-140.9","74":"-140.9","75":"-140.9","76":"-140.9","77":"-140.7","78":"-140.5","79":"-140.3","80":"-140.0","81":"-139.7","82":"-139.3","83":"-138.9","84":"-138.4","85":"-137.8","86":"-137.2","87":"-136.6","88":"-135.8","89":"-135.1","90":"-134.3","91":"-133.4"},{"1":"0","2":"-3.89","3":"-7.72","4":"-11.50","5":"-15.2","6":"-18.9","7":"-22.5","8":"-26.1","9":"-29.6","10":"-33.1","11":"-36.5","12":"-39.9","13":"-43.2","14":"-46.4","15":"-49.6","16":"-52.8","17":"-55.9","18":"-58.9","19":"-61.9","20":"-64.8","21":"-67.7","22":"-70.5","23":"-73.3","24":"-76.1","25":"-78.7","26":"-81.3","27":"-83.9","28":"-86.4","29":"-88.9","30":"-91.3","31":"-93.7","32":"-96.0","33":"-98.2","34":"-100.4","35":"-102.6","36":"-104.7","37":"-106.7","38":"-108.7","39":"-110.6","40":"-112.5","41":"-114.3","42":"-116.1","43":"-117.8","44":"-119.5","45":"-121.1","46":"-122.7","47":"-124.2","48":"-125.7","49":"-127.1","50":"-128.4","51":"-129.7","52":"-131.0","53":"-132.2","54":"-133.3","55":"-134.4","56":"-135.4","57":"-136.4","58":"-137.4","59":"-138.2","60":"-139.1","61":"-139.8","62":"-140.6","63":"-141.2","64":"-141.8","65":"-142.4","66":"-142.9","67":"-143.4","68":"-143.8","69":"-144.1","70":"-144.4","71":"-144.7","72":"-144.9","73":"-145.0","74":"-145.1","75":"-145.1","76":"-145.1","77":"-145.0","78":"-144.9","79":"-144.7","80":"-144.5","81":"-144.2","82":"-143.9","83":"-143.5","84":"-143.1","85":"-142.6","86":"-142.0","87":"-141.4","88":"-140.8","89":"-140.1","90":"-139.3","91":"-138.5"},{"1":"0","2":"-3.94","3":"-7.83","4":"-11.67","5":"-15.5","6":"-19.2","7":"-22.9","8":"-26.5","9":"-30.1","10":"-33.6","11":"-37.1","12":"-40.5","13":"-43.8","14":"-47.2","15":"-50.4","16":"-53.6","17":"-56.8","18":"-59.9","19":"-62.9","20":"-65.9","21":"-68.9","22":"-71.7","23":"-74.6","24":"-77.4","25":"-80.1","26":"-82.8","27":"-85.4","28":"-88.0","29":"-90.5","30":"-92.9","31":"-95.4","32":"-97.7","33":"-100.0","34":"-102.3","35":"-104.5","36":"-106.6","37":"-108.7","38":"-110.8","39":"-112.8","40":"-114.7","41":"-116.6","42":"-118.4","43":"-120.2","44":"-121.9","45":"-123.6","46":"-125.2","47":"-126.8","48":"-128.3","49":"-129.8","50":"-131.2","51":"-132.6","52":"-133.9","53":"-135.1","54":"-136.3","55":"-137.5","56":"-138.6","57":"-139.6","58":"-140.6","59":"-141.5","60":"-142.4","61":"-143.2","62":"-144.0","63":"-144.7","64":"-145.4","65":"-146.0","66":"-146.6","67":"-147.1","68":"-147.6","69":"-148.0","70":"-148.3","71":"-148.6","72":"-148.9","73":"-149.1","74":"-149.2","75":"-149.3","76":"-149.4","77":"-149.4","78":"-149.3","79":"-149.2","80":"-149.0","81":"-148.8","82":"-148.5","83":"-148.2","84":"-147.8","85":"-147.4","86":"-146.9","87":"-146.3","88":"-145.7","89":"-145.1","90":"-144.4","91":"-143.6"},{"1":"0","2":"-4.00","3":"-7.95","4":"-11.84","5":"-15.7","6":"-19.5","7":"-23.2","8":"-26.9","9":"-30.5","10":"-34.1","11":"-37.6","12":"-41.1","13":"-44.5","14":"-47.9","15":"-51.2","16":"-54.5","17":"-57.7","18":"-60.8","19":"-63.9","20":"-67.0","21":"-70.0","22":"-72.9","23":"-75.8","24":"-78.7","25":"-81.5","26":"-84.2","27":"-86.9","28":"-89.5","29":"-92.1","30":"-94.6","31":"-97.1","32":"-99.5","33":"-101.8","34":"-104.2","35":"-106.4","36":"-108.6","37":"-110.8","38":"-112.9","39":"-114.9","40":"-116.9","41":"-118.9","42":"-120.8","43":"-122.6","44":"-124.4","45":"-126.1","46":"-127.8","47":"-129.4","48":"-131.0","49":"-132.5","50":"-134.0","51":"-135.4","52":"-136.8","53":"-138.1","54":"-139.3","55":"-140.5","56":"-141.7","57":"-142.8","58":"-143.8","59":"-144.8","60":"-145.8","61":"-146.6","62":"-147.5","63":"-148.3","64":"-149.0","65":"-149.7","66":"-150.3","67":"-150.9","68":"-151.4","69":"-151.8","70":"-152.3","71":"-152.6","72":"-152.9","73":"-153.2","74":"-153.4","75":"-153.5","76":"-153.6","77":"-153.7","78":"-153.7","79":"-153.6","80":"-153.5","81":"-153.3","82":"-153.1","83":"-152.8","84":"-152.5","85":"-152.1","86":"-151.7","87":"-151.2","88":"-150.7","89":"-150.1","90":"-149.4","91":"-148.7"},{"1":"0","2":"-4.06","3":"-8.06","4":"-12.01","5":"-15.9","6":"-19.8","7":"-23.6","8":"-27.3","9":"-31.0","10":"-34.6","11":"-38.2","12":"-41.7","13":"-45.2","14":"-48.6","15":"-52.0","16":"-55.3","17":"-58.6","18":"-61.8","19":"-65.0","20":"-68.1","21":"-71.1","22":"-74.1","23":"-77.1","24":"-80.0","25":"-82.8","26":"-85.6","27":"-88.3","28":"-91.0","29":"-93.7","30":"-96.2","31":"-98.8","32":"-101.2","33":"-103.7","34":"-106.0","35":"-108.4","36":"-110.6","37":"-112.8","38":"-115.0","39":"-117.1","40":"-119.1","41":"-121.1","42":"-123.1","43":"-125.0","44":"-126.8","45":"-128.6","46":"-130.3","47":"-132.0","48":"-133.7","49":"-135.2","50":"-136.8","51":"-138.2","52":"-139.7","53":"-141.0","54":"-142.3","55":"-143.6","56":"-144.8","57":"-146.0","58":"-147.1","59":"-148.1","60":"-149.1","61":"-150.1","62":"-150.9","63":"-151.8","64":"-152.6","65":"-153.3","66":"-154.0","67":"-154.6","68":"-155.2","69":"-155.7","70":"-156.2","71":"-156.6","72":"-157.0","73":"-157.3","74":"-157.5","75":"-157.7","76":"-157.9","77":"-158.0","78":"-158.0","79":"-158.0","80":"-158.0","81":"-157.9","82":"-157.7","83":"-157.5","84":"-157.2","85":"-156.9","86":"-156.5","87":"-156.1","88":"-155.6","89":"-155.1","90":"-154.5","91":"-153.9"},{"1":"0","2":"-4.11","3":"-8.18","4":"-12.18","5":"-16.1","6":"-20.0","7":"-23.9","8":"-27.7","9":"-31.4","10":"-35.1","11":"-38.8","12":"-42.4","13":"-45.9","14":"-49.4","15":"-52.8","16":"-56.2","17":"-59.5","18":"-62.8","19":"-66.0","20":"-69.1","21":"-72.3","22":"-75.3","23":"-78.3","24":"-81.3","25":"-84.2","26":"-87.0","27":"-89.8","28":"-92.6","29":"-95.3","30":"-97.9","31":"-100.5","32":"-103.0","33":"-105.5","34":"-107.9","35":"-110.3","36":"-112.6","37":"-114.9","38":"-117.1","39":"-119.2","40":"-121.4","41":"-123.4","42":"-125.4","43":"-127.4","44":"-129.3","45":"-131.1","46":"-132.9","47":"-134.6","48":"-136.3","49":"-138.0","50":"-139.5","51":"-141.1","52":"-142.6","53":"-144.0","54":"-145.3","55":"-146.7","56":"-147.9","57":"-149.1","58":"-150.3","59":"-151.4","60":"-152.5","61":"-153.5","62":"-154.4","63":"-155.3","64":"-156.2","65":"-156.9","66":"-157.7","67":"-158.4","68":"-159.0","69":"-159.6","70":"-160.1","71":"-160.6","72":"-161.0","73":"-161.4","74":"-161.7","75":"-161.9","76":"-162.2","77":"-162.3","78":"-162.4","79":"-162.5","80":"-162.5","81":"-162.4","82":"-162.3","83":"-162.1","84":"-161.9","85":"-161.7","86":"-161.3","87":"-161.0","88":"-160.6","89":"-160.1","90":"-159.5","91":"-159.0"},{"1":"0","2":"-4.17","3":"-8.29","4":"-12.35","5":"-16.4","6":"-20.3","7":"-24.2","8":"-28.1","9":"-31.9","10":"-35.6","11":"-39.3","12":"-43.0","13":"-46.6","14":"-50.1","15":"-53.6","16":"-57.0","17":"-60.4","18":"-63.7","19":"-67.0","20":"-70.2","21":"-73.4","22":"-76.5","23":"-79.6","24":"-82.6","25":"-85.5","26":"-88.4","27":"-91.3","28":"-94.1","29":"-96.8","30":"-99.5","31":"-102.2","32":"-104.8","33":"-107.3","34":"-109.8","35":"-112.2","36":"-114.6","37":"-116.9","38":"-119.2","39":"-121.4","40":"-123.6","41":"-125.7","42":"-127.7","43":"-129.8","44":"-131.7","45":"-133.6","46":"-135.5","47":"-137.3","48":"-139.0","49":"-140.7","50":"-142.3","51":"-143.9","52":"-145.4","53":"-146.9","54":"-148.4","55":"-149.7","56":"-151.1","57":"-152.3","58":"-153.5","59":"-154.7","60":"-155.8","61":"-156.9","62":"-157.9","63":"-158.8","64":"-159.7","65":"-160.6","66":"-161.4","67":"-162.1","68":"-162.8","69":"-163.4","70":"-164.0","71":"-164.6","72":"-165.0","73":"-165.5","74":"-165.8","75":"-166.1","76":"-166.4","77":"-166.6","78":"-166.8","79":"-166.9","80":"-166.9","81":"-167.0","82":"-166.9","83":"-166.8","84":"-166.6","85":"-166.4","86":"-166.2","87":"-165.9","88":"-165.5","89":"-165.1","90":"-164.6","91":"-164.1"},{"1":"0","2":"-4.23","3":"-8.40","4":"-12.53","5":"-16.6","6":"-20.6","7":"-24.6","8":"-28.5","9":"-32.3","10":"-36.2","11":"-39.9","12":"-43.6","13":"-47.3","14":"-50.8","15":"-54.4","16":"-57.9","17":"-61.3","18":"-64.7","19":"-68.0","20":"-71.3","21":"-74.5","22":"-77.7","23":"-80.8","24":"-83.9","25":"-86.9","26":"-89.9","27":"-92.8","28":"-95.6","29":"-98.4","30":"-101.2","31":"-103.9","32":"-106.5","33":"-109.1","34":"-111.7","35":"-114.1","36":"-116.6","37":"-119.0","38":"-121.3","39":"-123.6","40":"-125.8","41":"-128.0","42":"-130.1","43":"-132.1","44":"-134.2","45":"-136.1","46":"-138.0","47":"-139.9","48":"-141.7","49":"-143.4","50":"-145.1","51":"-146.8","52":"-148.3","53":"-149.9","54":"-151.4","55":"-152.8","56":"-154.2","57":"-155.5","58":"-156.8","59":"-158.0","60":"-159.2","61":"-160.3","62":"-161.3","63":"-162.4","64":"-163.3","65":"-164.2","66":"-165.1","67":"-165.9","68":"-166.6","69":"-167.3","70":"-167.9","71":"-168.5","72":"-169.1","73":"-169.5","74":"-170.0","75":"-170.3","76":"-170.7","77":"-170.9","78":"-171.2","79":"-171.3","80":"-171.4","81":"-171.5","82":"-171.5","83":"-171.5","84":"-171.4","85":"-171.2","86":"-171.0","87":"-170.7","88":"-170.4","89":"-170.1","90":"-169.7","91":"-169.2"},{"1":"0","2":"-4.28","3":"-8.52","4":"-12.70","5":"-16.8","6":"-20.9","7":"-24.9","8":"-28.9","9":"-32.8","10":"-36.7","11":"-40.5","12":"-44.2","13":"-47.9","14":"-51.6","15":"-55.2","16":"-58.7","17":"-62.2","18":"-65.7","19":"-69.1","20":"-72.4","21":"-75.7","22":"-78.9","23":"-82.1","24":"-85.2","25":"-88.3","26":"-91.3","27":"-94.3","28":"-97.2","29":"-100.0","30":"-102.8","31":"-105.6","32":"-108.3","33":"-110.9","34":"-113.5","35":"-116.1","36":"-118.6","37":"-121.0","38":"-123.4","39":"-125.7","40":"-128.0","41":"-130.2","42":"-132.4","43":"-134.5","44":"-136.6","45":"-138.6","46":"-140.6","47":"-142.5","48":"-144.3","49":"-146.1","50":"-147.9","51":"-149.6","52":"-151.2","53":"-152.8","54":"-154.4","55":"-155.9","56":"-157.3","57":"-158.7","58":"-160.0","59":"-161.3","60":"-162.5","61":"-163.7","62":"-164.8","63":"-165.9","64":"-166.9","65":"-167.8","66":"-168.8","67":"-169.6","68":"-170.4","69":"-171.2","70":"-171.9","71":"-172.5","72":"-173.1","73":"-173.6","74":"-174.1","75":"-174.6","76":"-174.9","77":"-175.3","78":"-175.5","79":"-175.8","80":"-175.9","81":"-176.0","82":"-176.1","83":"-176.1","84":"-176.1","85":"-176.0","86":"-175.8","87":"-175.6","88":"-175.4","89":"-175.1","90":"-174.7","91":"-174.3"},{"1":"0","2":"-4.34","3":"-8.63","4":"-12.87","5":"-17.0","6":"-21.2","7":"-25.3","8":"-29.3","9":"-33.3","10":"-37.2","11":"-41.0","12":"-44.9","13":"-48.6","14":"-52.3","15":"-56.0","16":"-59.6","17":"-63.1","18":"-66.6","19":"-70.1","20":"-73.5","21":"-76.8","22":"-80.1","23":"-83.3","24":"-86.5","25":"-89.6","26":"-92.7","27":"-95.7","28":"-98.7","29":"-101.6","30":"-104.5","31":"-107.3","32":"-110.0","33":"-112.8","34":"-115.4","35":"-118.0","36":"-120.6","37":"-123.1","38":"-125.5","39":"-127.9","40":"-130.2","41":"-132.5","42":"-134.7","43":"-136.9","44":"-139.0","45":"-141.1","46":"-143.1","47":"-145.1","48":"-147.0","49":"-148.9","50":"-150.7","51":"-152.4","52":"-154.1","53":"-155.8","54":"-157.4","55":"-158.9","56":"-160.4","57":"-161.9","58":"-163.3","59":"-164.6","60":"-165.9","61":"-167.1","62":"-168.3","63":"-169.4","64":"-170.5","65":"-171.5","66":"-172.4","67":"-173.4","68":"-174.2","69":"-175.0","70":"-175.8","71":"-176.5","72":"-177.1","73":"-177.7","74":"-178.3","75":"-178.8","76":"-179.2","77":"-179.6","78":"-179.9","79":"-180.2","80":"-180.4","81":"-180.6","82":"-180.7","83":"-180.8","84":"-180.8","85":"-180.7","86":"-180.7","87":"-180.5","88":"-180.3","89":"-180.1","90":"-179.8","91":"-179.4"},{"1":"0","2":"-4.40","3":"-8.74","4":"-13.04","5":"-17.3","6":"-21.5","7":"-25.6","8":"-29.7","9":"-33.7","10":"-37.7","11":"-41.6","12":"-45.5","13":"-49.3","14":"-53.1","15":"-56.8","16":"-60.4","17":"-64.0","18":"-67.6","19":"-71.1","20":"-74.5","21":"-77.9","22":"-81.3","23":"-84.6","24":"-87.8","25":"-91.0","26":"-94.1","27":"-97.2","28":"-100.2","29":"-103.2","30":"-106.1","31":"-109.0","32":"-111.8","33":"-114.6","34":"-117.3","35":"-119.9","36":"-122.5","37":"-125.1","38":"-127.6","39":"-130.0","40":"-132.4","41":"-134.8","42":"-137.1","43":"-139.3","44":"-141.5","45":"-143.6","46":"-145.7","47":"-147.7","48":"-149.7","49":"-151.6","50":"-153.5","51":"-155.3","52":"-157.0","53":"-158.7","54":"-160.4","55":"-162.0","56":"-163.5","57":"-165.0","58":"-166.5","59":"-167.9","60":"-169.2","61":"-170.5","62":"-171.7","63":"-172.9","64":"-174.0","65":"-175.1","66":"-176.1","67":"-177.1","68":"-178.0","69":"-178.9","70":"-179.7","71":"-180.5","72":"-181.2","73":"-181.8","74":"-182.4","75":"-183.0","76":"-183.5","77":"-183.9","78":"-184.3","79":"-184.6","80":"-184.9","81":"-185.1","82":"-185.3","83":"-185.4","84":"-185.5","85":"-185.5","86":"-185.5","87":"-185.4","88":"-185.3","89":"-185.1","90":"-184.8","91":"-184.5"},{"1":"0","2":"-4.45","3":"-8.86","4":"-13.21","5":"-17.5","6":"-21.7","7":"-25.9","8":"-30.1","9":"-34.2","10":"-38.2","11":"-42.2","12":"-46.1","13":"-50.0","14":"-53.8","15":"-57.6","16":"-61.3","17":"-64.9","18":"-68.6","19":"-72.1","20":"-75.6","21":"-79.1","22":"-82.5","23":"-85.8","24":"-89.1","25":"-92.4","26":"-95.5","27":"-98.7","28":"-101.8","29":"-104.8","30":"-107.8","31":"-110.7","32":"-113.6","33":"-116.4","34":"-119.2","35":"-121.9","36":"-124.5","37":"-127.1","38":"-129.7","39":"-132.2","40":"-134.6","41":"-137.0","42":"-139.4","43":"-141.7","44":"-143.9","45":"-146.1","46":"-148.2","47":"-150.3","48":"-152.3","49":"-154.3","50":"-156.2","51":"-158.1","52":"-159.9","53":"-161.7","54":"-163.4","55":"-165.1","56":"-166.7","57":"-168.2","58":"-169.7","59":"-171.2","60":"-172.6","61":"-173.9","62":"-175.2","63":"-176.4","64":"-177.6","65":"-178.8","66":"-179.8","67":"-180.9","68":"-181.8","69":"-182.7","70":"-183.6","71":"-184.4","72":"-185.2","73":"-185.9","74":"-186.6","75":"-187.2","76":"-187.7","77":"-188.2","78":"-188.7","79":"-189.0","80":"-189.4","81":"-189.7","82":"-189.9","83":"-190.1","84":"-190.2","85":"-190.3","86":"-190.3","87":"-190.3","88":"-190.2","89":"-190.1","90":"-189.9","91":"-189.6"},{"1":"0","2":"-4.51","3":"-8.97","4":"-13.38","5":"-17.7","6":"-22.0","7":"-26.3","8":"-30.5","9":"-34.6","10":"-38.7","11":"-42.7","12":"-46.7","13":"-50.7","14":"-54.5","15":"-58.4","16":"-62.1","17":"-65.9","18":"-69.5","19":"-73.1","20":"-76.7","21":"-80.2","22":"-83.7","23":"-87.1","24":"-90.4","25":"-93.7","26":"-97.0","27":"-100.2","28":"-103.3","29":"-106.4","30":"-109.4","31":"-112.4","32":"-115.3","33":"-118.2","34":"-121.0","35":"-123.8","36":"-126.5","37":"-129.2","38":"-131.8","39":"-134.4","40":"-136.9","41":"-139.3","42":"-141.7","43":"-144.1","44":"-146.4","45":"-148.6","46":"-150.8","47":"-152.9","48":"-155.0","49":"-157.1","50":"-159.0","51":"-161.0","52":"-162.8","53":"-164.7","54":"-166.4","55":"-168.1","56":"-169.8","57":"-171.4","58":"-173.0","59":"-174.5","60":"-175.9","61":"-177.3","62":"-178.7","63":"-180.0","64":"-181.2","65":"-182.4","66":"-183.5","67":"-184.6","68":"-185.6","69":"-186.6","70":"-187.5","71":"-188.4","72":"-189.2","73":"-190.0","74":"-190.7","75":"-191.4","76":"-192.0","77":"-192.5","78":"-193.0","79":"-193.5","80":"-193.9","81":"-194.2","82":"-194.5","83":"-194.7","84":"-194.9","85":"-195.1","86":"-195.1","87":"-195.2","88":"-195.1","89":"-195.1","90":"-194.9","91":"-194.7"},{"1":"0","2":"-4.57","3":"-9.08","4":"-13.55","5":"-18.0","6":"-22.3","7":"-26.6","8":"-30.9","9":"-35.1","10":"-39.2","11":"-43.3","12":"-47.4","13":"-51.3","14":"-55.3","15":"-59.2","16":"-63.0","17":"-66.8","18":"-70.5","19":"-74.2","20":"-77.8","21":"-81.3","22":"-84.9","23":"-88.3","24":"-91.7","25":"-95.1","26":"-98.4","27":"-101.6","28":"-104.8","29":"-108.0","30":"-111.1","31":"-114.1","32":"-117.1","33":"-120.0","34":"-122.9","35":"-125.7","36":"-128.5","37":"-131.2","38":"-133.9","39":"-136.5","40":"-139.1","41":"-141.6","42":"-144.0","43":"-146.5","44":"-148.8","45":"-151.1","46":"-153.4","47":"-155.5","48":"-157.7","49":"-159.8","50":"-161.8","51":"-163.8","52":"-165.7","53":"-167.6","54":"-169.4","55":"-171.2","56":"-172.9","57":"-174.6","58":"-176.2","59":"-177.8","60":"-179.3","61":"-180.7","62":"-182.1","63":"-183.5","64":"-184.8","65":"-186.0","66":"-187.2","67":"-188.4","68":"-189.4","69":"-190.5","70":"-191.5","71":"-192.4","72":"-193.3","73":"-194.1","74":"-194.8","75":"-195.6","76":"-196.2","77":"-196.8","78":"-197.4","79":"-197.9","80":"-198.4","81":"-198.8","82":"-199.1","83":"-199.4","84":"-199.6","85":"-199.8","86":"-200.0","87":"-200.1","88":"-200.1","89":"-200.1","90":"-200.0","91":"-199.9"},{"1":"0","2":"-4.63","3":"-9.20","4":"-13.72","5":"-18.2","6":"-22.6","7":"-27.0","8":"-31.3","9":"-35.5","10":"-39.7","11":"-43.9","12":"-48.0","13":"-52.0","14":"-56.0","15":"-60.0","16":"-63.8","17":"-67.7","18":"-71.5","19":"-75.2","20":"-78.9","21":"-82.5","22":"-86.1","23":"-89.6","24":"-93.0","25":"-96.4","26":"-99.8","27":"-103.1","28":"-106.4","29":"-109.6","30":"-112.7","31":"-115.8","32":"-118.9","33":"-121.8","34":"-124.8","35":"-127.7","36":"-130.5","37":"-133.3","38":"-136.0","39":"-138.7","40":"-141.3","41":"-143.9","42":"-146.4","43":"-148.8","44":"-151.2","45":"-153.6","46":"-155.9","47":"-158.2","48":"-160.4","49":"-162.5","50":"-164.6","51":"-166.6","52":"-168.6","53":"-170.6","54":"-172.4","55":"-174.3","56":"-176.0","57":"-177.8","58":"-179.4","59":"-181.1","60":"-182.6","61":"-184.1","62":"-185.6","63":"-187.0","64":"-188.4","65":"-189.7","66":"-190.9","67":"-192.1","68":"-193.2","69":"-194.3","70":"-195.4","71":"-196.4","72":"-197.3","73":"-198.2","74":"-199.0","75":"-199.8","76":"-200.5","77":"-201.2","78":"-201.8","79":"-202.3","80":"-202.8","81":"-203.3","82":"-203.7","83":"-204.1","84":"-204.4","85":"-204.6","86":"-204.8","87":"-204.9","88":"-205.0","89":"-205.1","90":"-205.0","91":"-205.0"},{"1":"0","2":"-4.68","3":"-9.31","4":"-13.89","5":"-18.4","6":"-22.9","7":"-27.3","8":"-31.7","9":"-36.0","10":"-40.2","11":"-44.4","12":"-48.6","13":"-52.7","14":"-56.8","15":"-60.7","16":"-64.7","17":"-68.6","18":"-72.4","19":"-76.2","20":"-79.9","21":"-83.6","22":"-87.2","23":"-90.8","24":"-94.3","25":"-97.8","26":"-101.2","27":"-104.6","28":"-107.9","29":"-111.2","30":"-114.4","31":"-117.5","32":"-120.6","33":"-123.7","34":"-126.7","35":"-129.6","36":"-132.5","37":"-135.3","38":"-138.1","39":"-140.8","40":"-143.5","41":"-146.1","42":"-148.7","43":"-151.2","44":"-153.7","45":"-156.1","46":"-158.5","47":"-160.8","48":"-163.0","49":"-165.2","50":"-167.4","51":"-169.5","52":"-171.5","53":"-173.5","54":"-175.4","55":"-177.3","56":"-179.2","57":"-180.9","58":"-182.7","59":"-184.4","60":"-186.0","61":"-187.5","62":"-189.1","63":"-190.5","64":"-191.9","65":"-193.3","66":"-194.6","67":"-195.8","68":"-197.0","69":"-198.2","70":"-199.3","71":"-200.3","72":"-201.3","73":"-202.3","74":"-203.1","75":"-204.0","76":"-204.7","77":"-205.5","78":"-206.1","79":"-206.8","80":"-207.3","81":"-207.8","82":"-208.3","83":"-208.7","84":"-209.1","85":"-209.4","86":"-209.6","87":"-209.8","88":"-210.0","89":"-210.1","90":"-210.1","91":"-210.1"},{"1":"0","2":"-4.74","3":"-9.43","4":"-14.06","5":"-18.6","6":"-23.2","7":"-27.6","8":"-32.1","9":"-36.4","10":"-40.8","11":"-45.0","12":"-49.2","13":"-53.4","14":"-57.5","15":"-61.5","16":"-65.5","17":"-69.5","18":"-73.4","19":"-77.2","20":"-81.0","21":"-84.8","22":"-88.4","23":"-92.1","24":"-95.6","25":"-99.2","26":"-102.6","27":"-106.1","28":"-109.4","29":"-112.7","30":"-116.0","31":"-119.2","32":"-122.4","33":"-125.5","34":"-128.5","35":"-131.5","36":"-134.5","37":"-137.4","38":"-140.2","39":"-143.0","40":"-145.7","41":"-148.4","42":"-151.0","43":"-153.6","44":"-156.1","45":"-158.6","46":"-161.0","47":"-163.4","48":"-165.7","49":"-168.0","50":"-170.2","51":"-172.3","52":"-174.4","53":"-176.5","54":"-178.5","55":"-180.4","56":"-182.3","57":"-184.1","58":"-185.9","59":"-187.6","60":"-189.3","61":"-191.0","62":"-192.5","63":"-194.0","64":"-195.5","65":"-196.9","66":"-198.3","67":"-199.6","68":"-200.9","69":"-202.1","70":"-203.2","71":"-204.3","72":"-205.4","73":"-206.3","74":"-207.3","75":"-208.2","76":"-209.0","77":"-209.8","78":"-210.5","79":"-211.2","80":"-211.8","81":"-212.4","82":"-212.9","83":"-213.4","84":"-213.8","85":"-214.1","86":"-214.5","87":"-214.7","88":"-214.9","89":"-215.1","90":"-215.2","91":"-215.2"},{"1":"0","2":"-4.80","3":"-9.54","4":"-14.23","5":"-18.9","6":"-23.5","7":"-28.0","8":"-32.5","9":"-36.9","10":"-41.3","11":"-45.6","12":"-49.9","13":"-54.1","14":"-58.2","15":"-62.3","16":"-66.4","17":"-70.4","18":"-74.4","19":"-78.3","20":"-82.1","21":"-85.9","22":"-89.6","23":"-93.3","24":"-97.0","25":"-100.5","26":"-104.1","27":"-107.5","28":"-111.0","29":"-114.3","30":"-117.7","31":"-120.9","32":"-124.1","33":"-127.3","34":"-130.4","35":"-133.5","36":"-136.5","37":"-139.4","38":"-142.3","39":"-145.1","40":"-147.9","41":"-150.7","42":"-153.4","43":"-156.0","44":"-158.6","45":"-161.1","46":"-163.6","47":"-166.0","48":"-168.4","49":"-170.7","50":"-172.9","51":"-175.2","52":"-177.3","53":"-179.4","54":"-181.5","55":"-183.5","56":"-185.4","57":"-187.3","58":"-189.2","59":"-190.9","60":"-192.7","61":"-194.4","62":"-196.0","63":"-197.6","64":"-199.1","65":"-200.6","66":"-202.0","67":"-203.3","68":"-204.7","69":"-205.9","70":"-207.1","71":"-208.3","72":"-209.4","73":"-210.4","74":"-211.4","75":"-212.4","76":"-213.3","77":"-214.1","78":"-214.9","79":"-215.6","80":"-216.3","81":"-216.9","82":"-217.5","83":"-218.0","84":"-218.5","85":"-218.9","86":"-219.3","87":"-219.6","88":"-219.9","89":"-220.1","90":"-220.2","91":"-220.3"},{"1":"0","2":"-4.85","3":"-9.65","4":"-14.40","5":"-19.1","6":"-23.7","7":"-28.3","8":"-32.9","9":"-37.3","10":"-41.8","11":"-46.2","12":"-50.5","13":"-54.7","14":"-59.0","15":"-63.1","16":"-67.2","17":"-71.3","18":"-75.3","19":"-79.3","20":"-83.2","21":"-87.0","22":"-90.8","23":"-94.6","24":"-98.3","25":"-101.9","26":"-105.5","27":"-109.0","28":"-112.5","29":"-115.9","30":"-119.3","31":"-122.6","32":"-125.9","33":"-129.1","34":"-132.3","35":"-135.4","36":"-138.4","37":"-141.5","38":"-144.4","39":"-147.3","40":"-150.2","41":"-152.9","42":"-155.7","43":"-158.4","44":"-161.0","45":"-163.6","46":"-166.1","47":"-168.6","48":"-171.0","49":"-173.4","50":"-175.7","51":"-178.0","52":"-180.2","53":"-182.4","54":"-184.5","55":"-186.5","56":"-188.5","57":"-190.5","58":"-192.4","59":"-194.2","60":"-196.0","61":"-197.8","62":"-199.5","63":"-201.1","64":"-202.7","65":"-204.2","66":"-205.7","67":"-207.1","68":"-208.5","69":"-209.8","70":"-211.0","71":"-212.3","72":"-213.4","73":"-214.5","74":"-215.6","75":"-216.6","76":"-217.5","77":"-218.4","78":"-219.3","79":"-220.1","80":"-220.8","81":"-221.5","82":"-222.1","83":"-222.7","84":"-223.2","85":"-223.7","86":"-224.1","87":"-224.5","88":"-224.8","89":"-225.1","90":"-225.3","91":"-225.4"},{"1":"0","2":"-4.91","3":"-9.77","4":"-14.57","5":"-19.3","6":"-24.0","7":"-28.7","8":"-33.3","9":"-37.8","10":"-42.3","11":"-46.7","12":"-51.1","13":"-55.4","14":"-59.7","15":"-63.9","16":"-68.1","17":"-72.2","18":"-76.3","19":"-80.3","20":"-84.3","21":"-88.2","22":"-92.0","23":"-95.8","24":"-99.6","25":"-103.3","26":"-106.9","27":"-110.5","28":"-114.0","29":"-117.5","30":"-121.0","31":"-124.3","32":"-127.7","33":"-130.9","34":"-134.2","35":"-137.3","36":"-140.4","37":"-143.5","38":"-146.5","39":"-149.5","40":"-152.4","41":"-155.2","42":"-158.0","43":"-160.8","44":"-163.5","45":"-166.1","46":"-168.7","47":"-171.2","48":"-173.7","49":"-176.1","50":"-178.5","51":"-180.8","52":"-183.1","53":"-185.3","54":"-187.5","55":"-189.6","56":"-191.7","57":"-193.7","58":"-195.6","59":"-197.5","60":"-199.4","61":"-201.2","62":"-202.9","63":"-204.6","64":"-206.2","65":"-207.8","66":"-209.4","67":"-210.8","68":"-212.3","69":"-213.6","70":"-215.0","71":"-216.2","72":"-217.5","73":"-218.6","74":"-219.7","75":"-220.8","76":"-221.8","77":"-222.7","78":"-223.6","79":"-224.5","80":"-225.3","81":"-226.0","82":"-226.7","83":"-227.3","84":"-227.9","85":"-228.5","86":"-228.9","87":"-229.4","88":"-229.7","89":"-230.1","90":"-230.3","91":"-230.5"},{"1":"0","2":"-4.97","3":"-9.88","4":"-14.74","5":"-19.5","6":"-24.3","7":"-29.0","8":"-33.7","9":"-38.3","10":"-42.8","11":"-47.3","12":"-51.7","13":"-56.1","14":"-60.4","15":"-64.7","16":"-69.0","17":"-73.1","18":"-77.2","19":"-81.3","20":"-85.3","21":"-89.3","22":"-93.2","23":"-97.1","24":"-100.9","25":"-104.6","26":"-108.3","27":"-112.0","28":"-115.6","29":"-119.1","30":"-122.6","31":"-126.0","32":"-129.4","33":"-132.7","34":"-136.0","35":"-139.3","36":"-142.4","37":"-145.5","38":"-148.6","39":"-151.6","40":"-154.6","41":"-157.5","42":"-160.3","43":"-163.2","44":"-165.9","45":"-168.6","46":"-171.2","47":"-173.8","48":"-176.4","49":"-178.9","50":"-181.3","51":"-183.7","52":"-186.0","53":"-188.3","54":"-190.5","55":"-192.7","56":"-194.8","57":"-196.9","58":"-198.9","59":"-200.8","60":"-202.7","61":"-204.6","62":"-206.4","63":"-208.1","64":"-209.8","65":"-211.5","66":"-213.1","67":"-214.6","68":"-216.1","69":"-217.5","70":"-218.9","71":"-220.2","72":"-221.5","73":"-222.7","74":"-223.9","75":"-225.0","76":"-226.0","77":"-227.1","78":"-228.0","79":"-228.9","80":"-229.8","81":"-230.6","82":"-231.3","83":"-232.0","84":"-232.6","85":"-233.2","86":"-233.8","87":"-234.2","88":"-234.7","89":"-235.1","90":"-235.4","91":"-235.6"},{"1":"0","2":"-5.02","3":"-9.99","4":"-14.91","5":"-19.8","6":"-24.6","7":"-29.3","8":"-34.1","9":"-38.7","10":"-43.3","11":"-47.9","12":"-52.4","13":"-56.8","14":"-61.2","15":"-65.5","16":"-69.8","17":"-74.0","18":"-78.2","19":"-82.3","20":"-86.4","21":"-90.4","22":"-94.4","23":"-98.3","24":"-102.2","25":"-106.0","26":"-109.7","27":"-113.4","28":"-117.1","29":"-120.7","30":"-124.2","31":"-127.7","32":"-131.2","33":"-134.6","34":"-137.9","35":"-141.2","36":"-144.4","37":"-147.6","38":"-150.7","39":"-153.8","40":"-156.8","41":"-159.8","42":"-162.7","43":"-165.5","44":"-168.3","45":"-171.1","46":"-173.8","47":"-176.4","48":"-179.0","49":"-181.6","50":"-184.1","51":"-186.5","52":"-188.9","53":"-191.2","54":"-193.5","55":"-195.7","56":"-197.9","57":"-200.0","58":"-202.1","59":"-204.1","60":"-206.1","61":"-208.0","62":"-209.8","63":"-211.7","64":"-213.4","65":"-215.1","66":"-216.7","67":"-218.3","68":"-219.9","69":"-221.4","70":"-222.8","71":"-224.2","72":"-225.5","73":"-226.8","74":"-228.0","75":"-229.2","76":"-230.3","77":"-231.4","78":"-232.4","79":"-233.3","80":"-234.3","81":"-235.1","82":"-235.9","83":"-236.7","84":"-237.4","85":"-238.0","86":"-238.6","87":"-239.1","88":"-239.6","89":"-240.0","90":"-240.4","91":"-240.8"},{"1":"0","2":"-5.08","3":"-10.11","4":"-15.08","5":"-20.0","6":"-24.9","7":"-29.7","8":"-34.5","9":"-39.2","10":"-43.8","11":"-48.4","12":"-53.0","13":"-57.5","14":"-61.9","15":"-66.3","16":"-70.7","17":"-74.9","18":"-79.2","19":"-83.4","20":"-87.5","21":"-91.6","22":"-95.6","23":"-99.6","24":"-103.5","25":"-107.4","26":"-111.2","27":"-114.9","28":"-118.6","29":"-122.3","30":"-125.9","31":"-129.4","32":"-132.9","33":"-136.4","34":"-139.8","35":"-143.1","36":"-146.4","37":"-149.6","38":"-152.8","39":"-155.9","40":"-159.0","41":"-162.0","42":"-165.0","43":"-167.9","44":"-170.8","45":"-173.6","46":"-176.4","47":"-179.1","48":"-181.7","49":"-184.3","50":"-186.9","51":"-189.4","52":"-191.8","53":"-194.2","54":"-196.5","55":"-198.8","56":"-201.0","57":"-203.2","58":"-205.3","59":"-207.4","60":"-209.4","61":"-211.4","62":"-213.3","63":"-215.2","64":"-217.0","65":"-218.7","66":"-220.4","67":"-222.1","68":"-223.7","69":"-225.2","70":"-226.7","71":"-228.2","72":"-229.5","73":"-230.9","74":"-232.2","75":"-233.4","76":"-234.6","77":"-235.7","78":"-236.8","79":"-237.8","80":"-238.7","81":"-239.7","82":"-240.5","83":"-241.3","84":"-242.1","85":"-242.8","86":"-243.4","87":"-244.0","88":"-244.6","89":"-245.0","90":"-245.5","91":"-245.9"},{"1":"0","2":"-5.14","3":"-10.22","4":"-15.25","5":"-20.2","6":"-25.2","7":"-30.0","8":"-34.8","9":"-39.6","10":"-44.3","11":"-49.0","12":"-53.6","13":"-58.2","14":"-62.7","15":"-67.1","16":"-71.5","17":"-75.9","18":"-80.1","19":"-84.4","20":"-88.6","21":"-92.7","22":"-96.8","23":"-100.8","24":"-104.8","25":"-108.7","26":"-112.6","27":"-116.4","28":"-120.2","29":"-123.9","30":"-127.5","31":"-131.1","32":"-134.7","33":"-138.2","34":"-141.6","35":"-145.0","36":"-148.4","37":"-151.7","38":"-154.9","39":"-158.1","40":"-161.2","41":"-164.3","42":"-167.3","43":"-170.3","44":"-173.2","45":"-176.1","46":"-178.9","47":"-181.7","48":"-184.4","49":"-187.0","50":"-189.6","51":"-192.2","52":"-194.7","53":"-197.1","54":"-199.5","55":"-201.9","56":"-204.2","57":"-206.4","58":"-208.6","59":"-210.7","60":"-212.8","61":"-214.8","62":"-216.8","63":"-218.7","64":"-220.6","65":"-222.4","66":"-224.1","67":"-225.8","68":"-227.5","69":"-229.1","70":"-230.6","71":"-232.1","72":"-233.6","73":"-235.0","74":"-236.3","75":"-237.6","76":"-238.8","77":"-240.0","78":"-241.1","79":"-242.2","80":"-243.2","81":"-244.2","82":"-245.1","83":"-246.0","84":"-246.8","85":"-247.5","86":"-248.2","87":"-248.9","88":"-249.5","89":"-250.0","90":"-250.5","91":"-251.0"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
df$stringency_index %>% 
  summary()
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    39.4    48.6    49.0    60.2    88.0       1 
```

