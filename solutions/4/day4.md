# Challenge 1

    library(readr)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(purrr)
    library(magrittr)

We’ve got a pretty messy file to read in. Never fear - between `stringr`
and `janitor` we can make short work of this.

    readr::read_file(here::here("data/input_4.txt")) %>% 
      str_split("\n\n") %>% 
      unlist() %>% 
      str_replace_all("\n", " ") %>% 
      str_split(" ") %>% 
      map_df(~str_split(., ":") %>% 
        do.call(cbind.data.frame, .) %>% 
        janitor::row_to_names(1)
      ) %>% 
      janitor::remove_constant(na.rm = TRUE) -> passport_data

    passport_data %>% 
      select(-cid) %>% 
      drop_na() %>% 
      nrow()

    ## [1] 210

## Challenge 2

We need to implement some validation rules. `pointblank` allows for
custom data validation:

    library(pointblank)

    passport_data %>% mutate(hgt_num = as.numeric(str_extract(hgt, "[0-9]+")),
                             hgt_unit = (str_extract(hgt, "[aA-zZ]+"))) %>% 
      mutate(hgt_cm = ifelse(hgt_unit == "in", hgt_num * 2.54, hgt_num)) %>% 
      create_agent() %>% 
      col_vals_between(vars(byr), 1920, 2002) %>% 
      col_vals_between(vars(iyr), 2010, 2020) %>% 
      col_vals_between(vars(eyr), 2020, 2030) %>% 
      col_vals_regex(vars(hcl), "^#[0-9a-f]{6}$") %>% 
      col_vals_in_set(vars(ecl), c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) %>% 
      col_vals_regex(vars(pid), "^[0-9]{9}$") %>% 
      col_vals_between(vars(hgt_cm), 150, 193) %>% 
      interrogate() -> valid_report

    valid_report

<!--html_preserve-->
<style>@import url("https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://unpkg.com/balloon-css/balloon.min.css");
@import url("https://fonts.googleapis.com/css2?family=IBM+Plex+Mono&display=swap");
html {
  font-family: 'IBM Plex Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#report .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 90%;
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

#report .gt_heading {
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

#report .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#report .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#report .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#report .gt_col_headings {
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

#report .gt_col_heading {
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

#report .gt_column_spanner_outer {
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

#report .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#report .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#report .gt_column_spanner {
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

#report .gt_group_heading {
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

#report .gt_empty_group_heading {
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

#report .gt_from_md > :first-child {
  margin-top: 0;
}

#report .gt_from_md > :last-child {
  margin-bottom: 0;
}

#report .gt_row {
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

#report .gt_stub {
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

#report .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#report .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#report .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#report .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#report .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#report .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#report .gt_footnotes {
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

#report .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#report .gt_sourcenotes {
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

#report .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#report .gt_left {
  text-align: left;
}

#report .gt_center {
  text-align: center;
}

#report .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#report .gt_font_normal {
  font-weight: normal;
}

#report .gt_font_bold {
  font-weight: bold;
}

#report .gt_font_italic {
  font-style: italic;
}

#report .gt_super {
  font-size: 65%;
}

#report .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}

#pb_information {
  -webkit-font-smoothing: antialiased;
}

#report .gt_row {
  overflow: visible;
}

#report .gt_sourcenote {
  height: 35px;
  padding: 0;
}

#report code {
  font-family: 'IBM Plex Mono', monospace, courier;
  font-size: 11px;
  background-color: transparent;
  padding: 0;
}
</style>
<div id="report" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<table class="gt_table" style="table-layout: fixed;; width: 0px">
<colgroup>
<col style="width:6px;"/>
<col style="width:35px;"/>
<col style="width:190px;"/>
<col style="width:120px;"/>
<col style="width:120px;"/>
<col style="width:50px;"/>
<col style="width:50px;"/>
<col style="width:50px;"/>
<col style="width:50px;"/>
<col style="width:50px;"/>
<col style="width:30px;"/>
<col style="width:30px;"/>
<col style="width:30px;"/>
<col style="width:65px;"/>
</colgroup>
<thead class="gt_header">
<tr>
<th colspan="14" class="gt_heading gt_title gt_font_normal" style="color: #444444; font-size: 28px; text-align: left; font-weight: 500;">
Pointblank Validation
</th>
</tr>
<tr>
<th colspan="14" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style="font-size: 12px; text-align: left;">
<span
style="text-decoration-style:solid;text-decoration-color:#ADD8E6;text-decoration-line:underline;text-underline-position:under;color:#333333;font-variant-numeric:tabular-nums;padding-left:4px;margin-right:5px;padding-right:2px;">\[2020-12-04|10:33:19\]</span>
</p>

<span
style="background-color: #9933CC;color: #FFFFFF;padding: 0.5em 0.5em;position: inherit;text-transform: uppercase;margin: 5px 1px 5px 4px;font-weight: bold;border: solid 1px #9933CC;padding: 2px 10px 2px 10px;font-size: smaller;">data
frame</span>

</th>
</tr>
</thead>
<thead class="gt_col_headings">
<tr>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
STEP
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
COLUMNS
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
VALUES
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
TBL
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
EVAL
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
⋅ ⋅ ⋅
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
PASS
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
FAIL
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
W
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
S
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
N
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="color: #666666; font-weight: bold;">
EXT
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td class="gt_row gt_left" style="background-color: rgba(76,166,76,0.5); height:  40px">
</td>
<td class="gt_row gt_right" style="color: #666666; font-size: 13px; font-weight: bold; height:  40px">
1
</td>
<td class="gt_row gt_left" style="height:  40px">

<svg width="30px" height="30px" viewBox="0 0 67 67" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<defs>
<path d="M10.712234,0 L56.712234,0 C62.2350815,-1.01453063e-15 66.712234,4.4771525 66.712234,10 L66.712234,66 L10.712234,66 C5.18938647,66 0.712233968,61.5228475 0.712233968,56 L0.712233968,10 C0.712233968,4.4771525 5.18938647,1.01453063e-15 10.712234,0 Z" id="path-1"></path>
</defs>
<g id="pointblank" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
<g id="col_vals_between" transform="translate(-0.487938, 0.651308)">
<g id="rectangle">
<use fill="#FFFFFF" fill-rule="evenodd" xlink:href="#path-1"></use>
<path stroke="#000000" stroke-width="2" d="M65.712234,65 L65.712234,10 C65.712234,5.02943725 61.6827967,1 56.712234,1 L10.712234,1 C5.74167122,1 1.71223397,5.02943725 1.71223397,10 L1.71223397,56 C1.71223397,60.9705627 5.74167122,65 10.712234,65 L65.712234,65 Z"></path>
</g>
<path d="M11.993484,21.96875 C10.962234,22.082031 10.188797,22.964844 10.212234,24 L10.212234,42 C10.200515,42.722656 10.579422,43.390625 11.204422,43.753906 C11.825515,44.121094 12.598953,44.121094 13.220047,43.753906 C13.845047,43.390625 14.223953,42.722656 14.212234,42 L14.212234,24 C14.220047,23.457031 14.009109,22.9375 13.626297,22.554688 C13.243484,22.171875 12.723953,21.960938 12.180984,21.96875 C12.118484,21.964844 12.055984,21.964844 11.993484,21.96875 Z M55.993484,21.96875 C54.962234,22.082031 54.188797,22.964844 54.212234,24 L54.212234,42 C54.200515,42.722656 54.579422,43.390625 55.204422,43.753906 C55.825515,44.121094 56.598953,44.121094 57.220047,43.753906 C57.845047,43.390625 58.223953,42.722656 58.212234,42 L58.212234,24 C58.220047,23.457031 58.009109,22.9375 57.626297,22.554688 C57.243484,22.171875 56.723953,21.960938 56.180984,21.96875 C56.118484,21.964844 56.055984,21.964844 55.993484,21.96875 Z M16.212234,22 C15.661453,22 15.212234,22.449219 15.212234,23 C15.212234,23.550781 15.661453,24 16.212234,24 C16.763015,24 17.212234,23.550781 17.212234,23 C17.212234,22.449219 16.763015,22 16.212234,22 Z M20.212234,22 C19.661453,22 19.212234,22.449219 19.212234,23 C19.212234,23.550781 19.661453,24 20.212234,24 C20.763015,24 21.212234,23.550781 21.212234,23 C21.212234,22.449219 20.763015,22 20.212234,22 Z M24.212234,22 C23.661453,22 23.212234,22.449219 23.212234,23 C23.212234,23.550781 23.661453,24 24.212234,24 C24.763015,24 25.212234,23.550781 25.212234,23 C25.212234,22.449219 24.763015,22 24.212234,22 Z M28.212234,22 C27.661453,22 27.212234,22.449219 27.212234,23 C27.212234,23.550781 27.661453,24 28.212234,24 C28.763015,24 29.212234,23.550781 29.212234,23 C29.212234,22.449219 28.763015,22 28.212234,22 Z M32.212234,22 C31.661453,22 31.212234,22.449219 31.212234,23 C31.212234,23.550781 31.661453,24 32.212234,24 C32.763015,24 33.212234,23.550781 33.212234,23 C33.212234,22.449219 32.763015,22 32.212234,22 Z M36.212234,22 C35.661453,22 35.212234,22.449219 35.212234,23 C35.212234,23.550781 35.661453,24 36.212234,24 C36.763015,24 37.212234,23.550781 37.212234,23 C37.212234,22.449219 36.763015,22 36.212234,22 Z M40.212234,22 C39.661453,22 39.212234,22.449219 39.212234,23 C39.212234,23.550781 39.661453,24 40.212234,24 C40.763015,24 41.212234,23.550781 41.212234,23 C41.212234,22.449219 40.763015,22 40.212234,22 Z M44.212234,22 C43.661453,22 43.212234,22.449219 43.212234,23 C43.212234,23.550781 43.661453,24 44.212234,24 C44.763015,24 45.212234,23.550781 45.212234,23 C45.212234,22.449219 44.763015,22 44.212234,22 Z M48.212234,22 C47.661453,22 47.212234,22.449219 47.212234,23 C47.212234,23.550781 47.661453,24 48.212234,24 C48.763015,24 49.212234,23.550781 49.212234,23 C49.212234,22.449219 48.763015,22 48.212234,22 Z M52.212234,22 C51.661453,22 51.212234,22.449219 51.212234,23 C51.212234,23.550781 51.661453,24 52.212234,24 C52.763015,24 53.212234,23.550781 53.212234,23 C53.212234,22.449219 52.763015,22 52.212234,22 Z M21.462234,27.96875 C21.419265,27.976563 21.376297,27.988281 21.337234,28 C21.177078,28.027344 21.02864,28.089844 20.899734,28.1875 L15.618484,32.1875 C15.356765,32.375 15.200515,32.679688 15.200515,33 C15.200515,33.320313 15.356765,33.625 15.618484,33.8125 L20.899734,37.8125 C21.348953,38.148438 21.985672,38.058594 22.321609,37.609375 C22.657547,37.160156 22.567703,36.523438 22.118484,36.1875 L19.212234,34 L49.212234,34 L46.305984,36.1875 C45.856765,36.523438 45.766922,37.160156 46.102859,37.609375 C46.438797,38.058594 47.075515,38.148438 47.524734,37.8125 L52.805984,33.8125 C53.067703,33.625 53.223953,33.320313 53.223953,33 C53.223953,32.679688 53.067703,32.375 52.805984,32.1875 L47.524734,28.1875 C47.30989,28.027344 47.040359,27.960938 46.774734,28 C46.743484,28 46.712234,28 46.680984,28 C46.282547,28.074219 45.96614,28.382813 45.884109,28.78125 C45.802078,29.179688 45.970047,29.585938 46.305984,29.8125 L49.212234,32 L19.212234,32 L22.118484,29.8125 C22.520828,29.566406 22.696609,29.070313 22.536453,28.625 C22.380203,28.179688 21.930984,27.90625 21.462234,27.96875 Z M16.212234,42 C15.661453,42 15.212234,42.449219 15.212234,43 C15.212234,43.550781 15.661453,44 16.212234,44 C16.763015,44 17.212234,43.550781 17.212234,43 C17.212234,42.449219 16.763015,42 16.212234,42 Z M20.212234,42 C19.661453,42 19.212234,42.449219 19.212234,43 C19.212234,43.550781 19.661453,44 20.212234,44 C20.763015,44 21.212234,43.550781 21.212234,43 C21.212234,42.449219 20.763015,42 20.212234,42 Z M24.212234,42 C23.661453,42 23.212234,42.449219 23.212234,43 C23.212234,43.550781 23.661453,44 24.212234,44 C24.763015,44 25.212234,43.550781 25.212234,43 C25.212234,42.449219 24.763015,42 24.212234,42 Z M28.212234,42 C27.661453,42 27.212234,42.449219 27.212234,43 C27.212234,43.550781 27.661453,44 28.212234,44 C28.763015,44 29.212234,43.550781 29.212234,43 C29.212234,42.449219 28.763015,42 28.212234,42 Z M32.212234,42 C31.661453,42 31.212234,42.449219 31.212234,43 C31.212234,43.550781 31.661453,44 32.212234,44 C32.763015,44 33.212234,43.550781 33.212234,43 C33.212234,42.449219 32.763015,42 32.212234,42 Z M36.212234,42 C35.661453,42 35.212234,42.449219 35.212234,43 C35.212234,43.550781 35.661453,44 36.212234,44 C36.763015,44 37.212234,43.550781 37.212234,43 C37.212234,42.449219 36.763015,42 36.212234,42 Z M40.212234,42 C39.661453,42 39.212234,42.449219 39.212234,43 C39.212234,43.550781 39.661453,44 40.212234,44 C40.763015,44 41.212234,43.550781 41.212234,43 C41.212234,42.449219 40.763015,42 40.212234,42 Z M44.212234,42 C43.661453,42 43.212234,42.449219 43.212234,43 C43.212234,43.550781 43.661453,44 44.212234,44 C44.763015,44 45.212234,43.550781 45.212234,43 C45.212234,42.449219 44.763015,42 44.212234,42 Z M48.212234,42 C47.661453,42 47.212234,42.449219 47.212234,43 C47.212234,43.550781 47.661453,44 48.212234,44 C48.763015,44 49.212234,43.550781 49.212234,43 C49.212234,42.449219 48.763015,42 48.212234,42 Z M52.212234,42 C51.661453,42 51.212234,42.449219 51.212234,43 C51.212234,43.550781 51.661453,44 52.212234,44 C52.763015,44 53.212234,43.550781 53.212234,43 C53.212234,42.449219 52.763015,42 52.212234,42 Z" id="inside_range" fill="#000000" fill-rule="nonzero"></path>
</g> </g>
</svg>

<code style="font-size:11px;"> col\_vals\_between()</code>

</td>
<td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">

<p style="margin-top:0;margin-bottom:0;font-size:11px;white-space:nowrap;text-overflow:ellipsis;overflow:hidden;line-height:2em;">
<code><span style="color:purple;">▮</span>byr</code>
</p>

</td>
<td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">

<p style="margin-top: 0px; margin-bottom: 0px; font-size: 11px; white-space: nowrap; text-overflow: ellipsis; overflow: hidden;">
<code>\[1,920, 2,002\]</code>
</p>

</td>
<td class="gt_row gt_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; background-color: #FCFCFC; height:  40px">

<p>
<span
style="background:transparent;padding:0;color:#333333;vertical-align:middle;font-size:18px;border:none;border-radius:4px;"
aria-label="No modifications of the table."
data-balloon-pos="left">→</span>
</p>

</td>
<td class="gt_row gt_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; background-color: #FCFCFC; height:  40px">

<p>
<span
style="background:transparent;padding:5px;color:#4CA64C;vertical-align:middle;font-size:15px;border:none;"
aria-label="No evaluation issues." data-balloon-pos="left">✓</span>
</p>

</td>
<td class="gt_row gt_right" style="height:  40px">
<code>276</code>
</td>
<td class="gt_row gt_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">
<code>210</code><br><code>0.76</code>
</td>
<td class="gt_row gt_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">
<code>66</code><br><code>0.24</code>
</td>
<td class="gt_row gt_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; background-color: #FCFCFC; height:  40px">

<p>
—
</p>

</td>
<td class="gt_row gt_center" style="background-color: #FCFCFC; height:  40px">

<p>
—
</p>

</td>
<td class="gt_row gt_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; background-color: #FCFCFC; height:  40px">

<p>
—
</p>

</td>
<td class="gt_row gt_center" style="height:  40px">

<a href="data:text/csv;base64,Iml5ciIsImhndCIsImJ5ciIsImNpZCIsInBpZCIsImV5ciIsImhjbCIsImVjbCIsImhndF9udW0iLCJoZ3RfdW5pdCIsImhndF9jbSINCiIyMDE1IiwiNTljbSIsIjIwMjkiLCIyMTkiLCI5MzgxNjg4NzUzIiwiMTk5MiIsIiNiNjY1MmEiLCIjN2EwZmE2Iiw1OSwiY20iLDU5DQoiMjAyNSIsIjY0Y20iLCIyMDI5IiwiMjgxIiwiMDY3Mjg1OTg1IiwiMTk0NCIsIiNjZWIzYTEiLCIjMDcyMTlhIiw2NCwiY20iLDY0DQoiMjAyNSIsIjE0MSIsIjIwMDYiLCIzMjciLCIjMWM0MmNjIiwiMTk1NiIsInoiLCIjZjJhZmZjIiwxNDEsTkEsTkENCiIxOTI0IiwiNzRjbSIsIjIwMjQiLCIxNTMiLCIzNjE5NjQwMSIsIjE5MjEiLCJhNGU0YzAiLCIjM2FjZjU3Iiw3NCwiY20iLDc0DQoiMjAyMCIsIjE1MSIsIjIwMTIiLCIzMTQiLCI5ODQzNjQ4NjIiLCIyMDIzIiwieiIsImRuZSIsMTUxLE5BLE5BDQoiMTkzMyIsIjkwIiwiMjAyNSIsTkEsIjgxOTQzNDc1NDQiLCIyMDQwIixOQSwiZG5lIiw5MCxOQSxOQQ0KIjE5MjAiLCI2MGNtIiwiMjAyOSIsIjEwNyIsIiM1NWNlNmIiLCIyMDQwIiwiZDMwZjZiIiwiZG5lIiw2MCwiY20iLDYwDQoiMjAxNiIsIjE3MmNtIixOQSxOQSwiIzk4Y2FlYyIsIjIwMzYiLCJ6IiwiZG5lIiwxNzIsImNtIiwxNzINCiIxOTM4IiwiMTcyaW4iLCIyMDE4IiwiMzM5IiwiIzZjMTIxNiIsIjIwMzkiLCJ6IiwiIzUxMDY3MiIsMTcyLCJpbiIsNDM2Ljg4DQoiMTk5NyIsIjk2IiwiMjAwOSIsTkEsIjA2MzY5MTcyMjIiLCIyMDI2IiwieiIsImh6bCIsOTYsTkEsTkENCiIxOTUxIiwiMTkzaW4iLCIyMDE5IixOQSwiI2NiYzA4YyIsIjIwMDIiLCJ6IiwiIzNlOWYyZiIsMTkzLCJpbiIsNDkwLjIyDQpOQSwiMTc1aW4iLCIyMDEyIiwiMjQwIiwiNTcxNDc3MTc2IiwiMTkyOSIsImY0ZWYzMiIsInV0YyIsMTc1LCJpbiIsNDQ0LjUNCiIyMDI2IiwiNzAiLCIyMDIyIixOQSwiNDA0NjIyMDgzIiwiMTk3OSIsImMxYmE3ZiIsImx6ciIsNzAsTkEsTkENCiIyMDE4IiwiMTU2Y20iLCIyMDE3IiwiMTc5IiwiMjk4MzYxMjQiLCIyMDIzIiwiNTZkZTgzIiwienp6IiwxNTYsImNtIiwxNTYNCk5BLCI4MiIsIjIwMTciLCIxNTMiLCIxNzI2NjE2MTciLCIyMDM2IiwieiIsIiNhZTYwN2QiLDgyLE5BLE5BDQoiMjAyMCIsIjYzaW4iLE5BLE5BLCIxNDY2NTA4OTQiLCIyMDI1IiwiI2EzNTViZSIsImFtYiIsNjMsImluIiwxNjAuMDINCiIyMDE2IiwiMTkyY20iLE5BLE5BLCI1MzEzNzI5NjUiLCIyMDI1IiwiI2ZmZmZmZCIsImJsdSIsMTkyLCJjbSIsMTkyDQoiMTk0NSIsIjE2NWluIiwiMjAxMCIsIjI3MyIsIjQzNzA3NTA0IiwiMjAyNiIsInoiLCJncnQiLDE2NSwiaW4iLDQxOS4xDQoiMjAxNSIsIjE1OWNtIixOQSxOQSwiOTE1ODE5MjcyIiwiMjAzMCIsIiM2YjU0NDIiLCJncnkiLDE1OSwiY20iLDE1OQ0KIjIwMjEiLCIxODhpbiIsIjIwMDgiLE5BLCI1Njk1ODM1MDkiLE5BLCJmNzQ4MjMiLCJoemwiLDE4OCwiaW4iLDQ3Ny41Mg0KIjIwMTYiLCI2N2NtIiwiMjAxMCIsIjE0NCIsIjM0OTQ5MjI0MyIsIjIwMjEiLCJ6IiwiIzI0Y2VlZSIsNjcsImNtIiw2Nw0KTkEsIjE2M2luIiwiMjAyOSIsTkEsTkEsIjE5NjIiLCIzYTBjMzAiLCJncnkiLDE2MywiaW4iLDQxNC4wMg0KIjIwMjUiLCIxNTIiLCIyMDEyIixOQSwiMzgyMTc0NzQ0IiwiMjAyNCIsIiM4ODg3ODUiLCIjOTVkOGE5IiwxNTIsTkEsTkENCiIxOTk3IiwiNjljbSIsIjIwMTIiLCI1NCIsIjE4NGNtIiwiMjAzNyIsImY4ZWQ0NSIsImdyeSIsNjksImNtIiw2OQ0KIjE5NTUiLCIxNjNpbiIsIjIwMTgiLCIyOTgiLCI4NzE1NTI2NiIsIjIwMjEiLCIjYzA5NDZmIiwiIzIxNjkyMCIsMTYzLCJpbiIsNDE0LjAyDQoiMjAxMiIsIjE4NWNtIixOQSxOQSxOQSwiMjAzMCIsIiNiNjY1MmEiLCJncm4iLDE4NSwiY20iLDE4NQ0KTkEsIjYzY20iLCIyMDA0IiwiMTg4IiwiNTYyMTIwMDEzNCIsIjE5MjQiLCI2ZWY5YmEiLCIjZWY2OGY1Iiw2MywiY20iLDYzDQoiMjAyMSIsIjE2N2luIiwiMjAyNSIsIjIzNCIsIjE4MWNtIiwiMTk0MSIsIjk4NjE5YSIsIiNmNWU2NTEiLDE2NywiaW4iLDQyNC4xOA0KIjIwMjgiLCIxMTEiLCIyMDMwIiwiMTgwIiwiMTgzMzkxODYxIiwiMTk1NCIsIjFmYjMwZiIsIiMwZDAxNjAiLDExMSxOQSxOQQ0KTkEsIjE5MWNtIiwiMjAyMyIsTkEsIjcyNzAyNDY3NiIsIjIwMjUiLCIjYjY2NTJhIiwiIzBiM2IyZCIsMTkxLCJjbSIsMTkxDQoiMjAxNiIsIjEzNSIsIjIwMTIiLE5BLCI3NDg3ODY4NzciLCIyMDExIiwiYjZlOTYyIiwiZ3J5IiwxMzUsTkEsTkENCk5BLE5BLCIyMDIzIiwiMjcxIiwiIzY2ZWM4MiIsIjIwMjkiLCIxMGQ5ZDgiLCIjMzUzZTBmIixOQSxOQSxOQQ0KIjIwMTMiLCIxODVjbSIsIjIwMTQiLE5BLCI4MTY0ODUwNTQiLCIyMDE5IiwiI2VmY2M5OCIsImdybiIsMTg1LCJjbSIsMTg1DQoiMTk4MSIsIjU5Y20iLCIyMDA5IixOQSwiMTYyY20iLCIyMDI1IiwiMTE2NzQyIiwiZ210Iiw1OSwiY20iLDU5DQoiMjAxMyIsIjE1N2luIiwiMjAyMyIsIjE4NiIsIjI3MTQ1MDc1NCIsIjIwMTYiLCJlMjA4ODIiLCJ1dGMiLDE1NywiaW4iLDM5OC43OA0KIjIwMTgiLCIxODZjbSIsTkEsTkEsIjE3ODUyNTEzMiIsIjIwMjMiLCIjODg4Nzg1IixOQSwxODYsImNtIiwxODYNCiIxOTI5IiwiMTkzIixOQSwiMzMwIiwiMzM3NjU0MjYiLCIyMDM4IiwieiIsIiMxOGU4ODMiLDE5MyxOQSxOQQ0KIjIwMTkiLCIxNjljbSIsIjIwMTIiLCIyOTgiLCIwNjYwMzE2NTU4IiwiMTkyMCIsIiM4ODg3ODUiLCJ6enoiLDE2OSwiY20iLDE2OQ0KIjE5OTciLCI2MWNtIiwiMjAyMyIsTkEsIjE2MmNtIiwiMTk4NyIsInoiLCIjOWY0NThjIiw2MSwiY20iLDYxDQoiMjAyMCIsTkEsIjIwMTAiLE5BLCIwNjU2MDc2NDkiLE5BLCI1OWUzNzYiLCJibHUiLE5BLE5BLE5BDQoiMjAxNSIsIjE1MGNtIiwiMjAyMiIsTkEsIjE2N2NtIiwiMjAzMiIsImNhYTE0NSIsIiMwNjY1MGEiLDE1MCwiY20iLDE1MA0KIjIwMjQiLCIxODFpbiIsIjIwMjkiLCI5NyIsIiNjMzg2MjAiLCIxOTc2IiwieiIsInp6eiIsMTgxLCJpbiIsNDU5Ljc0DQoiMjAxNyIsIjYxaW4iLE5BLCI5MyIsIjgwNzk5MDQ3NiIsIjIwMjciLCIjY2ZhMDdkIiwib3RoIiw2MSwiaW4iLDE1NC45NA0KIjIwMjMiLCIxNjJjbSIsIjIwMjUiLCIzNDMiLCI1MDQyNDAzNSIsIjIwMjQiLCI2MDUyMjMiLCJvdGgiLDE2MiwiY20iLDE2Mg0KIjIwMDEiLCIxNzFjbSIsIjIwMTMiLE5BLCI4OTAwOTY4MzI1IiwiMjAyMiIsIjY5OTExNiIsTkEsMTcxLCJjbSIsMTcxDQoiMjAwOSIsIjE3MmluIiwiMjAwNSIsIjMzNCIsIjE4OWNtIiwiMjAzMiIsInoiLCJ4cnkiLDE3MiwiaW4iLDQzNi44OA0KIjIwMjAiLCIxNTljbSIsTkEsTkEsIjE2NmNtIiwiMjAyNiIsInoiLCJvdGgiLDE1OSwiY20iLDE1OQ0KIjIwMTAiLCIxNjciLCIyMDAzIiwiMTY5IixOQSwiMjAzNyIsIjQ4NjgwMCIsIiMyOWJkZDYiLDE2NyxOQSxOQQ0KIjIwMjkiLCIxODBjbSIsIjIwMjciLCIyMDUiLCI0NDM4MDkzMzciLCIxOTgwIiwiIzM0MWUxMyIsImdydCIsMTgwLCJjbSIsMTgwDQoiMTk4MiIsIjE0NCIsIjIwMTIiLE5BLCIjM2I0M2MxIiwiMjAzMiIsIjJmMjZhYiIsIiNmODlkZjAiLDE0NCxOQSxOQQ0KIjIwMjAiLCIxODVpbiIsIjIwMjgiLE5BLCI3NzM3Mzk3NDQiLCIxOTg2IiwieiIsImRuZSIsMTg1LCJpbiIsNDY5LjkNCiIyMDI0IiwiMTY3aW4iLCIyMDA2IiwiMzQ2IiwiMTk1MzEzNDEiLCIyMDM1IiwieiIsIiNkMzIzMjAiLDE2NywiaW4iLDQyNC4xOA0KIjIwMTUiLCIxNTJjbSIsIjIwMTkiLE5BLCIxODJjbSIsIjIwMjgiLCI0M2Q1NmQiLCJ6enoiLDE1MiwiY20iLDE1Mg0KIjIwMTEiLCI2NWluIixOQSxOQSwiODUwMTkyNTAyIiwiMjAyMyIsIiM3ZDNiMGMiLCJncnkiLDY1LCJpbiIsMTY1LjENCiIxOTU4IiwiMTc2Y20iLE5BLE5BLCIjNGNiNDgwIiwiMjAyNiIsInoiLCJkbmUiLDE3NiwiY20iLDE3Ng0KIjIwMTMiLCI3NGNtIiwiMjAwNyIsIjMxNCIsIjE4NmNtIiwiMTk3MyIsIjE4MGUwYyIsImh6bCIsNzQsImNtIiw3NA0KIjIwMTciLCIxNjZjbSIsIjIwMTIiLCIxNzIiLCIjNDI0YWU0IiwiMjAyMiIsImIxMzE5YiIsIiM2NjM1ZDgiLDE2NiwiY20iLDE2Ng0KIjIwMjAiLCI3MGNtIiwiMjAwNCIsIjE2NiIsTkEsIjIwNDAiLCIjNzMzODIwIiwibHpyIiw3MCwiY20iLDcwDQoiMjAxMCIsIjE1NWluIiwiMjAxNiIsIjYxIiwiOTU5NDI4MzgwMyIsIjIwMjgiLCIjY2ZhMDdkIiwiZ3JuIiwxNTUsImluIiwzOTMuNw0KIjIwMTEiLCIxNzZjbSIsIjIwMTIiLCI2NCIsIjIxMzIxMzM1OSIsIjE5NzEiLCJiZTdiMTMiLCJnbXQiLDE3NiwiY20iLDE3Ng0KIjIwMTIiLCIxNjRjbSIsIjIwMDgiLE5BLCI0MjAxNjg0ODEiLCIyMDIzIiwiI2I2NjUyYSIsImdybiIsMTY0LCJjbSIsMTY0DQoiMjAyMSIsIjgyIiwiMjAwNyIsIjE5MSIsIiMxY2Y2OWYiLCIyMDM5IiwieiIsImRuZSIsODIsTkEsTkENCiIxOTUwIiwiNjZjbSIsTkEsIjExMSIsIjE4M2NtIiwiMTk0NyIsTkEsIiMwMTZmNmEiLDY2LCJjbSIsNjYNCiIyMDIwIiwiMTkzaW4iLCIyMDI2IiwiODIiLE5BLCIyMDM0IiwiI2I2NjUyYSIsImdybiIsMTkzLCJpbiIsNDkwLjIyDQoiMTkyMiIsIjEwMSIsIjIwMTUiLCIxMzYiLCIxNTFjbSIsIjIwNDAiLCIyNDVjYjMiLCJsenIiLDEwMSxOQSxOQQ0KIjIwMjgiLCIxOTNpbiIsIjIwMjUiLCIzMDgiLCI5MzM1MTUzMjg5IiwiMjAyOSIsInoiLCJncnkiLDE5MywiaW4iLDQ5MC4yMg0K" download="extract_0001.csv">
<button aria-label="There are 66 &#39;fail&#39; rows available as a CSV file." data-balloon-pos="left" style="background-color:#67C2DC;color:#FFFFFF;border:none;padding:5px;font-weight:bold;cursor:pointer;border-radius:4px;">CSV</button>
</a>

</td>
</tr>
<tr>
<td class="gt_row gt_left" style="background-color: rgba(76,166,76,0.5); height:  40px">
</td>
<td class="gt_row gt_right" style="color: #666666; font-size: 13px; font-weight: bold; height:  40px">
2
</td>
<td class="gt_row gt_left" style="height:  40px">

<svg width="30px" height="30px" viewBox="0 0 67 67" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<defs>
<path d="M10.712234,0 L56.712234,0 C62.2350815,-1.01453063e-15 66.712234,4.4771525 66.712234,10 L66.712234,66 L10.712234,66 C5.18938647,66 0.712233968,61.5228475 0.712233968,56 L0.712233968,10 C0.712233968,4.4771525 5.18938647,1.01453063e-15 10.712234,0 Z" id="path-1"></path>
</defs>
<g id="pointblank" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
<g id="col_vals_between" transform="translate(-0.487938, 0.651308)">
<g id="rectangle">
<use fill="#FFFFFF" fill-rule="evenodd" xlink:href="#path-1"></use>
<path stroke="#000000" stroke-width="2" d="M65.712234,65 L65.712234,10 C65.712234,5.02943725 61.6827967,1 56.712234,1 L10.712234,1 C5.74167122,1 1.71223397,5.02943725 1.71223397,10 L1.71223397,56 C1.71223397,60.9705627 5.74167122,65 10.712234,65 L65.712234,65 Z"></path>
</g>
<path d="M11.993484,21.96875 C10.962234,22.082031 10.188797,22.964844 10.212234,24 L10.212234,42 C10.200515,42.722656 10.579422,43.390625 11.204422,43.753906 C11.825515,44.121094 12.598953,44.121094 13.220047,43.753906 C13.845047,43.390625 14.223953,42.722656 14.212234,42 L14.212234,24 C14.220047,23.457031 14.009109,22.9375 13.626297,22.554688 C13.243484,22.171875 12.723953,21.960938 12.180984,21.96875 C12.118484,21.964844 12.055984,21.964844 11.993484,21.96875 Z M55.993484,21.96875 C54.962234,22.082031 54.188797,22.964844 54.212234,24 L54.212234,42 C54.200515,42.722656 54.579422,43.390625 55.204422,43.753906 C55.825515,44.121094 56.598953,44.121094 57.220047,43.753906 C57.845047,43.390625 58.223953,42.722656 58.212234,42 L58.212234,24 C58.220047,23.457031 58.009109,22.9375 57.626297,22.554688 C57.243484,22.171875 56.723953,21.960938 56.180984,21.96875 C56.118484,21.964844 56.055984,21.964844 55.993484,21.96875 Z M16.212234,22 C15.661453,22 15.212234,22.449219 15.212234,23 C15.212234,23.550781 15.661453,24 16.212234,24 C16.763015,24 17.212234,23.550781 17.212234,23 C17.212234,22.449219 16.763015,22 16.212234,22 Z M20.212234,22 C19.661453,22 19.212234,22.449219 19.212234,23 C19.212234,23.550781 19.661453,24 20.212234,24 C20.763015,24 21.212234,23.550781 21.212234,23 C21.212234,22.449219 20.763015,22 20.212234,22 Z M24.212234,22 C23.661453,22 23.212234,22.449219 23.212234,23 C23.212234,23.550781 23.661453,24 24.212234,24 C24.763015,24 25.212234,23.550781 25.212234,23 C25.212234,22.449219 24.763015,22 24.212234,22 Z M28.212234,22 C27.661453,22 27.212234,22.449219 27.212234,23 C27.212234,23.550781 27.661453,24 28.212234,24 C28.763015,24 29.212234,23.550781 29.212234,23 C29.212234,22.449219 28.763015,22 28.212234,22 Z M32.212234,22 C31.661453,22 31.212234,22.449219 31.212234,23 C31.212234,23.550781 31.661453,24 32.212234,24 C32.763015,24 33.212234,23.550781 33.212234,23 C33.212234,22.449219 32.763015,22 32.212234,22 Z M36.212234,22 C35.661453,22 35.212234,22.449219 35.212234,23 C35.212234,23.550781 35.661453,24 36.212234,24 C36.763015,24 37.212234,23.550781 37.212234,23 C37.212234,22.449219 36.763015,22 36.212234,22 Z M40.212234,22 C39.661453,22 39.212234,22.449219 39.212234,23 C39.212234,23.550781 39.661453,24 40.212234,24 C40.763015,24 41.212234,23.550781 41.212234,23 C41.212234,22.449219 40.763015,22 40.212234,22 Z M44.212234,22 C43.661453,22 43.212234,22.449219 43.212234,23 C43.212234,23.550781 43.661453,24 44.212234,24 C44.763015,24 45.212234,23.550781 45.212234,23 C45.212234,22.449219 44.763015,22 44.212234,22 Z M48.212234,22 C47.661453,22 47.212234,22.449219 47.212234,23 C47.212234,23.550781 47.661453,24 48.212234,24 C48.763015,24 49.212234,23.550781 49.212234,23 C49.212234,22.449219 48.763015,22 48.212234,22 Z M52.212234,22 C51.661453,22 51.212234,22.449219 51.212234,23 C51.212234,23.550781 51.661453,24 52.212234,24 C52.763015,24 53.212234,23.550781 53.212234,23 C53.212234,22.449219 52.763015,22 52.212234,22 Z M21.462234,27.96875 C21.419265,27.976563 21.376297,27.988281 21.337234,28 C21.177078,28.027344 21.02864,28.089844 20.899734,28.1875 L15.618484,32.1875 C15.356765,32.375 15.200515,32.679688 15.200515,33 C15.200515,33.320313 15.356765,33.625 15.618484,33.8125 L20.899734,37.8125 C21.348953,38.148438 21.985672,38.058594 22.321609,37.609375 C22.657547,37.160156 22.567703,36.523438 22.118484,36.1875 L19.212234,34 L49.212234,34 L46.305984,36.1875 C45.856765,36.523438 45.766922,37.160156 46.102859,37.609375 C46.438797,38.058594 47.075515,38.148438 47.524734,37.8125 L52.805984,33.8125 C53.067703,33.625 53.223953,33.320313 53.223953,33 C53.223953,32.679688 53.067703,32.375 52.805984,32.1875 L47.524734,28.1875 C47.30989,28.027344 47.040359,27.960938 46.774734,28 C46.743484,28 46.712234,28 46.680984,28 C46.282547,28.074219 45.96614,28.382813 45.884109,28.78125 C45.802078,29.179688 45.970047,29.585938 46.305984,29.8125 L49.212234,32 L19.212234,32 L22.118484,29.8125 C22.520828,29.566406 22.696609,29.070313 22.536453,28.625 C22.380203,28.179688 21.930984,27.90625 21.462234,27.96875 Z M16.212234,42 C15.661453,42 15.212234,42.449219 15.212234,43 C15.212234,43.550781 15.661453,44 16.212234,44 C16.763015,44 17.212234,43.550781 17.212234,43 C17.212234,42.449219 16.763015,42 16.212234,42 Z M20.212234,42 C19.661453,42 19.212234,42.449219 19.212234,43 C19.212234,43.550781 19.661453,44 20.212234,44 C20.763015,44 21.212234,43.550781 21.212234,43 C21.212234,42.449219 20.763015,42 20.212234,42 Z M24.212234,42 C23.661453,42 23.212234,42.449219 23.212234,43 C23.212234,43.550781 23.661453,44 24.212234,44 C24.763015,44 25.212234,43.550781 25.212234,43 C25.212234,42.449219 24.763015,42 24.212234,42 Z M28.212234,42 C27.661453,42 27.212234,42.449219 27.212234,43 C27.212234,43.550781 27.661453,44 28.212234,44 C28.763015,44 29.212234,43.550781 29.212234,43 C29.212234,42.449219 28.763015,42 28.212234,42 Z M32.212234,42 C31.661453,42 31.212234,42.449219 31.212234,43 C31.212234,43.550781 31.661453,44 32.212234,44 C32.763015,44 33.212234,43.550781 33.212234,43 C33.212234,42.449219 32.763015,42 32.212234,42 Z M36.212234,42 C35.661453,42 35.212234,42.449219 35.212234,43 C35.212234,43.550781 35.661453,44 36.212234,44 C36.763015,44 37.212234,43.550781 37.212234,43 C37.212234,42.449219 36.763015,42 36.212234,42 Z M40.212234,42 C39.661453,42 39.212234,42.449219 39.212234,43 C39.212234,43.550781 39.661453,44 40.212234,44 C40.763015,44 41.212234,43.550781 41.212234,43 C41.212234,42.449219 40.763015,42 40.212234,42 Z M44.212234,42 C43.661453,42 43.212234,42.449219 43.212234,43 C43.212234,43.550781 43.661453,44 44.212234,44 C44.763015,44 45.212234,43.550781 45.212234,43 C45.212234,42.449219 44.763015,42 44.212234,42 Z M48.212234,42 C47.661453,42 47.212234,42.449219 47.212234,43 C47.212234,43.550781 47.661453,44 48.212234,44 C48.763015,44 49.212234,43.550781 49.212234,43 C49.212234,42.449219 48.763015,42 48.212234,42 Z M52.212234,42 C51.661453,42 51.212234,42.449219 51.212234,43 C51.212234,43.550781 51.661453,44 52.212234,44 C52.763015,44 53.212234,43.550781 53.212234,43 C53.212234,42.449219 52.763015,42 52.212234,42 Z" id="inside_range" fill="#000000" fill-rule="nonzero"></path>
</g> </g>
</svg>

<code style="font-size:11px;"> col\_vals\_between()</code>

</td>
<td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">

<p style="margin-top:0;margin-bottom:0;font-size:11px;white-space:nowrap;text-overflow:ellipsis;overflow:hidden;line-height:2em;">
<code><span style="color:purple;">▮</span>iyr</code>
</p>

</td>
<td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">

<p style="margin-top: 0px; margin-bottom: 0px; font-size: 11px; white-space: nowrap; text-overflow: ellipsis; overflow: hidden;">
<code>\[2,010, 2,020\]</code>
</p>

</td>
<td class="gt_row gt_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; background-color: #FCFCFC; height:  40px">

<p>
<span
style="background:transparent;padding:0;color:#333333;vertical-align:middle;font-size:18px;border:none;border-radius:4px;"
aria-label="No modifications of the table."
data-balloon-pos="left">→</span>
</p>

</td>
<td class="gt_row gt_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; background-color: #FCFCFC; height:  40px">

<p>
<span
style="background:transparent;padding:5px;color:#4CA64C;vertical-align:middle;font-size:15px;border:none;"
aria-label="No evaluation issues." data-balloon-pos="left">✓</span>
</p>

</td>
<td class="gt_row gt_right" style="height:  40px">
<code>276</code>
</td>
<td class="gt_row gt_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">
<code>209</code><br><code>0.76</code>
</td>
<td class="gt_row gt_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">
<code>67</code><br><code>0.24</code>
</td>
<td class="gt_row gt_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; background-color: #FCFCFC; height:  40px">

<p>
—
</p>

</td>
<td class="gt_row gt_center" style="background-color: #FCFCFC; height:  40px">

<p>
—
</p>

</td>
<td class="gt_row gt_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; background-color: #FCFCFC; height:  40px">

<p>
—
</p>

</td>
<td class="gt_row gt_center" style="height:  40px">

<a href="data:text/csv;base64,Iml5ciIsImhndCIsImJ5ciIsImNpZCIsInBpZCIsImV5ciIsImhjbCIsImVjbCIsImhndF9udW0iLCJoZ3RfdW5pdCIsImhndF9jbSINCiIyMDI1IiwiNjRjbSIsIjIwMjkiLCIyODEiLCIwNjcyODU5ODUiLCIxOTQ0IiwiI2NlYjNhMSIsIiMwNzIxOWEiLDY0LCJjbSIsNjQNCiIxOTMwIiwiMTgxY20iLCIxOTUwIixOQSwiNjA0OTgzNDY2IiwiMjAzOSIsIiNiNjY1MmEiLCIjOTA2NTQ4IiwxODEsImNtIiwxODENCiIyMDI1IiwiMTQxIiwiMjAwNiIsIjMyNyIsIiMxYzQyY2MiLCIxOTU2IiwieiIsIiNmMmFmZmMiLDE0MSxOQSxOQQ0KIjE5MjQiLCI3NGNtIiwiMjAyNCIsIjE1MyIsIjM2MTk2NDAxIiwiMTkyMSIsImE0ZTRjMCIsIiMzYWNmNTciLDc0LCJjbSIsNzQNCiIxOTMzIiwiOTAiLCIyMDI1IixOQSwiODE5NDM0NzU0NCIsIjIwNDAiLE5BLCJkbmUiLDkwLE5BLE5BDQoiMTkyMCIsIjYwY20iLCIyMDI5IiwiMTA3IiwiIzU1Y2U2YiIsIjIwNDAiLCJkMzBmNmIiLCJkbmUiLDYwLCJjbSIsNjANCiIxOTM4IiwiMTcyaW4iLCIyMDE4IiwiMzM5IiwiIzZjMTIxNiIsIjIwMzkiLCJ6IiwiIzUxMDY3MiIsMTcyLCJpbiIsNDM2Ljg4DQoiMTk5NyIsIjk2IiwiMjAwOSIsTkEsIjA2MzY5MTcyMjIiLCIyMDI2IiwieiIsImh6bCIsOTYsTkEsTkENCiIxOTUxIiwiMTkzaW4iLCIyMDE5IixOQSwiI2NiYzA4YyIsIjIwMDIiLCJ6IiwiIzNlOWYyZiIsMTkzLCJpbiIsNDkwLjIyDQpOQSwiMTc1aW4iLCIyMDEyIiwiMjQwIiwiNTcxNDc3MTc2IiwiMTkyOSIsImY0ZWYzMiIsInV0YyIsMTc1LCJpbiIsNDQ0LjUNCiIyMDI2IiwiNzAiLCIyMDIyIixOQSwiNDA0NjIyMDgzIiwiMTk3OSIsImMxYmE3ZiIsImx6ciIsNzAsTkEsTkENCk5BLCI4MiIsIjIwMTciLCIxNTMiLCIxNzI2NjE2MTciLCIyMDM2IiwieiIsIiNhZTYwN2QiLDgyLE5BLE5BDQoiMTk0NSIsIjE2NWluIiwiMjAxMCIsIjI3MyIsIjQzNzA3NTA0IiwiMjAyNiIsInoiLCJncnQiLDE2NSwiaW4iLDQxOS4xDQoiMTk0MSIsIjE2MWNtIiwiMTkyMyIsIjI4NSIsIjgwODM5MjMxNCIsIjIwMjAiLCIjZWZjYzk4IiwiZ3J5IiwxNjEsImNtIiwxNjENCk5BLCIxODFjbSIsIjE5OTIiLCIzMjAiLCIwMzI3Njk3NTciLCIyMDIyIiwiIzczMzgyMCIsImdybiIsMTgxLCJjbSIsMTgxDQoiMjAyMSIsIjE4OGluIiwiMjAwOCIsTkEsIjU2OTU4MzUwOSIsTkEsImY3NDgyMyIsImh6bCIsMTg4LCJpbiIsNDc3LjUyDQpOQSwiMTYzaW4iLCIyMDI5IixOQSxOQSwiMTk2MiIsIjNhMGMzMCIsImdyeSIsMTYzLCJpbiIsNDE0LjAyDQoiMjAyNCIsIjE4MCIsIjE5MjciLCI4NyIsIjU5NzA5ODk0MCIsIjIwMjciLCIjNjIzYTJmIiwiIzdlYTc3NyIsMTgwLE5BLE5BDQoiMjAyNSIsIjE1MiIsIjIwMTIiLE5BLCIzODIxNzQ3NDQiLCIyMDI0IiwiIzg4ODc4NSIsIiM5NWQ4YTkiLDE1MixOQSxOQQ0KTkEsIjE4M2NtIiwiMTkzNCIsTkEsTkEsIjIwMjMiLCIjNmI1NDQyIiwiZ3JuIiwxODMsImNtIiwxODMNCiIxOTk3IiwiNjljbSIsIjIwMTIiLCI1NCIsIjE4NGNtIiwiMjAzNyIsImY4ZWQ0NSIsImdyeSIsNjksImNtIiw2OQ0KIjE5NTUiLCIxNjNpbiIsIjIwMTgiLCIyOTgiLCI4NzE1NTI2NiIsIjIwMjEiLCIjYzA5NDZmIiwiIzIxNjkyMCIsMTYzLCJpbiIsNDE0LjAyDQpOQSwiMTgyY20iLCIxOTkzIiwiMTE3IiwiMDczMDM1OTk5IiwiMjAzMCIsIiMzNDFlMTMiLE5BLDE4MiwiY20iLDE4Mg0KIjE5NDAiLCIxNzNjbSIsIjE5NDciLE5BLE5BLCIxOTQ3IiwiN2U1MTVjIiwiZ210IiwxNzMsImNtIiwxNzMNCk5BLCI2M2NtIiwiMjAwNCIsIjE4OCIsIjU2MjEyMDAxMzQiLCIxOTI0IiwiNmVmOWJhIiwiI2VmNjhmNSIsNjMsImNtIiw2Mw0KIjIwMjEiLCIxNjdpbiIsIjIwMjUiLCIyMzQiLCIxODFjbSIsIjE5NDEiLCI5ODYxOWEiLCIjZjVlNjUxIiwxNjcsImluIiw0MjQuMTgNCiIxOTQ5IiwiNzNjbSIsIjE5ODUiLCIyMDciLCIjZWU5Zjk1IiwiMjAyOCIsInoiLCJ1dGMiLDczLCJjbSIsNzMNCiIxOTMzIiwiNjljbSIsIjE5MzQiLE5BLCIxNzljbSIsIjIwMzAiLCJiOGUxNDIiLCJncm4iLDY5LCJjbSIsNjkNCiIyMDI4IiwiMTExIiwiMjAzMCIsIjE4MCIsIjE4MzM5MTg2MSIsIjE5NTQiLCIxZmIzMGYiLCIjMGQwMTYwIiwxMTEsTkEsTkENCk5BLCIxOTFjbSIsIjIwMjMiLE5BLCI3MjcwMjQ2NzYiLCIyMDI1IiwiI2I2NjUyYSIsIiMwYjNiMmQiLDE5MSwiY20iLDE5MQ0KTkEsTkEsIjIwMjMiLCIyNzEiLCIjNjZlYzgyIiwiMjAyOSIsIjEwZDlkOCIsIiMzNTNlMGYiLE5BLE5BLE5BDQoiMTk4MSIsIjU5Y20iLCIyMDA5IixOQSwiMTYyY20iLCIyMDI1IiwiMTE2NzQyIiwiZ210Iiw1OSwiY20iLDU5DQoiMjAyMyIsIjY3Y20iLCIxOTYzIixOQSwiNjI4NTkzMzIiLCIyMDI4IiwiM2QxZjM0IiwiZG5lIiw2NywiY20iLDY3DQpOQSwiMTkxY20iLCIxOTg5IiwiMTE5IiwiNTU2MDExNDM0IiwiMjAyNSIsIiM4NjY4NTciLCJhbWIiLDE5MSwiY20iLDE5MQ0KIjIwMDMiLCIxNTAiLCIyMDAxIiwiMjkyIiwiOTM0NzMxOTIiLCIxOTIyIiwieiIsIiNkNTZiYmQiLDE1MCxOQSxOQQ0KIjE5MjkiLCIxOTMiLE5BLCIzMzAiLCIzMzc2NTQyNiIsIjIwMzgiLCJ6IiwiIzE4ZTg4MyIsMTkzLE5BLE5BDQoiMTk0OSIsIjE2MGNtIiwiMTk1NSIsTkEsIjc0MzA5NDM0NSIsIjIwMjciLCI4ZGFlNjciLCJncnkiLDE2MCwiY20iLDE2MA0KIjIwMjUiLCIxNjJjbSIsIjE5ODkiLCIzMTkiLCI2NzMxMTIyMiIsIjE5MzkiLCJ6IiwidXRjIiwxNjIsImNtIiwxNjINCiIxOTY1IiwiNjAiLCIxOTMxIixOQSwiI2FhNWZkMCIsIjIwMTciLCI1NzkyNjYiLCJncnkiLDYwLE5BLE5BDQoiMTk5NyIsIjYxY20iLCIyMDIzIixOQSwiMTYyY20iLCIxOTg3IiwieiIsIiM5ZjQ1OGMiLDYxLCJjbSIsNjENCk5BLCIxOTJjbSIsIjE5ODMiLCIyNDIiLCI4Mzk2MDg2MTYiLCIyMDI2IiwiI2NlYjNhMSIsImh6bCIsMTkyLCJjbSIsMTkyDQoiMjAyNCIsIjE4MWluIiwiMjAyOSIsIjk3IiwiI2MzODYyMCIsIjE5NzYiLCJ6Iiwienp6IiwxODEsImluIiw0NTkuNzQNCiIyMDI1IiwiNTlpbiIsIjE5MjAiLCIyMDIiLCIxODJjbSIsIjIwMzUiLCJ6IiwiIzc5OWYyOSIsNTksImluIiwxNDkuODYNCk5BLCI3M2NtIiwiMTkyNyIsTkEsIjcxOTczODQ2OCIsIjIwMzkiLE5BLCIjYTgyZTkwIiw3MywiY20iLDczDQoiMjAyMyIsIjE2MmNtIiwiMjAyNSIsIjM0MyIsIjUwNDI0MDM1IiwiMjAyNCIsIjYwNTIyMyIsIm90aCIsMTYyLCJjbSIsMTYyDQoiMjAwMSIsIjE3MWNtIiwiMjAxMyIsTkEsIjg5MDA5NjgzMjUiLCIyMDIyIiwiNjk5MTE2IixOQSwxNzEsImNtIiwxNzENCiIyMDA5IiwiMTcyaW4iLCIyMDA1IiwiMzM0IiwiMTg5Y20iLCIyMDMyIiwieiIsInhyeSIsMTcyLCJpbiIsNDM2Ljg4DQoiMTkzNCIsIjE4MGNtIiwiMTk0MiIsTkEsIjQyNzAwMTU5NyIsIjIwMzgiLCIjYTk3ODQyIiwiYnJuIiwxODAsImNtIiwxODANCk5BLCIxODhjbSIsIjE5ODgiLCIyNjciLCI2OTY2MTcyMzIiLCIyMDI4IiwiIzE4MTcxZCIsImFtYiIsMTg4LCJjbSIsMTg4DQoiMjAyOSIsIjE4MGNtIiwiMjAyNyIsIjIwNSIsIjQ0MzgwOTMzNyIsIjE5ODAiLCIjMzQxZTEzIiwiZ3J0IiwxODAsImNtIiwxODANCiIxOTgyIiwiMTQ0IiwiMjAxMiIsTkEsIiMzYjQzYzEiLCIyMDMyIiwiMmYyNmFiIiwiI2Y4OWRmMCIsMTQ0LE5BLE5BDQoiMjAyMiIsIjE2MGNtIiwiMTk4OCIsTkEsIjc4ODgwNTE3OSIsIjIwMjMiLCIjODY2ODU3IiwiYW1iIiwxNjAsImNtIiwxNjANCk5BLCIxNjRjbSIsIjE5OTYiLCIzMzgiLCIyMDg1OTYwMTQiLCIyMDI5IiwiI2VmY2M5OCIsImJsdSIsMTY0LCJjbSIsMTY0DQpOQSwiMTkyY20iLCIxOTYwIixOQSwiMzU3NjgwMDY0IiwiMjAyOSIsIiNjMDk0NmYiLCJncnkiLDE5MiwiY20iLDE5Mg0KIjIwMjQiLCIxNjdpbiIsIjIwMDYiLCIzNDYiLCIxOTUzMTM0MSIsIjIwMzUiLCJ6IiwiI2QzMjMyMCIsMTY3LCJpbiIsNDI0LjE4DQoiMTk1OCIsIjE3NmNtIixOQSxOQSwiIzRjYjQ4MCIsIjIwMjYiLCJ6IiwiZG5lIiwxNzYsImNtIiwxNzYNCiIxOTI4IiwiMTg1Y20iLCIxOTg0IixOQSwiI2FjNWE5MCIsIjIwMzAiLCJhYzhmNDMiLCJicm4iLDE4NSwiY20iLDE4NQ0KTkEsIjEyOCIsIjE5OTciLCIyOTkiLCI5ODQ2NzUxOTgiLCIyMDM3IiwiI2I2NjUyYSIsImdtdCIsMTI4LE5BLE5BDQoiMTk3MSIsIjE1N2luIiwiMTk3MCIsTkEsIjgxMjYxNzgwOSIsIjIwMjAiLCIjN2QzYjBjIiwiZ210IiwxNTcsImluIiwzOTguNzgNCk5BLCI2NmNtIiwiMTkyOCIsIjg3IiwiMjcxMDc5NDYiLCIyMDQwIixOQSwidXRjIiw2NiwiY20iLDY2DQoiMjAyOSIsIjY4IiwiMTk1OSIsTkEsIjkwMTc2MDk0OTciLCIyMDIzIiwiNGUwMjNiIiwiYmx1Iiw2OCxOQSxOQQ0KIjIwMjEiLCI4MiIsIjIwMDciLCIxOTEiLCIjMWNmNjlmIiwiMjAzOSIsInoiLCJkbmUiLDgyLE5BLE5BDQoiMTk1MCIsIjY2Y20iLE5BLCIxMTEiLCIxODNjbSIsIjE5NDciLE5BLCIjMDE2ZjZhIiw2NiwiY20iLDY2DQpOQSwiMTg5Y20iLCIxOTc0IiwiMjE0IiwiNzg1Njg4NTQyIiwiMjAzMCIsIiMxODE3MWQiLCJicm4iLDE4OSwiY20iLDE4OQ0KIjIwMjMiLE5BLCIxOTY2IixOQSwiMTY0Y20iLCIyMDIxIiwieiIsInV0YyIsTkEsTkEsTkENCiIxOTIyIiwiMTAxIiwiMjAxNSIsIjEzNiIsIjE1MWNtIiwiMjA0MCIsIjI0NWNiMyIsImx6ciIsMTAxLE5BLE5BDQoiMjAyOCIsIjE5M2luIiwiMjAyNSIsIjMwOCIsIjkzMzUxNTMyODkiLCIyMDI5IiwieiIsImdyeSIsMTkzLCJpbiIsNDkwLjIyDQo=" download="extract_0002.csv">
<button aria-label="There are 67 &#39;fail&#39; rows available as a CSV file." data-balloon-pos="left" style="background-color:#67C2DC;color:#FFFFFF;border:none;padding:5px;font-weight:bold;cursor:pointer;border-radius:4px;">CSV</button>
</a>

</td>
</tr>
<tr>
<td class="gt_row gt_left" style="background-color: rgba(76,166,76,0.5); height:  40px">
</td>
<td class="gt_row gt_right" style="color: #666666; font-size: 13px; font-weight: bold; height:  40px">
3
</td>
<td class="gt_row gt_left" style="height:  40px">

<svg width="30px" height="30px" viewBox="0 0 67 67" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<defs>
<path d="M10.712234,0 L56.712234,0 C62.2350815,-1.01453063e-15 66.712234,4.4771525 66.712234,10 L66.712234,66 L10.712234,66 C5.18938647,66 0.712233968,61.5228475 0.712233968,56 L0.712233968,10 C0.712233968,4.4771525 5.18938647,1.01453063e-15 10.712234,0 Z" id="path-1"></path>
</defs>
<g id="pointblank" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
<g id="col_vals_between" transform="translate(-0.487938, 0.651308)">
<g id="rectangle">
<use fill="#FFFFFF" fill-rule="evenodd" xlink:href="#path-1"></use>
<path stroke="#000000" stroke-width="2" d="M65.712234,65 L65.712234,10 C65.712234,5.02943725 61.6827967,1 56.712234,1 L10.712234,1 C5.74167122,1 1.71223397,5.02943725 1.71223397,10 L1.71223397,56 C1.71223397,60.9705627 5.74167122,65 10.712234,65 L65.712234,65 Z"></path>
</g>
<path d="M11.993484,21.96875 C10.962234,22.082031 10.188797,22.964844 10.212234,24 L10.212234,42 C10.200515,42.722656 10.579422,43.390625 11.204422,43.753906 C11.825515,44.121094 12.598953,44.121094 13.220047,43.753906 C13.845047,43.390625 14.223953,42.722656 14.212234,42 L14.212234,24 C14.220047,23.457031 14.009109,22.9375 13.626297,22.554688 C13.243484,22.171875 12.723953,21.960938 12.180984,21.96875 C12.118484,21.964844 12.055984,21.964844 11.993484,21.96875 Z M55.993484,21.96875 C54.962234,22.082031 54.188797,22.964844 54.212234,24 L54.212234,42 C54.200515,42.722656 54.579422,43.390625 55.204422,43.753906 C55.825515,44.121094 56.598953,44.121094 57.220047,43.753906 C57.845047,43.390625 58.223953,42.722656 58.212234,42 L58.212234,24 C58.220047,23.457031 58.009109,22.9375 57.626297,22.554688 C57.243484,22.171875 56.723953,21.960938 56.180984,21.96875 C56.118484,21.964844 56.055984,21.964844 55.993484,21.96875 Z M16.212234,22 C15.661453,22 15.212234,22.449219 15.212234,23 C15.212234,23.550781 15.661453,24 16.212234,24 C16.763015,24 17.212234,23.550781 17.212234,23 C17.212234,22.449219 16.763015,22 16.212234,22 Z M20.212234,22 C19.661453,22 19.212234,22.449219 19.212234,23 C19.212234,23.550781 19.661453,24 20.212234,24 C20.763015,24 21.212234,23.550781 21.212234,23 C21.212234,22.449219 20.763015,22 20.212234,22 Z M24.212234,22 C23.661453,22 23.212234,22.449219 23.212234,23 C23.212234,23.550781 23.661453,24 24.212234,24 C24.763015,24 25.212234,23.550781 25.212234,23 C25.212234,22.449219 24.763015,22 24.212234,22 Z M28.212234,22 C27.661453,22 27.212234,22.449219 27.212234,23 C27.212234,23.550781 27.661453,24 28.212234,24 C28.763015,24 29.212234,23.550781 29.212234,23 C29.212234,22.449219 28.763015,22 28.212234,22 Z M32.212234,22 C31.661453,22 31.212234,22.449219 31.212234,23 C31.212234,23.550781 31.661453,24 32.212234,24 C32.763015,24 33.212234,23.550781 33.212234,23 C33.212234,22.449219 32.763015,22 32.212234,22 Z M36.212234,22 C35.661453,22 35.212234,22.449219 35.212234,23 C35.212234,23.550781 35.661453,24 36.212234,24 C36.763015,24 37.212234,23.550781 37.212234,23 C37.212234,22.449219 36.763015,22 36.212234,22 Z M40.212234,22 C39.661453,22 39.212234,22.449219 39.212234,23 C39.212234,23.550781 39.661453,24 40.212234,24 C40.763015,24 41.212234,23.550781 41.212234,23 C41.212234,22.449219 40.763015,22 40.212234,22 Z M44.212234,22 C43.661453,22 43.212234,22.449219 43.212234,23 C43.212234,23.550781 43.661453,24 44.212234,24 C44.763015,24 45.212234,23.550781 45.212234,23 C45.212234,22.449219 44.763015,22 44.212234,22 Z M48.212234,22 C47.661453,22 47.212234,22.449219 47.212234,23 C47.212234,23.550781 47.661453,24 48.212234,24 C48.763015,24 49.212234,23.550781 49.212234,23 C49.212234,22.449219 48.763015,22 48.212234,22 Z M52.212234,22 C51.661453,22 51.212234,22.449219 51.212234,23 C51.212234,23.550781 51.661453,24 52.212234,24 C52.763015,24 53.212234,23.550781 53.212234,23 C53.212234,22.449219 52.763015,22 52.212234,22 Z M21.462234,27.96875 C21.419265,27.976563 21.376297,27.988281 21.337234,28 C21.177078,28.027344 21.02864,28.089844 20.899734,28.1875 L15.618484,32.1875 C15.356765,32.375 15.200515,32.679688 15.200515,33 C15.200515,33.320313 15.356765,33.625 15.618484,33.8125 L20.899734,37.8125 C21.348953,38.148438 21.985672,38.058594 22.321609,37.609375 C22.657547,37.160156 22.567703,36.523438 22.118484,36.1875 L19.212234,34 L49.212234,34 L46.305984,36.1875 C45.856765,36.523438 45.766922,37.160156 46.102859,37.609375 C46.438797,38.058594 47.075515,38.148438 47.524734,37.8125 L52.805984,33.8125 C53.067703,33.625 53.223953,33.320313 53.223953,33 C53.223953,32.679688 53.067703,32.375 52.805984,32.1875 L47.524734,28.1875 C47.30989,28.027344 47.040359,27.960938 46.774734,28 C46.743484,28 46.712234,28 46.680984,28 C46.282547,28.074219 45.96614,28.382813 45.884109,28.78125 C45.802078,29.179688 45.970047,29.585938 46.305984,29.8125 L49.212234,32 L19.212234,32 L22.118484,29.8125 C22.520828,29.566406 22.696609,29.070313 22.536453,28.625 C22.380203,28.179688 21.930984,27.90625 21.462234,27.96875 Z M16.212234,42 C15.661453,42 15.212234,42.449219 15.212234,43 C15.212234,43.550781 15.661453,44 16.212234,44 C16.763015,44 17.212234,43.550781 17.212234,43 C17.212234,42.449219 16.763015,42 16.212234,42 Z M20.212234,42 C19.661453,42 19.212234,42.449219 19.212234,43 C19.212234,43.550781 19.661453,44 20.212234,44 C20.763015,44 21.212234,43.550781 21.212234,43 C21.212234,42.449219 20.763015,42 20.212234,42 Z M24.212234,42 C23.661453,42 23.212234,42.449219 23.212234,43 C23.212234,43.550781 23.661453,44 24.212234,44 C24.763015,44 25.212234,43.550781 25.212234,43 C25.212234,42.449219 24.763015,42 24.212234,42 Z M28.212234,42 C27.661453,42 27.212234,42.449219 27.212234,43 C27.212234,43.550781 27.661453,44 28.212234,44 C28.763015,44 29.212234,43.550781 29.212234,43 C29.212234,42.449219 28.763015,42 28.212234,42 Z M32.212234,42 C31.661453,42 31.212234,42.449219 31.212234,43 C31.212234,43.550781 31.661453,44 32.212234,44 C32.763015,44 33.212234,43.550781 33.212234,43 C33.212234,42.449219 32.763015,42 32.212234,42 Z M36.212234,42 C35.661453,42 35.212234,42.449219 35.212234,43 C35.212234,43.550781 35.661453,44 36.212234,44 C36.763015,44 37.212234,43.550781 37.212234,43 C37.212234,42.449219 36.763015,42 36.212234,42 Z M40.212234,42 C39.661453,42 39.212234,42.449219 39.212234,43 C39.212234,43.550781 39.661453,44 40.212234,44 C40.763015,44 41.212234,43.550781 41.212234,43 C41.212234,42.449219 40.763015,42 40.212234,42 Z M44.212234,42 C43.661453,42 43.212234,42.449219 43.212234,43 C43.212234,43.550781 43.661453,44 44.212234,44 C44.763015,44 45.212234,43.550781 45.212234,43 C45.212234,42.449219 44.763015,42 44.212234,42 Z M48.212234,42 C47.661453,42 47.212234,42.449219 47.212234,43 C47.212234,43.550781 47.661453,44 48.212234,44 C48.763015,44 49.212234,43.550781 49.212234,43 C49.212234,42.449219 48.763015,42 48.212234,42 Z M52.212234,42 C51.661453,42 51.212234,42.449219 51.212234,43 C51.212234,43.550781 51.661453,44 52.212234,44 C52.763015,44 53.212234,43.550781 53.212234,43 C53.212234,42.449219 52.763015,42 52.212234,42 Z" id="inside_range" fill="#000000" fill-rule="nonzero"></path>
</g> </g>
</svg>

<code style="font-size:11px;"> col\_vals\_between()</code>

</td>
<td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">

<p style="margin-top:0;margin-bottom:0;font-size:11px;white-space:nowrap;text-overflow:ellipsis;overflow:hidden;line-height:2em;">
<code><span style="color:purple;">▮</span>eyr</code>
</p>

</td>
<td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">

<p style="margin-top: 0px; margin-bottom: 0px; font-size: 11px; white-space: nowrap; text-overflow: ellipsis; overflow: hidden;">
<code>\[2,020, 2,030\]</code>
</p>

</td>
<td class="gt_row gt_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; background-color: #FCFCFC; height:  40px">

<p>
<span
style="background:transparent;padding:0;color:#333333;vertical-align:middle;font-size:18px;border:none;border-radius:4px;"
aria-label="No modifications of the table."
data-balloon-pos="left">→</span>
</p>

</td>
<td class="gt_row gt_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; background-color: #FCFCFC; height:  40px">

<p>
<span
style="background:transparent;padding:5px;color:#4CA64C;vertical-align:middle;font-size:15px;border:none;"
aria-label="No evaluation issues." data-balloon-pos="left">✓</span>
</p>

</td>
<td class="gt_row gt_right" style="height:  40px">
<code>276</code>
</td>
<td class="gt_row gt_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">
<code>210</code><br><code>0.76</code>
</td>
<td class="gt_row gt_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">
<code>66</code><br><code>0.24</code>
</td>
<td class="gt_row gt_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; background-color: #FCFCFC; height:  40px">

<p>
—
</p>

</td>
<td class="gt_row gt_center" style="background-color: #FCFCFC; height:  40px">

<p>
—
</p>

</td>
<td class="gt_row gt_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; background-color: #FCFCFC; height:  40px">

<p>
—
</p>

</td>
<td class="gt_row gt_center" style="height:  40px">

<a href="data:text/csv;base64,Iml5ciIsImhndCIsImJ5ciIsImNpZCIsInBpZCIsImV5ciIsImhjbCIsImVjbCIsImhndF9udW0iLCJoZ3RfdW5pdCIsImhndF9jbSINCiIyMDE1IiwiNTljbSIsIjIwMjkiLCIyMTkiLCI5MzgxNjg4NzUzIiwiMTk5MiIsIiNiNjY1MmEiLCIjN2EwZmE2Iiw1OSwiY20iLDU5DQoiMjAyNSIsIjY0Y20iLCIyMDI5IiwiMjgxIiwiMDY3Mjg1OTg1IiwiMTk0NCIsIiNjZWIzYTEiLCIjMDcyMTlhIiw2NCwiY20iLDY0DQoiMjAxMiIsIjE2M2NtIiwiMTk5MCIsIjI3NSIsIjI2MDA0MzU3MCIsTkEsIiNiNjY1MmEiLCJicm4iLDE2MywiY20iLDE2Mw0KIjE5MzAiLCIxODFjbSIsIjE5NTAiLE5BLCI2MDQ5ODM0NjYiLCIyMDM5IiwiI2I2NjUyYSIsIiM5MDY1NDgiLDE4MSwiY20iLDE4MQ0KIjIwMjUiLCIxNDEiLCIyMDA2IiwiMzI3IiwiIzFjNDJjYyIsIjE5NTYiLCJ6IiwiI2YyYWZmYyIsMTQxLE5BLE5BDQoiMTkyNCIsIjc0Y20iLCIyMDI0IiwiMTUzIiwiMzYxOTY0MDEiLCIxOTIxIiwiYTRlNGMwIiwiIzNhY2Y1NyIsNzQsImNtIiw3NA0KIjE5MzMiLCI5MCIsIjIwMjUiLE5BLCI4MTk0MzQ3NTQ0IiwiMjA0MCIsTkEsImRuZSIsOTAsTkEsTkENCiIxOTIwIiwiNjBjbSIsIjIwMjkiLCIxMDciLCIjNTVjZTZiIiwiMjA0MCIsImQzMGY2YiIsImRuZSIsNjAsImNtIiw2MA0KIjIwMTYiLCIxNzJjbSIsTkEsTkEsIiM5OGNhZWMiLCIyMDM2IiwieiIsImRuZSIsMTcyLCJjbSIsMTcyDQoiMTkzOCIsIjE3MmluIiwiMjAxOCIsIjMzOSIsIiM2YzEyMTYiLCIyMDM5IiwieiIsIiM1MTA2NzIiLDE3MiwiaW4iLDQzNi44OA0KIjIwMTEiLCIxNzBjbSIsIjE5ODkiLCIxNTUiLCIwNzE1ODg2ODIiLCIxOTU1IiwiI2NlYjNhMSIsImdybiIsMTcwLCJjbSIsMTcwDQoiMjAxNyIsIjE3NWNtIiwiMTk2NCIsIjI2NiIsTkEsTkEsIiNhOTc4NDIiLCJicm4iLDE3NSwiY20iLDE3NQ0KIjE5NTEiLCIxOTNpbiIsIjIwMTkiLE5BLCIjY2JjMDhjIiwiMjAwMiIsInoiLCIjM2U5ZjJmIiwxOTMsImluIiw0OTAuMjINCk5BLCIxNzVpbiIsIjIwMTIiLCIyNDAiLCI1NzE0NzcxNzYiLCIxOTI5IiwiZjRlZjMyIiwidXRjIiwxNzUsImluIiw0NDQuNQ0KIjIwMjYiLCI3MCIsIjIwMjIiLE5BLCI0MDQ2MjIwODMiLCIxOTc5IiwiYzFiYTdmIiwibHpyIiw3MCxOQSxOQQ0KTkEsIjgyIiwiMjAxNyIsIjE1MyIsIjE3MjY2MTYxNyIsIjIwMzYiLCJ6IiwiI2FlNjA3ZCIsODIsTkEsTkENCiIyMDIxIiwiMTg4aW4iLCIyMDA4IixOQSwiNTY5NTgzNTA5IixOQSwiZjc0ODIzIiwiaHpsIiwxODgsImluIiw0NzcuNTINCk5BLCIxNjNpbiIsIjIwMjkiLE5BLE5BLCIxOTYyIiwiM2EwYzMwIiwiZ3J5IiwxNjMsImluIiw0MTQuMDINCiIxOTk3IiwiNjljbSIsIjIwMTIiLCI1NCIsIjE4NGNtIiwiMjAzNyIsImY4ZWQ0NSIsImdyeSIsNjksImNtIiw2OQ0KIjIwMTciLCI4MyIsIjE5NDAiLE5BLE5BLCIyMDMyIiwiMTc0Nzc0IiwieHJ5Iiw4MyxOQSxOQQ0KIjE5NDAiLCIxNzNjbSIsIjE5NDciLE5BLE5BLCIxOTQ3IiwiN2U1MTVjIiwiZ210IiwxNzMsImNtIiwxNzMNCiIyMDEyIiwiNjJjbSIsIjE5ODUiLCI1OSIsIiNhZjEwNmEiLCIxOTU3IiwiIzYyM2EyZiIsImdydCIsNjIsImNtIiw2Mg0KTkEsIjYzY20iLCIyMDA0IiwiMTg4IiwiNTYyMTIwMDEzNCIsIjE5MjQiLCI2ZWY5YmEiLCIjZWY2OGY1Iiw2MywiY20iLDYzDQoiMjAyMSIsIjE2N2luIiwiMjAyNSIsIjIzNCIsIjE4MWNtIiwiMTk0MSIsIjk4NjE5YSIsIiNmNWU2NTEiLDE2NywiaW4iLDQyNC4xOA0KIjIwMjgiLCIxMTEiLCIyMDMwIiwiMTgwIiwiMTgzMzkxODYxIiwiMTk1NCIsIjFmYjMwZiIsIiMwZDAxNjAiLDExMSxOQSxOQQ0KIjIwMTYiLCIxMzUiLCIyMDEyIixOQSwiNzQ4Nzg2ODc3IiwiMjAxMSIsImI2ZTk2MiIsImdyeSIsMTM1LE5BLE5BDQoiMjAxMyIsIjE4NWNtIiwiMjAxNCIsTkEsIjgxNjQ4NTA1NCIsIjIwMTkiLCIjZWZjYzk4IiwiZ3JuIiwxODUsImNtIiwxODUNCiIyMDEzIiwiMTU3aW4iLCIyMDIzIiwiMTg2IiwiMjcxNDUwNzU0IiwiMjAxNiIsImUyMDg4MiIsInV0YyIsMTU3LCJpbiIsMzk4Ljc4DQoiMjAxOCIsIjE5M2luIiwiMTk0MyIsTkEsIjcwMjIwMDAyNiIsIjE5NjgiLCIjODg4Nzg1IiwiZ210IiwxOTMsImluIiw0OTAuMjINCiIyMDAzIiwiMTUwIiwiMjAwMSIsIjI5MiIsIjkzNDczMTkyIiwiMTkyMiIsInoiLCIjZDU2YmJkIiwxNTAsTkEsTkENCiIxOTI5IiwiMTkzIixOQSwiMzMwIiwiMzM3NjU0MjYiLCIyMDM4IiwieiIsIiMxOGU4ODMiLDE5MyxOQSxOQQ0KIjIwMjUiLCIxNjJjbSIsIjE5ODkiLCIzMTkiLCI2NzMxMTIyMiIsIjE5MzkiLCJ6IiwidXRjIiwxNjIsImNtIiwxNjINCiIyMDE5IiwiMTY5Y20iLCIyMDEyIiwiMjk4IiwiMDY2MDMxNjU1OCIsIjE5MjAiLCIjODg4Nzg1Iiwienp6IiwxNjksImNtIiwxNjkNCiIxOTY1IiwiNjAiLCIxOTMxIixOQSwiI2FhNWZkMCIsIjIwMTciLCI1NzkyNjYiLCJncnkiLDYwLE5BLE5BDQoiMjAxNSIsIjE1OGNtIiwiMTkyOCIsIjIwNyIsIjQ0MzI0Njc5MSIsTkEsIiMxODE3MWQiLCJicm4iLDE1OCwiY20iLDE1OA0KIjE5OTciLCI2MWNtIiwiMjAyMyIsTkEsIjE2MmNtIiwiMTk4NyIsInoiLCIjOWY0NThjIiw2MSwiY20iLDYxDQoiMjAyMCIsTkEsIjIwMTAiLE5BLCIwNjU2MDc2NDkiLE5BLCI1OWUzNzYiLCJibHUiLE5BLE5BLE5BDQoiMjAxNSIsIjE1MGNtIiwiMjAyMiIsTkEsIjE2N2NtIiwiMjAzMiIsImNhYTE0NSIsIiMwNjY1MGEiLDE1MCwiY20iLDE1MA0KIjIwMTciLE5BLCIxOTMyIiwiMjAzIiwiMTA1OTIxMDg1IixOQSwiIzQxOWQ3MyIsImdyeSIsTkEsTkEsTkENCiIyMDI0IiwiMTgxaW4iLCIyMDI5IiwiOTciLCIjYzM4NjIwIiwiMTk3NiIsInoiLCJ6enoiLDE4MSwiaW4iLDQ1OS43NA0KIjIwMjUiLCI1OWluIiwiMTkyMCIsIjIwMiIsIjE4MmNtIiwiMjAzNSIsInoiLCIjNzk5ZjI5Iiw1OSwiaW4iLDE0OS44Ng0KIjIwMTMiLE5BLCIxOTUxIiwiMjU3IiwiMTIzMDY1NjM5IixOQSwiIzdkM2IwYyIsImdyeSIsTkEsTkEsTkENCk5BLCI3M2NtIiwiMTkyNyIsTkEsIjcxOTczODQ2OCIsIjIwMzkiLE5BLCIjYTgyZTkwIiw3MywiY20iLDczDQoiMjAwOSIsIjE3MmluIiwiMjAwNSIsIjMzNCIsIjE4OWNtIiwiMjAzMiIsInoiLCJ4cnkiLDE3MiwiaW4iLDQzNi44OA0KIjIwMTAiLCIxNjciLCIyMDAzIiwiMTY5IixOQSwiMjAzNyIsIjQ4NjgwMCIsIiMyOWJkZDYiLDE2NyxOQSxOQQ0KIjE5MzQiLCIxODBjbSIsIjE5NDIiLE5BLCI0MjcwMDE1OTciLCIyMDM4IiwiI2E5Nzg0MiIsImJybiIsMTgwLCJjbSIsMTgwDQoiMjAxNiIsIjE1MWNtIiwiMTk0OCIsIjMyNyIsIjM0MTk3ODgyMiIsTkEsIiM3MzM4MjAiLCJvdGgiLDE1MSwiY20iLDE1MQ0KIjIwMjkiLCIxODBjbSIsIjIwMjciLCIyMDUiLCI0NDM4MDkzMzciLCIxOTgwIiwiIzM0MWUxMyIsImdydCIsMTgwLCJjbSIsMTgwDQoiMTk4MiIsIjE0NCIsIjIwMTIiLE5BLCIjM2I0M2MxIiwiMjAzMiIsIjJmMjZhYiIsIiNmODlkZjAiLDE0NCxOQSxOQQ0KIjIwMjAiLCIxODVpbiIsIjIwMjgiLE5BLCI3NzM3Mzk3NDQiLCIxOTg2IiwieiIsImRuZSIsMTg1LCJpbiIsNDY5LjkNCiIyMDI0IiwiMTY3aW4iLCIyMDA2IiwiMzQ2IiwiMTk1MzEzNDEiLCIyMDM1IiwieiIsIiNkMzIzMjAiLDE2NywiaW4iLDQyNC4xOA0KIjIwMTMiLCI3NGNtIiwiMjAwNyIsIjMxNCIsIjE4NmNtIiwiMTk3MyIsIjE4MGUwYyIsImh6bCIsNzQsImNtIiw3NA0KIjIwMTAiLCIxODRjbSIsIjE5ODUiLE5BLCIjMTc1MTI5IiwiMjAzMiIsIiNmZmZmZmQiLCJoemwiLDE4NCwiY20iLDE4NA0KIjIwMjAiLCI3MGNtIiwiMjAwNCIsIjE2NiIsTkEsIjIwNDAiLCIjNzMzODIwIiwibHpyIiw3MCwiY20iLDcwDQpOQSwiMTI4IiwiMTk5NyIsIjI5OSIsIjk4NDY3NTE5OCIsIjIwMzciLCIjYjY2NTJhIiwiZ210IiwxMjgsTkEsTkENCiIyMDExIiwiMTc2Y20iLCIyMDEyIiwiNjQiLCIyMTMyMTMzNTkiLCIxOTcxIiwiYmU3YjEzIiwiZ210IiwxNzYsImNtIiwxNzYNCk5BLCI2NmNtIiwiMTkyOCIsIjg3IiwiMjcxMDc5NDYiLCIyMDQwIixOQSwidXRjIiw2NiwiY20iLDY2DQoiMjAxOCIsIjE1MGluIiwiMTkzNCIsIjE2MyIsIjI4NjMyODQ3NTQiLCIxOTc3IiwiIzYyM2EyZiIsImJybiIsMTUwLCJpbiIsMzgxDQoiMjAxMCIsIjE4NmNtIiwiMTk0MSIsIjE0NSIsIjcyMjA1NjEzOSIsIjIwMzEiLCIjY2ZhMDdkIiwiaHpsIiwxODYsImNtIiwxODYNCiIyMDIxIiwiODIiLCIyMDA3IiwiMTkxIiwiIzFjZjY5ZiIsIjIwMzkiLCJ6IiwiZG5lIiw4MixOQSxOQQ0KIjE5NTAiLCI2NmNtIixOQSwiMTExIiwiMTgzY20iLCIxOTQ3IixOQSwiIzAxNmY2YSIsNjYsImNtIiw2Ng0KIjIwMjAiLCIxNjNjbSIsIjE5NjciLCIzMDkiLCIxMjg1NTg0MjkzIiwiMjAzMSIsIiM3MzM4MjAiLCJvdGgiLDE2MywiY20iLDE2Mw0KIjIwMTQiLCIxNjljbSIsIjE5NjYiLE5BLCI2MjE4NzY1MzIiLE5BLCIjZWZjYzk4IiwiZ3J5IiwxNjksImNtIiwxNjkNCiIyMDEyIiwiMTgwY20iLCIxOTQzIiwiMTQ0IiwiMTU1Y20iLCIyMDM1IixOQSwiYW1iIiwxODAsImNtIiwxODANCiIyMDIwIiwiMTkzaW4iLCIyMDI2IiwiODIiLE5BLCIyMDM0IiwiI2I2NjUyYSIsImdybiIsMTkzLCJpbiIsNDkwLjIyDQoiMTkyMiIsIjEwMSIsIjIwMTUiLCIxMzYiLCIxNTFjbSIsIjIwNDAiLCIyNDVjYjMiLCJsenIiLDEwMSxOQSxOQQ0K" download="extract_0003.csv">
<button aria-label="There are 66 &#39;fail&#39; rows available as a CSV file." data-balloon-pos="left" style="background-color:#67C2DC;color:#FFFFFF;border:none;padding:5px;font-weight:bold;cursor:pointer;border-radius:4px;">CSV</button>
</a>

</td>
</tr>
<tr>
<td class="gt_row gt_left" style="background-color: rgba(76,166,76,0.5); height:  40px">
</td>
<td class="gt_row gt_right" style="color: #666666; font-size: 13px; font-weight: bold; height:  40px">
4
</td>
<td class="gt_row gt_left" style="height:  40px">

<svg width="30px" height="30px" viewBox="0 0 67 67" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<defs>
<path d="M10.712234,0 L56.712234,0 C62.2350815,-1.01453063e-15 66.712234,4.4771525 66.712234,10 L66.712234,66 L10.712234,66 C5.18938647,66 0.712233968,61.5228475 0.712233968,56 L0.712233968,10 C0.712233968,4.4771525 5.18938647,1.01453063e-15 10.712234,0 Z" id="path-1"></path>
</defs>
<g id="pointblank" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
<g id="col_vals_regex" transform="translate(-0.500000, 0.651308)">
<g id="rectangle">
<use fill="#FFFFFF" fill-rule="evenodd" xlink:href="#path-1"></use>
<path stroke="#000000" stroke-width="2" d="M65.712234,65 L65.712234,10 C65.712234,5.02943725 61.6827967,1 56.712234,1 L10.712234,1 C5.74167122,1 1.71223397,5.02943725 1.71223397,10 L1.71223397,56 C1.71223397,60.9705627 5.74167122,65 10.712234,65 L65.712234,65 Z"></path>
</g>
<g id="regex_symbols" transform="translate(18.000000, 12.000000)" fill="#000000" fill-rule="nonzero">
<path d="M4.17434508,33.013582 C1.94895328,33.013582 0.138006923,34.8245284 0.138006923,37.0499202 C0.138006923,39.275312 1.94895328,41.0862583 4.17434508,41.0862583 C6.39973688,41.0862583 8.21068324,39.275312 8.21068324,37.0499202 C8.21068324,34.8245284 6.39973688,33.013582 4.17434508,33.013582 Z" id="full_stop"></path>
<path d="M23.9479718,23.3175402 L21.5628264,23.3175402 C21.2344032,23.3175402 20.9665401,23.0520067 20.9665401,22.7212538 L20.9665401,15.1022979 L14.3445004,18.8873192 C14.0626621,19.050366 13.7016292,18.952538 13.5362533,18.6706991 L12.3436806,16.6442575 C12.262157,16.506832 12.2388642,16.3437852 12.2807909,16.1900549 C12.3203879,16.0363251 12.4205455,15.9058874 12.557971,15.8266929 L19.1800101,11.9880994 L12.557971,8.15183511 C12.4205455,8.07264112 12.3203879,7.93987439 12.2807909,7.78614401 C12.2388642,7.63241423 12.262157,7.46936689 12.3413509,7.33194137 L13.5339237,5.30549975 C13.6993001,5.02366143 14.0626621,4.92816199 14.3445004,5.09120934 L20.9665401,8.87390091 L20.9665401,1.25494501 C20.9665401,0.926521818 21.2344032,0.658658658 21.5628264,0.658658658 L23.9479718,0.658658658 C24.2787247,0.658658658 24.5442582,0.926521818 24.5442582,1.25494501 L24.5442582,8.87390091 L31.1662979,5.09120934 C31.4481362,4.92816199 31.8091691,5.02366143 31.9745455,5.30549975 L33.1671182,7.33194137 C33.2486413,7.46936689 33.2719341,7.63241423 33.2300074,7.78614401 C33.1904104,7.93987439 33.0902528,8.07264112 32.9528278,8.15183511 L26.3307882,11.9880994 L32.9528278,15.8243638 C33.0879237,15.9058874 33.1880813,16.0363251 33.2300074,16.1900549 C33.269605,16.3437852 33.2486413,16.506832 33.1671182,16.6442575 L31.9745455,18.6706991 C31.8091691,18.952538 31.4481362,19.050366 31.1662979,18.8849895 L24.5442582,15.1022979 L24.5442582,22.7212538 C24.5442582,23.0520067 24.2787247,23.3175402 23.9479718,23.3175402 Z" id="asterisk"></path>
</g> </g> </g>
</svg>

<code style="font-size:11px;"> col\_vals\_regex()</code>

</td>
<td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">

<p style="margin-top:0;margin-bottom:0;font-size:11px;white-space:nowrap;text-overflow:ellipsis;overflow:hidden;line-height:2em;">
<code><span style="color:purple;">▮</span>hcl</code>
</p>

</td>
<td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">
<div class='gt_from_md'>
<div aria-label="^#[0-9a-f]{6}$" data-balloon-pos="left">
<p style="margin-top: 0px; margin-bottom: 0px; font-family: monospace; white-space: nowrap; text-overflow: ellipsis; overflow: hidden;">
<code>^\#\[0-9a-f\]{6}$&lt;/code&gt;&lt;/p&gt;&lt;/div&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: \#D3D3D3; background-color: \#FCFCFC; height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;p&gt;&lt;span style="background:transparent;padding:0;color:\#333333;vertical-align:middle;font-size:18px;border:none;border-radius:4px;" aria-label="No modifications of the table." data-balloon-pos="left"&gt;→&lt;/span&gt;&lt;/p&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: \#D3D3D3; background-color: \#FCFCFC; height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;p&gt;&lt;span style="background:transparent;padding:5px;color:\#4CA64C;vertical-align:middle;font-size:15px;border:none;" aria-label="No evaluation issues." data-balloon-pos="left"&gt;✓&lt;/span&gt;&lt;/p&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_right" style="height: 40px"&gt;&lt;code&gt;276&lt;/code&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: \#E5E5E5; height: 40px"&gt;&lt;code&gt;202&lt;/code&gt;&lt;br&gt;&lt;code&gt;0.73&lt;/code&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: \#E5E5E5; height: 40px"&gt;&lt;code&gt;74&lt;/code&gt;&lt;br&gt;&lt;code&gt;0.27&lt;/code&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: \#D3D3D3; background-color: \#FCFCFC; height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;p&gt;—&lt;/p&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_center" style="background-color: \#FCFCFC; height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;p&gt;—&lt;/p&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: \#D3D3D3; background-color: \#FCFCFC; height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;p&gt;—&lt;/p&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_center" style="height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;a href="data:text/csv;base64,Iml5ciIsImhndCIsImJ5ciIsImNpZCIsInBpZCIsImV5ciIsImhjbCIsImVjbCIsImhndF9udW0iLCJoZ3RfdW5pdCIsImhndF9jbSINCiIyMDE4IixOQSwiMTk2OCIsIjMzNSIsIjk0MzYxNDc1NSIsIjIwMjYiLE5BLCJibHUiLE5BLE5BLE5BDQoiMjAyNSIsIjE0MSIsIjIwMDYiLCIzMjciLCIjMWM0MmNjIiwiMTk1NiIsInoiLCIjZjJhZmZjIiwxNDEsTkEsTkENCiIxOTI0IiwiNzRjbSIsIjIwMjQiLCIxNTMiLCIzNjE5NjQwMSIsIjE5MjEiLCJhNGU0YzAiLCIjM2FjZjU3Iiw3NCwiY20iLDc0DQoiMjAyMCIsIjE1MSIsIjIwMTIiLCIzMTQiLCI5ODQzNjQ4NjIiLCIyMDIzIiwieiIsImRuZSIsMTUxLE5BLE5BDQoiMTkzMyIsIjkwIiwiMjAyNSIsTkEsIjgxOTQzNDc1NDQiLCIyMDQwIixOQSwiZG5lIiw5MCxOQSxOQQ0KIjE5MjAiLCI2MGNtIiwiMjAyOSIsIjEwNyIsIiM1NWNlNmIiLCIyMDQwIiwiZDMwZjZiIiwiZG5lIiw2MCwiY20iLDYwDQoiMjAxOCIsIjY1Y20iLCIxOTM3IixOQSwiIzRiZmYzZSIsIjIwMjciLCI0M2ZhZmIiLCJncnQiLDY1LCJjbSIsNjUNCiIyMDE2IiwiMTY3Y20iLCIxOTYzIiwiNTIiLCI5NTUyMDM3ODEiLCIyMDI2IixOQSwiZ3JuIiwxNjcsImNtIiwxNjcNCiIyMDE2IiwiMTcyY20iLE5BLE5BLCIjOThjYWVjIiwiMjAzNiIsInoiLCJkbmUiLDE3MiwiY20iLDE3Mg0KIjE5MzgiLCIxNzJpbiIsIjIwMTgiLCIzMzkiLCIjNmMxMjE2IiwiMjAzOSIsInoiLCIjNTEwNjcyIiwxNzIsImluIiw0MzYuODgNCiIxOTk3IiwiOTYiLCIyMDA5IixOQSwiMDYzNjkxNzIyMiIsIjIwMjYiLCJ6IiwiaHpsIiw5NixOQSxOQQ0KIjE5NTEiLCIxOTNpbiIsIjIwMTkiLE5BLCIjY2JjMDhjIiwiMjAwMiIsInoiLCIjM2U5ZjJmIiwxOTMsImluIiw0OTAuMjINCk5BLCIxNzVpbiIsIjIwMTIiLCIyNDAiLCI1NzE0NzcxNzYiLCIxOTI5IiwiZjRlZjMyIiwidXRjIiwxNzUsImluIiw0NDQuNQ0KIjIwMjYiLCI3MCIsIjIwMjIiLE5BLCI0MDQ2MjIwODMiLCIxOTc5IiwiYzFiYTdmIiwibHpyIiw3MCxOQSxOQQ0KIjIwMTQiLCIxNTNjbSIsIjE5NTAiLE5BLCI0MTEwMzI2NTkiLCIyMDIwIixOQSwiaHpsIiwxNTMsImNtIiwxNTMNCiIyMDE4IiwiMTU2Y20iLCIyMDE3IiwiMTc5IiwiMjk4MzYxMjQiLCIyMDIzIiwiNTZkZTgzIiwienp6IiwxNTYsImNtIiwxNTYNCk5BLCI4MiIsIjIwMTciLCIxNTMiLCIxNzI2NjE2MTciLCIyMDM2IiwieiIsIiNhZTYwN2QiLDgyLE5BLE5BDQoiMTk0NSIsIjE2NWluIiwiMjAxMCIsIjI3MyIsIjQzNzA3NTA0IiwiMjAyNiIsInoiLCJncnQiLDE2NSwiaW4iLDQxOS4xDQoiMjAyMSIsIjE4OGluIiwiMjAwOCIsTkEsIjU2OTU4MzUwOSIsTkEsImY3NDgyMyIsImh6bCIsMTg4LCJpbiIsNDc3LjUyDQoiMjAxNiIsIjY3Y20iLCIyMDEwIiwiMTQ0IiwiMzQ5NDkyMjQzIiwiMjAyMSIsInoiLCIjMjRjZWVlIiw2NywiY20iLDY3DQpOQSwiMTYzaW4iLCIyMDI5IixOQSxOQSwiMTk2MiIsIjNhMGMzMCIsImdyeSIsMTYzLCJpbiIsNDE0LjAyDQoiMTk5NyIsIjY5Y20iLCIyMDEyIiwiNTQiLCIxODRjbSIsIjIwMzciLCJmOGVkNDUiLCJncnkiLDY5LCJjbSIsNjkNCiIyMDE3IiwiODMiLCIxOTQwIixOQSxOQSwiMjAzMiIsIjE3NDc3NCIsInhyeSIsODMsTkEsTkENCiIxOTQwIiwiMTczY20iLCIxOTQ3IixOQSxOQSwiMTk0NyIsIjdlNTE1YyIsImdtdCIsMTczLCJjbSIsMTczDQpOQSwiNjNjbSIsIjIwMDQiLCIxODgiLCI1NjIxMjAwMTM0IiwiMTkyNCIsIjZlZjliYSIsIiNlZjY4ZjUiLDYzLCJjbSIsNjMNCiIyMDIxIiwiMTY3aW4iLCIyMDI1IiwiMjM0IiwiMTgxY20iLCIxOTQxIiwiOTg2MTlhIiwiI2Y1ZTY1MSIsMTY3LCJpbiIsNDI0LjE4DQoiMTk0OSIsIjczY20iLCIxOTg1IiwiMjA3IiwiI2VlOWY5NSIsIjIwMjgiLCJ6IiwidXRjIiw3MywiY20iLDczDQoiMTkzMyIsIjY5Y20iLCIxOTM0IixOQSwiMTc5Y20iLCIyMDMwIiwiYjhlMTQyIiwiZ3JuIiw2OSwiY20iLDY5DQoiMjAyOCIsIjExMSIsIjIwMzAiLCIxODAiLCIxODMzOTE4NjEiLCIxOTU0IiwiMWZiMzBmIiwiIzBkMDE2MCIsMTExLE5BLE5BDQoiMjAxNiIsIjEzNSIsIjIwMTIiLE5BLCI3NDg3ODY4NzciLCIyMDExIiwiYjZlOTYyIiwiZ3J5IiwxMzUsTkEsTkENCk5BLE5BLCIyMDIzIiwiMjcxIiwiIzY2ZWM4MiIsIjIwMjkiLCIxMGQ5ZDgiLCIjMzUzZTBmIixOQSxOQSxOQQ0KIjE5ODEiLCI1OWNtIiwiMjAwOSIsTkEsIjE2MmNtIiwiMjAyNSIsIjExNjc0MiIsImdtdCIsNTksImNtIiw1OQ0KIjIwMjMiLCI2N2NtIiwiMTk2MyIsTkEsIjYyODU5MzMyIiwiMjAyOCIsIjNkMWYzNCIsImRuZSIsNjcsImNtIiw2Nw0KIjIwMTMiLCIxNTdpbiIsIjIwMjMiLCIxODYiLCIyNzE0NTA3NTQiLCIyMDE2IiwiZTIwODgyIiwidXRjIiwxNTcsImluIiwzOTguNzgNCiIyMDAzIiwiMTUwIiwiMjAwMSIsIjI5MiIsIjkzNDczMTkyIiwiMTkyMiIsInoiLCIjZDU2YmJkIiwxNTAsTkEsTkENCiIxOTI5IiwiMTkzIixOQSwiMzMwIiwiMzM3NjU0MjYiLCIyMDM4IiwieiIsIiMxOGU4ODMiLDE5MyxOQSxOQQ0KIjE5NDkiLCIxNjBjbSIsIjE5NTUiLE5BLCI3NDMwOTQzNDUiLCIyMDI3IiwiOGRhZTY3IiwiZ3J5IiwxNjAsImNtIiwxNjANCiIyMDI1IiwiMTYyY20iLCIxOTg5IiwiMzE5IiwiNjczMTEyMjIiLCIxOTM5IiwieiIsInV0YyIsMTYyLCJjbSIsMTYyDQoiMjAxNCIsIjE3MWNtIiwiMTk5NyIsIjMwMiIsIjEwMTM2MzM2NyIsIjIwMjUiLCJ6IiwiYW1iIiwxNzEsImNtIiwxNzENCiIyMDEwIiwiMTY0Y20iLCIxOTQ3IiwiOTYiLCIxNjYxMTU0NDIiLCIyMDMwIiwiNGJjMjBhIiwib3RoIiwxNjQsImNtIiwxNjQNCiIxOTY1IiwiNjAiLCIxOTMxIixOQSwiI2FhNWZkMCIsIjIwMTciLCI1NzkyNjYiLCJncnkiLDYwLE5BLE5BDQoiMTk5NyIsIjYxY20iLCIyMDIzIixOQSwiMTYyY20iLCIxOTg3IiwieiIsIiM5ZjQ1OGMiLDYxLCJjbSIsNjENCiIyMDIwIixOQSwiMjAxMCIsTkEsIjA2NTYwNzY0OSIsTkEsIjU5ZTM3NiIsImJsdSIsTkEsTkEsTkENCiIyMDE1IiwiMTUwY20iLCIyMDIyIixOQSwiMTY3Y20iLCIyMDMyIiwiY2FhMTQ1IiwiIzA2NjUwYSIsMTUwLCJjbSIsMTUwDQoiMjAxMCIsIjE3OWNtIiwiMTk0MCIsIjE1MyIsIjc0MDY5MjMyMSIsIjIwMjciLE5BLCJibHUiLDE3OSwiY20iLDE3OQ0KIjIwMjQiLCIxODFpbiIsIjIwMjkiLCI5NyIsIiNjMzg2MjAiLCIxOTc2IiwieiIsInp6eiIsMTgxLCJpbiIsNDU5Ljc0DQoiMjAxOCIsIjE2M2NtIiwiMTk5OSIsIjIwOSIsIjQwMTYwNjU3MSIsIjIwMjMiLCI2ZjI5YTYiLCJsenIiLDE2MywiY20iLDE2Mw0KIjIwMTQiLCIxNzJpbiIsIjE5NTAiLE5BLCIxODdjbSIsIjIwMjgiLCJ6IiwiYnJuIiwxNzIsImluIiw0MzYuODgNCiIyMDI1IiwiNTlpbiIsIjE5MjAiLCIyMDIiLCIxODJjbSIsIjIwMzUiLCJ6IiwiIzc5OWYyOSIsNTksImluIiwxNDkuODYNCk5BLCI3M2NtIiwiMTkyNyIsTkEsIjcxOTczODQ2OCIsIjIwMzkiLE5BLCIjYTgyZTkwIiw3MywiY20iLDczDQoiMjAyMyIsIjE2MmNtIiwiMjAyNSIsIjM0MyIsIjUwNDI0MDM1IiwiMjAyNCIsIjYwNTIyMyIsIm90aCIsMTYyLCJjbSIsMTYyDQoiMjAwMSIsIjE3MWNtIiwiMjAxMyIsTkEsIjg5MDA5NjgzMjUiLCIyMDIyIiwiNjk5MTE2IixOQSwxNzEsImNtIiwxNzENCiIyMDA5IiwiMTcyaW4iLCIyMDA1IiwiMzM0IiwiMTg5Y20iLCIyMDMyIiwieiIsInhyeSIsMTcyLCJpbiIsNDM2Ljg4DQoiMjAyMCIsIjE1OWNtIixOQSxOQSwiMTY2Y20iLCIyMDI2IiwieiIsIm90aCIsMTU5LCJjbSIsMTU5DQoiMjAxMCIsIjE2NyIsIjIwMDMiLCIxNjkiLE5BLCIyMDM3IiwiNDg2ODAwIiwiIzI5YmRkNiIsMTY3LE5BLE5BDQoiMTk4MiIsIjE0NCIsIjIwMTIiLE5BLCIjM2I0M2MxIiwiMjAzMiIsIjJmMjZhYiIsIiNmODlkZjAiLDE0NCxOQSxOQQ0KIjIwMjAiLCIxNzljbSIsIjE5NDAiLE5BLCI0Mzc4MjAyNTQiLCIyMDI2IixOQSwiZ3J5IiwxNzksImNtIiwxNzkNCiIyMDIwIiwiMTg1aW4iLCIyMDI4IixOQSwiNzczNzM5NzQ0IiwiMTk4NiIsInoiLCJkbmUiLDE4NSwiaW4iLDQ2OS45DQoiMjAyNCIsIjE2N2luIiwiMjAwNiIsIjM0NiIsIjE5NTMxMzQxIiwiMjAzNSIsInoiLCIjZDMyMzIwIiwxNjcsImluIiw0MjQuMTgNCiIyMDExIiwiMTYyY20iLCIxOTYxIiwiNTQiLCI4OTEzOTc5ODIiLCIyMDIwIixOQSwiYnJuIiwxNjIsImNtIiwxNjINCiIyMDE1IiwiMTUyY20iLCIyMDE5IixOQSwiMTgyY20iLCIyMDI4IiwiNDNkNTZkIiwienp6IiwxNTIsImNtIiwxNTINCiIxOTU4IiwiMTc2Y20iLE5BLE5BLCIjNGNiNDgwIiwiMjAyNiIsInoiLCJkbmUiLDE3NiwiY20iLDE3Ng0KIjIwMTMiLCI3NGNtIiwiMjAwNyIsIjMxNCIsIjE4NmNtIiwiMTk3MyIsIjE4MGUwYyIsImh6bCIsNzQsImNtIiw3NA0KIjIwMTciLCIxNjZjbSIsIjIwMTIiLCIxNzIiLCIjNDI0YWU0IiwiMjAyMiIsImIxMzE5YiIsIiM2NjM1ZDgiLDE2NiwiY20iLDE2Ng0KIjE5MjgiLCIxODVjbSIsIjE5ODQiLE5BLCIjYWM1YTkwIiwiMjAzMCIsImFjOGY0MyIsImJybiIsMTg1LCJjbSIsMTg1DQoiMjAxMSIsIjE3NmNtIiwiMjAxMiIsIjY0IiwiMjEzMjEzMzU5IiwiMTk3MSIsImJlN2IxMyIsImdtdCIsMTc2LCJjbSIsMTc2DQpOQSwiNjZjbSIsIjE5MjgiLCI4NyIsIjI3MTA3OTQ2IiwiMjA0MCIsTkEsInV0YyIsNjYsImNtIiw2Ng0KIjIwMjkiLCI2OCIsIjE5NTkiLE5BLCI5MDE3NjA5NDk3IiwiMjAyMyIsIjRlMDIzYiIsImJsdSIsNjgsTkEsTkENCiIyMDIxIiwiODIiLCIyMDA3IiwiMTkxIiwiIzFjZjY5ZiIsIjIwMzkiLCJ6IiwiZG5lIiw4MixOQSxOQQ0KIjE5NTAiLCI2NmNtIixOQSwiMTExIiwiMTgzY20iLCIxOTQ3IixOQSwiIzAxNmY2YSIsNjYsImNtIiw2Ng0KIjIwMjMiLE5BLCIxOTY2IixOQSwiMTY0Y20iLCIyMDIxIiwieiIsInV0YyIsTkEsTkEsTkENCiIyMDEyIiwiMTgwY20iLCIxOTQzIiwiMTQ0IiwiMTU1Y20iLCIyMDM1IixOQSwiYW1iIiwxODAsImNtIiwxODANCiIxOTIyIiwiMTAxIiwiMjAxNSIsIjEzNiIsIjE1MWNtIiwiMjA0MCIsIjI0NWNiMyIsImx6ciIsMTAxLE5BLE5BDQoiMjAyOCIsIjE5M2luIiwiMjAyNSIsIjMwOCIsIjkzMzUxNTMyODkiLCIyMDI5IiwieiIsImdyeSIsMTkzLCJpbiIsNDkwLjIyDQo=" download="extract\_0004.csv"&gt;  &lt;button aria-label="There are 74 &\#39;fail&\#39; rows available as a CSV file." data-balloon-pos="left" style="background-color:\#67C2DC;color:\#FFFFFF;border:none;padding:5px;font-weight:bold;cursor:pointer;border-radius:4px;"&gt;CSV&lt;/button&gt; &lt;/a&gt; &lt;/div&gt;&lt;/td&gt;  &lt;/tr&gt;  &lt;tr&gt;  &lt;td class="gt\_row gt\_left" style="background-color: rgba(76,166,76,0.5); height: 40px"&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_right" style="color: \#666666; font-size: 13px; font-weight: bold; height: 40px"&gt;5&lt;/td&gt;  &lt;td class="gt\_row gt\_left" style="height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;div aria-label="Expect that values in \`ecl\` should be in the set of \`amb\`, \`blu\`, \`brn\` (and 4 more). " data-balloon-pos="right" style="width:fit-content;"&gt;  &lt;div style="margin:0;padding:0;display:inline-block;height:30px;vertical-align:middle;"&gt;&lt;svg width="30px" height="30px" viewBox="0 0 67 67" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"&gt; &lt;defs&gt; &lt;path d="M10.712234,0 L56.712234,0 C62.2350815,-1.01453063e-15 66.712234,4.4771525 66.712234,10 L66.712234,66 L10.712234,66 C5.18938647,66 0.712233968,61.5228475 0.712233968,56 L0.712233968,10 C0.712233968,4.4771525 5.18938647,1.01453063e-15 10.712234,0 Z" id="path-1"&gt;&lt;/path&gt; &lt;/defs&gt; &lt;g id="pointblank" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd"&gt; &lt;g id="col\_vals\_in\_set" transform="translate(-0.487938, 0.859210)"&gt; &lt;g id="rectangle"&gt; &lt;use fill="\#FFFFFF" fill-rule="evenodd" xlink:href="\#path-1"&gt;&lt;/use&gt; &lt;path stroke="\#000000" stroke-width="2" d="M65.712234,65 L65.712234,10 C65.712234,5.02943725 61.6827967,1 56.712234,1 L10.712234,1 C5.74167122,1 1.71223397,5.02943725 1.71223397,10 L1.71223397,56 C1.71223397,60.9705627 5.74167122,65 10.712234,65 L65.712234,65 Z"&gt;&lt;/path&gt; &lt;/g&gt; &lt;path d="M44.127969,41.1538382 L31.0814568,41.1538382 C29.9510748,41.1536429 28.8827052,40.9256134 27.9079888,40.5136953 C26.4467442,39.8960136 25.19849,38.8599685 24.3189894,37.5577099 C23.8792391,36.906727 23.5314818,36.1899233 23.2936866,35.4252675 C23.2130217,35.16589 23.1460289,34.9005554 23.0913409,34.6307286 L44.1278714,34.6307286 C45.028466,34.6306309 45.7586488,33.9004481 45.7586488,32.9998535 C45.7586488,32.0992589 45.028466,31.3690761 44.1278714,31.3690761 L23.0905596,31.3690761 C23.1990567,30.8337194 23.3597028,30.3180894 23.5675173,29.8264831 C24.185199,28.3652386 25.2212442,27.1169844 26.5236004,26.2374838 C27.1745833,25.7977334 27.891387,25.4499762 28.6560428,25.2122786 C29.4208939,24.9744833 30.2334994,24.8459665 31.0813591,24.8459665 L44.1277737,24.8459665 C45.0283683,24.8459665 45.7585511,24.1157837 45.7585511,23.2151891 C45.7585511,22.3145945 45.0283683,21.5844117 44.1277737,21.5844117 L31.0813591,21.5844117 C29.5096643,21.5844117 28.0039858,21.9038483 26.6373711,22.4820765 C24.5866678,23.3498583 22.8469049,24.7950871 21.6163267,26.616296 C20.3856508,28.4362354 19.665136,30.6413347 19.6658196,33.0000488 C19.6656243,34.5717436 19.9852563,36.0774222 20.5635822,37.4440369 C21.4312663,39.4947402 22.8765927,41.2345031 24.697704,42.4650813 C26.5176434,43.6957572 28.7227427,44.4155883 31.0814568,44.4155883 L44.1278714,44.4155883 C45.028466,44.4155883 45.7586488,43.6854055 45.7586488,42.7848109 C45.7586488,41.8842163 45.0285636,41.1538382 44.127969,41.1538382 Z" id="set\_of" fill="\#000000" fill-rule="nonzero"&gt;&lt;/path&gt; &lt;/g&gt; &lt;/g&gt;&lt;/svg&gt;&lt;/div&gt;  &lt;code style="font-size:11px;"&gt;&nbsp;col\_vals\_in\_set()&lt;/code&gt; &lt;/div&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: \#E5E5E5; height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;div aria-label="ecl" data-balloon-pos="left"&gt;  &lt;p style="margin-top:0;margin-bottom:0;font-size:11px;white-space:nowrap;text-overflow:ellipsis;overflow:hidden;line-height:2em;"&gt;  &lt;code&gt;&lt;span style="color:purple;"&gt;&marker;&lt;/span&gt;ecl&lt;/code&gt;  &lt;/p&gt; &lt;/div&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: \#E5E5E5; height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;div aria-label="amb, blu, brn, gry, grn, hzl, oth" data-balloon-pos="left"&gt;&lt;p style="margin-top: 0px; margin-bottom: 0px; font-family: monospace; white-space: nowrap; text-overflow: ellipsis; overflow: hidden;"&gt;&lt;code&gt;amb, blu, brn, gry, grn, hzl, oth&lt;/code&gt;&lt;/p&gt;&lt;/div&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: \#D3D3D3; background-color: \#FCFCFC; height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;p&gt;&lt;span style="background:transparent;padding:0;color:\#333333;vertical-align:middle;font-size:18px;border:none;border-radius:4px;" aria-label="No modifications of the table." data-balloon-pos="left"&gt;→&lt;/span&gt;&lt;/p&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: \#D3D3D3; background-color: \#FCFCFC; height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;p&gt;&lt;span style="background:transparent;padding:5px;color:\#4CA64C;vertical-align:middle;font-size:15px;border:none;" aria-label="No evaluation issues." data-balloon-pos="left"&gt;✓&lt;/span&gt;&lt;/p&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_right" style="height: 40px"&gt;&lt;code&gt;276&lt;/code&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: \#E5E5E5; height: 40px"&gt;&lt;code&gt;204&lt;/code&gt;&lt;br&gt;&lt;code&gt;0.74&lt;/code&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: \#E5E5E5; height: 40px"&gt;&lt;code&gt;72&lt;/code&gt;&lt;br&gt;&lt;code&gt;0.26&lt;/code&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: \#D3D3D3; background-color: \#FCFCFC; height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;p&gt;—&lt;/p&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_center" style="background-color: \#FCFCFC; height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;p&gt;—&lt;/p&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: \#D3D3D3; background-color: \#FCFCFC; height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;p&gt;—&lt;/p&gt; &lt;/div&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_center" style="height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;a href="data:text/csv;base64,Iml5ciIsImhndCIsImJ5ciIsImNpZCIsInBpZCIsImV5ciIsImhjbCIsImVjbCIsImhndF9udW0iLCJoZ3RfdW5pdCIsImhndF9jbSINCiIyMDE1IiwiNTljbSIsIjIwMjkiLCIyMTkiLCI5MzgxNjg4NzUzIiwiMTk5MiIsIiNiNjY1MmEiLCIjN2EwZmE2Iiw1OSwiY20iLDU5DQoiMjAyNSIsIjY0Y20iLCIyMDI5IiwiMjgxIiwiMDY3Mjg1OTg1IiwiMTk0NCIsIiNjZWIzYTEiLCIjMDcyMTlhIiw2NCwiY20iLDY0DQoiMTkzMCIsIjE4MWNtIiwiMTk1MCIsTkEsIjYwNDk4MzQ2NiIsIjIwMzkiLCIjYjY2NTJhIiwiIzkwNjU0OCIsMTgxLCJjbSIsMTgxDQoiMjAyNSIsIjE0MSIsIjIwMDYiLCIzMjciLCIjMWM0MmNjIiwiMTk1NiIsInoiLCIjZjJhZmZjIiwxNDEsTkEsTkENCiIxOTI0IiwiNzRjbSIsIjIwMjQiLCIxNTMiLCIzNjE5NjQwMSIsIjE5MjEiLCJhNGU0YzAiLCIjM2FjZjU3Iiw3NCwiY20iLDc0DQoiMjAyMCIsIjE1MSIsIjIwMTIiLCIzMTQiLCI5ODQzNjQ4NjIiLCIyMDIzIiwieiIsImRuZSIsMTUxLE5BLE5BDQoiMTkzMyIsIjkwIiwiMjAyNSIsTkEsIjgxOTQzNDc1NDQiLCIyMDQwIixOQSwiZG5lIiw5MCxOQSxOQQ0KIjE5MjAiLCI2MGNtIiwiMjAyOSIsIjEwNyIsIiM1NWNlNmIiLCIyMDQwIiwiZDMwZjZiIiwiZG5lIiw2MCwiY20iLDYwDQoiMjAxOCIsIjY1Y20iLCIxOTM3IixOQSwiIzRiZmYzZSIsIjIwMjciLCI0M2ZhZmIiLCJncnQiLDY1LCJjbSIsNjUNCiIyMDE0IiwiMTY2Y20iLCIxOTI0IiwiMTYzIixOQSwiMjAyNCIsIiMxODE3MWQiLE5BLDE2NiwiY20iLDE2Ng0KIjIwMTYiLCIxNzJjbSIsTkEsTkEsIiM5OGNhZWMiLCIyMDM2IiwieiIsImRuZSIsMTcyLCJjbSIsMTcyDQoiMTkzOCIsIjE3MmluIiwiMjAxOCIsIjMzOSIsIiM2YzEyMTYiLCIyMDM5IiwieiIsIiM1MTA2NzIiLDE3MiwiaW4iLDQzNi44OA0KIjE5NTEiLCIxOTNpbiIsIjIwMTkiLE5BLCIjY2JjMDhjIiwiMjAwMiIsInoiLCIjM2U5ZjJmIiwxOTMsImluIiw0OTAuMjINCk5BLCIxNzVpbiIsIjIwMTIiLCIyNDAiLCI1NzE0NzcxNzYiLCIxOTI5IiwiZjRlZjMyIiwidXRjIiwxNzUsImluIiw0NDQuNQ0KIjIwMjYiLCI3MCIsIjIwMjIiLE5BLCI0MDQ2MjIwODMiLCIxOTc5IiwiYzFiYTdmIiwibHpyIiw3MCxOQSxOQQ0KIjIwMTgiLCIxNTZjbSIsIjIwMTciLCIxNzkiLCIyOTgzNjEyNCIsIjIwMjMiLCI1NmRlODMiLCJ6enoiLDE1NiwiY20iLDE1Ng0KIjIwMTQiLCIxOTBjbSIsIjE5OTgiLE5BLCI1NjU1MjQ1NzQiLCIyMDIwIiwiIzg2Njg1NyIsTkEsMTkwLCJjbSIsMTkwDQpOQSwiODIiLCIyMDE3IiwiMTUzIiwiMTcyNjYxNjE3IiwiMjAzNiIsInoiLCIjYWU2MDdkIiw4MixOQSxOQQ0KIjE5NDUiLCIxNjVpbiIsIjIwMTAiLCIyNzMiLCI0MzcwNzUwNCIsIjIwMjYiLCJ6IiwiZ3J0IiwxNjUsImluIiw0MTkuMQ0KIjIwMTYiLCI2N2NtIiwiMjAxMCIsIjE0NCIsIjM0OTQ5MjI0MyIsIjIwMjEiLCJ6IiwiIzI0Y2VlZSIsNjcsImNtIiw2Nw0KIjIwMjQiLCIxODAiLCIxOTI3IiwiODciLCI1OTcwOTg5NDAiLCIyMDI3IiwiIzYyM2EyZiIsIiM3ZWE3NzciLDE4MCxOQSxOQQ0KIjIwMjUiLCIxNTIiLCIyMDEyIixOQSwiMzgyMTc0NzQ0IiwiMjAyNCIsIiM4ODg3ODUiLCIjOTVkOGE5IiwxNTIsTkEsTkENCiIxOTU1IiwiMTYzaW4iLCIyMDE4IiwiMjk4IiwiODcxNTUyNjYiLCIyMDIxIiwiI2MwOTQ2ZiIsIiMyMTY5MjAiLDE2MywiaW4iLDQxNC4wMg0KTkEsIjE4MmNtIiwiMTk5MyIsIjExNyIsIjA3MzAzNTk5OSIsIjIwMzAiLCIjMzQxZTEzIixOQSwxODIsImNtIiwxODINCiIyMDE3IiwiODMiLCIxOTQwIixOQSxOQSwiMjAzMiIsIjE3NDc3NCIsInhyeSIsODMsTkEsTkENCiIxOTQwIiwiMTczY20iLCIxOTQ3IixOQSxOQSwiMTk0NyIsIjdlNTE1YyIsImdtdCIsMTczLCJjbSIsMTczDQoiMjAxMiIsIjYyY20iLCIxOTg1IiwiNTkiLCIjYWYxMDZhIiwiMTk1NyIsIiM2MjNhMmYiLCJncnQiLDYyLCJjbSIsNjINCk5BLCI2M2NtIiwiMjAwNCIsIjE4OCIsIjU2MjEyMDAxMzQiLCIxOTI0IiwiNmVmOWJhIiwiI2VmNjhmNSIsNjMsImNtIiw2Mw0KIjIwMTkiLCIxNzFjbSIsIjE5NzYiLE5BLCIwNDE5MjYzNTQiLCIyMDIwIiwiI2E5Nzg0MiIsTkEsMTcxLCJjbSIsMTcxDQoiMjAyMSIsIjE2N2luIiwiMjAyNSIsIjIzNCIsIjE4MWNtIiwiMTk0MSIsIjk4NjE5YSIsIiNmNWU2NTEiLDE2NywiaW4iLDQyNC4xOA0KIjE5NDkiLCI3M2NtIiwiMTk4NSIsIjIwNyIsIiNlZTlmOTUiLCIyMDI4IiwieiIsInV0YyIsNzMsImNtIiw3Mw0KIjIwMjgiLCIxMTEiLCIyMDMwIiwiMTgwIiwiMTgzMzkxODYxIiwiMTk1NCIsIjFmYjMwZiIsIiMwZDAxNjAiLDExMSxOQSxOQQ0KTkEsIjE5MWNtIiwiMjAyMyIsTkEsIjcyNzAyNDY3NiIsIjIwMjUiLCIjYjY2NTJhIiwiIzBiM2IyZCIsMTkxLCJjbSIsMTkxDQpOQSxOQSwiMjAyMyIsIjI3MSIsIiM2NmVjODIiLCIyMDI5IiwiMTBkOWQ4IiwiIzM1M2UwZiIsTkEsTkEsTkENCiIxOTgxIiwiNTljbSIsIjIwMDkiLE5BLCIxNjJjbSIsIjIwMjUiLCIxMTY3NDIiLCJnbXQiLDU5LCJjbSIsNTkNCiIyMDIzIiwiNjdjbSIsIjE5NjMiLE5BLCI2Mjg1OTMzMiIsIjIwMjgiLCIzZDFmMzQiLCJkbmUiLDY3LCJjbSIsNjcNCiIyMDEzIiwiMTU3aW4iLCIyMDIzIiwiMTg2IiwiMjcxNDUwNzU0IiwiMjAxNiIsImUyMDg4MiIsInV0YyIsMTU3LCJpbiIsMzk4Ljc4DQoiMjAxOCIsIjE5M2luIiwiMTk0MyIsTkEsIjcwMjIwMDAyNiIsIjE5NjgiLCIjODg4Nzg1IiwiZ210IiwxOTMsImluIiw0OTAuMjINCiIyMDE4IiwiMTg2Y20iLE5BLE5BLCIxNzg1MjUxMzIiLCIyMDIzIiwiIzg4ODc4NSIsTkEsMTg2LCJjbSIsMTg2DQoiMjAwMyIsIjE1MCIsIjIwMDEiLCIyOTIiLCI5MzQ3MzE5MiIsIjE5MjIiLCJ6IiwiI2Q1NmJiZCIsMTUwLE5BLE5BDQoiMTkyOSIsIjE5MyIsTkEsIjMzMCIsIjMzNzY1NDI2IiwiMjAzOCIsInoiLCIjMThlODgzIiwxOTMsTkEsTkENCiIyMDI1IiwiMTYyY20iLCIxOTg5IiwiMzE5IiwiNjczMTEyMjIiLCIxOTM5IiwieiIsInV0YyIsMTYyLCJjbSIsMTYyDQoiMjAxOSIsIjE2OWNtIiwiMjAxMiIsIjI5OCIsIjA2NjAzMTY1NTgiLCIxOTIwIiwiIzg4ODc4NSIsInp6eiIsMTY5LCJjbSIsMTY5DQoiMTk5NyIsIjYxY20iLCIyMDIzIixOQSwiMTYyY20iLCIxOTg3IiwieiIsIiM5ZjQ1OGMiLDYxLCJjbSIsNjENCiIyMDE1IiwiMTUwY20iLCIyMDIyIixOQSwiMTY3Y20iLCIyMDMyIiwiY2FhMTQ1IiwiIzA2NjUwYSIsMTUwLCJjbSIsMTUwDQoiMjAyNCIsIjE4MWluIiwiMjAyOSIsIjk3IiwiI2MzODYyMCIsIjE5NzYiLCJ6Iiwienp6IiwxODEsImluIiw0NTkuNzQNCiIyMDE4IiwiMTYzY20iLCIxOTk5IiwiMjA5IiwiNDAxNjA2NTcxIiwiMjAyMyIsIjZmMjlhNiIsImx6ciIsMTYzLCJjbSIsMTYzDQoiMjAyNSIsIjU5aW4iLCIxOTIwIiwiMjAyIiwiMTgyY20iLCIyMDM1IiwieiIsIiM3OTlmMjkiLDU5LCJpbiIsMTQ5Ljg2DQpOQSwiNzNjbSIsIjE5MjciLE5BLCI3MTk3Mzg0NjgiLCIyMDM5IixOQSwiI2E4MmU5MCIsNzMsImNtIiw3Mw0KIjIwMDEiLCIxNzFjbSIsIjIwMTMiLE5BLCI4OTAwOTY4MzI1IiwiMjAyMiIsIjY5OTExNiIsTkEsMTcxLCJjbSIsMTcxDQoiMjAwOSIsIjE3MmluIiwiMjAwNSIsIjMzNCIsIjE4OWNtIiwiMjAzMiIsInoiLCJ4cnkiLDE3MiwiaW4iLDQzNi44OA0KIjIwMTAiLCIxNjciLCIyMDAzIiwiMTY5IixOQSwiMjAzNyIsIjQ4NjgwMCIsIiMyOWJkZDYiLDE2NyxOQSxOQQ0KIjIwMTgiLCIxNzBjbSIsIjE5NDAiLCIyNzMiLCI1ODgxNDI3NzEiLCIyMDIyIiwiIzczMzgyMCIsIiNhNjA4ZmUiLDE3MCwiY20iLDE3MA0KIjIwMjkiLCIxODBjbSIsIjIwMjciLCIyMDUiLCI0NDM4MDkzMzciLCIxOTgwIiwiIzM0MWUxMyIsImdydCIsMTgwLCJjbSIsMTgwDQoiMTk4MiIsIjE0NCIsIjIwMTIiLE5BLCIjM2I0M2MxIiwiMjAzMiIsIjJmMjZhYiIsIiNmODlkZjAiLDE0NCxOQSxOQQ0KIjIwMjAiLCIxODVpbiIsIjIwMjgiLE5BLCI3NzM3Mzk3NDQiLCIxOTg2IiwieiIsImRuZSIsMTg1LCJpbiIsNDY5LjkNCiIyMDE2IiwiMTg2Y20iLCIxOTQ3IiwiNjQiLE5BLCIyMDIxIiwiI2E5Nzg0MiIsTkEsMTg2LCJjbSIsMTg2DQoiMjAyNCIsIjE2N2luIiwiMjAwNiIsIjM0NiIsIjE5NTMxMzQxIiwiMjAzNSIsInoiLCIjZDMyMzIwIiwxNjcsImluIiw0MjQuMTgNCiIyMDE1IiwiMTUyY20iLCIyMDE5IixOQSwiMTgyY20iLCIyMDI4IiwiNDNkNTZkIiwienp6IiwxNTIsImNtIiwxNTINCiIxOTU4IiwiMTc2Y20iLE5BLE5BLCIjNGNiNDgwIiwiMjAyNiIsInoiLCJkbmUiLDE3NiwiY20iLDE3Ng0KIjIwMTgiLCIxNjhjbSIsIjE5MzIiLCIxNjUiLCI3NDU4NjczMzUiLCIyMDMwIiwiI2MwOTQ2ZiIsImdydCIsMTY4LCJjbSIsMTY4DQoiMjAxNyIsIjE2NmNtIiwiMjAxMiIsIjE3MiIsIiM0MjRhZTQiLCIyMDIyIiwiYjEzMTliIiwiIzY2MzVkOCIsMTY2LCJjbSIsMTY2DQoiMjAyMCIsIjcwY20iLCIyMDA0IiwiMTY2IixOQSwiMjA0MCIsIiM3MzM4MjAiLCJsenIiLDcwLCJjbSIsNzANCk5BLCIxMjgiLCIxOTk3IiwiMjk5IiwiOTg0Njc1MTk4IiwiMjAzNyIsIiNiNjY1MmEiLCJnbXQiLDEyOCxOQSxOQQ0KIjIwMTYiLCIxNTljbSIsIjE5OTEiLE5BLCI5NDk2MTcxMzg0IiwiMjAzMCIsIiNjZWIzYTEiLCJ4cnkiLDE1OSwiY20iLDE1OQ0KIjE5NzEiLCIxNTdpbiIsIjE5NzAiLE5BLCI4MTI2MTc4MDkiLCIyMDIwIiwiIzdkM2IwYyIsImdtdCIsMTU3LCJpbiIsMzk4Ljc4DQoiMjAxMSIsIjE3NmNtIiwiMjAxMiIsIjY0IiwiMjEzMjEzMzU5IiwiMTk3MSIsImJlN2IxMyIsImdtdCIsMTc2LCJjbSIsMTc2DQpOQSwiNjZjbSIsIjE5MjgiLCI4NyIsIjI3MTA3OTQ2IiwiMjA0MCIsTkEsInV0YyIsNjYsImNtIiw2Ng0KIjIwMjEiLCI4MiIsIjIwMDciLCIxOTEiLCIjMWNmNjlmIiwiMjAzOSIsInoiLCJkbmUiLDgyLE5BLE5BDQoiMTk1MCIsIjY2Y20iLE5BLCIxMTEiLCIxODNjbSIsIjE5NDciLE5BLCIjMDE2ZjZhIiw2NiwiY20iLDY2DQoiMjAyMyIsTkEsIjE5NjYiLE5BLCIxNjRjbSIsIjIwMjEiLCJ6IiwidXRjIixOQSxOQSxOQQ0KIjE5MjIiLCIxMDEiLCIyMDE1IiwiMTM2IiwiMTUxY20iLCIyMDQwIiwiMjQ1Y2IzIiwibHpyIiwxMDEsTkEsTkENCg==" download="extract\_0005.csv"&gt;  &lt;button aria-label="There are 72 &\#39;fail&\#39; rows available as a CSV file." data-balloon-pos="left" style="background-color:\#67C2DC;color:\#FFFFFF;border:none;padding:5px;font-weight:bold;cursor:pointer;border-radius:4px;"&gt;CSV&lt;/button&gt; &lt;/a&gt; &lt;/div&gt;&lt;/td&gt;  &lt;/tr&gt;  &lt;tr&gt;  &lt;td class="gt\_row gt\_left" style="background-color: rgba(76,166,76,0.5); height: 40px"&gt;&lt;/td&gt;  &lt;td class="gt\_row gt\_right" style="color: \#666666; font-size: 13px; font-weight: bold; height: 40px"&gt;6&lt;/td&gt;  &lt;td class="gt\_row gt\_left" style="height: 40px"&gt;&lt;div class='gt\_from\_md'&gt;&lt;div aria-label="Expect that values in \`pid\` should match the regular expression: \`^\[0-9\]{9}$`. " data-balloon-pos="right" style="width:fit-content;">   <div style="margin:0;padding:0;display:inline-block;height:30px;vertical-align:middle;"><svg width="30px" height="30px" viewBox="0 0 67 67" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">    <defs>        <path d="M10.712234,0 L56.712234,0 C62.2350815,-1.01453063e-15 66.712234,4.4771525 66.712234,10 L66.712234,66 L10.712234,66 C5.18938647,66 0.712233968,61.5228475 0.712233968,56 L0.712233968,10 C0.712233968,4.4771525 5.18938647,1.01453063e-15 10.712234,0 Z" id="path-1"></path>    </defs>    <g id="pointblank" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">        <g id="col_vals_regex" transform="translate(-0.500000, 0.651308)">            <g id="rectangle">                <use fill="#FFFFFF" fill-rule="evenodd" xlink:href="#path-1"></use>                <path stroke="#000000" stroke-width="2" d="M65.712234,65 L65.712234,10 C65.712234,5.02943725 61.6827967,1 56.712234,1 L10.712234,1 C5.74167122,1 1.71223397,5.02943725 1.71223397,10 L1.71223397,56 C1.71223397,60.9705627 5.74167122,65 10.712234,65 L65.712234,65 Z"></path>            </g>            <g id="regex_symbols" transform="translate(18.000000, 12.000000)" fill="#000000" fill-rule="nonzero">                <path d="M4.17434508,33.013582 C1.94895328,33.013582 0.138006923,34.8245284 0.138006923,37.0499202 C0.138006923,39.275312 1.94895328,41.0862583 4.17434508,41.0862583 C6.39973688,41.0862583 8.21068324,39.275312 8.21068324,37.0499202 C8.21068324,34.8245284 6.39973688,33.013582 4.17434508,33.013582 Z" id="full_stop"></path>                <path d="M23.9479718,23.3175402 L21.5628264,23.3175402 C21.2344032,23.3175402 20.9665401,23.0520067 20.9665401,22.7212538 L20.9665401,15.1022979 L14.3445004,18.8873192 C14.0626621,19.050366 13.7016292,18.952538 13.5362533,18.6706991 L12.3436806,16.6442575 C12.262157,16.506832 12.2388642,16.3437852 12.2807909,16.1900549 C12.3203879,16.0363251 12.4205455,15.9058874 12.557971,15.8266929 L19.1800101,11.9880994 L12.557971,8.15183511 C12.4205455,8.07264112 12.3203879,7.93987439 12.2807909,7.78614401 C12.2388642,7.63241423 12.262157,7.46936689 12.3413509,7.33194137 L13.5339237,5.30549975 C13.6993001,5.02366143 14.0626621,4.92816199 14.3445004,5.09120934 L20.9665401,8.87390091 L20.9665401,1.25494501 C20.9665401,0.926521818 21.2344032,0.658658658 21.5628264,0.658658658 L23.9479718,0.658658658 C24.2787247,0.658658658 24.5442582,0.926521818 24.5442582,1.25494501 L24.5442582,8.87390091 L31.1662979,5.09120934 C31.4481362,4.92816199 31.8091691,5.02366143 31.9745455,5.30549975 L33.1671182,7.33194137 C33.2486413,7.46936689 33.2719341,7.63241423 33.2300074,7.78614401 C33.1904104,7.93987439 33.0902528,8.07264112 32.9528278,8.15183511 L26.3307882,11.9880994 L32.9528278,15.8243638 C33.0879237,15.9058874 33.1880813,16.0363251 33.2300074,16.1900549 C33.269605,16.3437852 33.2486413,16.506832 33.1671182,16.6442575 L31.9745455,18.6706991 C31.8091691,18.952538 31.4481362,19.050366 31.1662979,18.8849895 L24.5442582,15.1022979 L24.5442582,22.7212538 C24.5442582,23.0520067 24.2787247,23.3175402 23.9479718,23.3175402 Z" id="asterisk"></path>            </g>        </g>    </g></svg></div>   <code style="font-size:11px;">&nbsp;col_vals_regex()</code> </div> </div></td>       <td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px"><div class='gt_from_md'><div aria-label="pid" data-balloon-pos="left">   <p style="margin-top:0;margin-bottom:0;font-size:11px;white-space:nowrap;text-overflow:ellipsis;overflow:hidden;line-height:2em;">     <code><span style="color:purple;">&marker;</span>pid</code>   </p> </div> </div></td>       <td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px"><div class='gt_from_md'><div aria-label="^[0-9]{9}$" data-balloon-pos="left"><p style="margin-top: 0px; margin-bottom: 0px; font-family: monospace; white-space: nowrap; text-overflow: ellipsis; overflow: hidden;"><code>^[0-9]{9}$</code></p></div> </div></td>       <td class="gt_row gt_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; background-color: #FCFCFC; height:  40px"><div class='gt_from_md'><p><span style="background:transparent;padding:0;color:#333333;vertical-align:middle;font-size:18px;border:none;border-radius:4px;" aria-label="No modifications of the table." data-balloon-pos="left">→</span></p> </div></td>       <td class="gt_row gt_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; background-color: #FCFCFC; height:  40px"><div class='gt_from_md'><p><span style="background:transparent;padding:5px;color:#4CA64C;vertical-align:middle;font-size:15px;border:none;" aria-label="No evaluation issues." data-balloon-pos="left">✓</span></p> </div></td>       <td class="gt_row gt_right" style="height:  40px"><code>276</code></td>       <td class="gt_row gt_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px"><code>198</code><br><code>0.72</code></td>       <td class="gt_row gt_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px"><code>78</code><br><code>0.28</code></td>       <td class="gt_row gt_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; background-color: #FCFCFC; height:  40px"><div class='gt_from_md'><p>—</p> </div></td>       <td class="gt_row gt_center" style="background-color: #FCFCFC; height:  40px"><div class='gt_from_md'><p>—</p> </div></td>       <td class="gt_row gt_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; background-color: #FCFCFC; height:  40px"><div class='gt_from_md'><p>—</p> </div></td>       <td class="gt_row gt_center" style="height:  40px"><div class='gt_from_md'><a href="data:text/csv;base64,Iml5ciIsImhndCIsImJ5ciIsImNpZCIsInBpZCIsImV5ciIsImhjbCIsImVjbCIsImhndF9udW0iLCJoZ3RfdW5pdCIsImhndF9jbSINCiIyMDE1IiwiNTljbSIsIjIwMjkiLCIyMTkiLCI5MzgxNjg4NzUzIiwiMTk5MiIsIiNiNjY1MmEiLCIjN2EwZmE2Iiw1OSwiY20iLDU5DQoiMjAyNSIsIjE0MSIsIjIwMDYiLCIzMjciLCIjMWM0MmNjIiwiMTk1NiIsInoiLCIjZjJhZmZjIiwxNDEsTkEsTkENCiIxOTI0IiwiNzRjbSIsIjIwMjQiLCIxNTMiLCIzNjE5NjQwMSIsIjE5MjEiLCJhNGU0YzAiLCIjM2FjZjU3Iiw3NCwiY20iLDc0DQoiMTkzMyIsIjkwIiwiMjAyNSIsTkEsIjgxOTQzNDc1NDQiLCIyMDQwIixOQSwiZG5lIiw5MCxOQSxOQQ0KIjE5MjAiLCI2MGNtIiwiMjAyOSIsIjEwNyIsIiM1NWNlNmIiLCIyMDQwIiwiZDMwZjZiIiwiZG5lIiw2MCwiY20iLDYwDQoiMjAxOCIsIjY1Y20iLCIxOTM3IixOQSwiIzRiZmYzZSIsIjIwMjciLCI0M2ZhZmIiLCJncnQiLDY1LCJjbSIsNjUNCiIyMDE0IiwiMTY2Y20iLCIxOTI0IiwiMTYzIixOQSwiMjAyNCIsIiMxODE3MWQiLE5BLDE2NiwiY20iLDE2Ng0KIjIwMTYiLCIxNzJjbSIsTkEsTkEsIiM5OGNhZWMiLCIyMDM2IiwieiIsImRuZSIsMTcyLCJjbSIsMTcyDQoiMTkzOCIsIjE3MmluIiwiMjAxOCIsIjMzOSIsIiM2YzEyMTYiLCIyMDM5IiwieiIsIiM1MTA2NzIiLDE3MiwiaW4iLDQzNi44OA0KIjE5OTciLCI5NiIsIjIwMDkiLE5BLCIwNjM2OTE3MjIyIiwiMjAyNiIsInoiLCJoemwiLDk2LE5BLE5BDQoiMjAxNyIsIjE3NWNtIiwiMTk2NCIsIjI2NiIsTkEsTkEsIiNhOTc4NDIiLCJicm4iLDE3NSwiY20iLDE3NQ0KIjE5NTEiLCIxOTNpbiIsIjIwMTkiLE5BLCIjY2JjMDhjIiwiMjAwMiIsInoiLCIjM2U5ZjJmIiwxOTMsImluIiw0OTAuMjINCiIyMDE4IiwiMTU2Y20iLCIyMDE3IiwiMTc5IiwiMjk4MzYxMjQiLCIyMDIzIiwiNTZkZTgzIiwienp6IiwxNTYsImNtIiwxNTYNCiIyMDE2IiwiMTczY20iLCIxOTczIixOQSxOQSwiMjAyOCIsIiM4ODg3ODUiLCJibHUiLDE3MywiY20iLDE3Mw0KIjE5NDUiLCIxNjVpbiIsIjIwMTAiLCIyNzMiLCI0MzcwNzUwNCIsIjIwMjYiLCJ6IiwiZ3J0IiwxNjUsImluIiw0MTkuMQ0KTkEsIjE2M2luIiwiMjAyOSIsTkEsTkEsIjE5NjIiLCIzYTBjMzAiLCJncnkiLDE2MywiaW4iLDQxNC4wMg0KTkEsIjE4M2NtIiwiMTkzNCIsTkEsTkEsIjIwMjMiLCIjNmI1NDQyIiwiZ3JuIiwxODMsImNtIiwxODMNCiIxOTk3IiwiNjljbSIsIjIwMTIiLCI1NCIsIjE4NGNtIiwiMjAzNyIsImY4ZWQ0NSIsImdyeSIsNjksImNtIiw2OQ0KIjE5NTUiLCIxNjNpbiIsIjIwMTgiLCIyOTgiLCI4NzE1NTI2NiIsIjIwMjEiLCIjYzA5NDZmIiwiIzIxNjkyMCIsMTYzLCJpbiIsNDE0LjAyDQoiMjAxNyIsIjgzIiwiMTk0MCIsTkEsTkEsIjIwMzIiLCIxNzQ3NzQiLCJ4cnkiLDgzLE5BLE5BDQoiMTk0MCIsIjE3M2NtIiwiMTk0NyIsTkEsTkEsIjE5NDciLCI3ZTUxNWMiLCJnbXQiLDE3MywiY20iLDE3Mw0KIjIwMTIiLCIxODVjbSIsTkEsTkEsTkEsIjIwMzAiLCIjYjY2NTJhIiwiZ3JuIiwxODUsImNtIiwxODUNCiIyMDEyIiwiNjJjbSIsIjE5ODUiLCI1OSIsIiNhZjEwNmEiLCIxOTU3IiwiIzYyM2EyZiIsImdydCIsNjIsImNtIiw2Mg0KTkEsIjYzY20iLCIyMDA0IiwiMTg4IiwiNTYyMTIwMDEzNCIsIjE5MjQiLCI2ZWY5YmEiLCIjZWY2OGY1Iiw2MywiY20iLDYzDQoiMjAyMSIsIjE2N2luIiwiMjAyNSIsIjIzNCIsIjE4MWNtIiwiMTk0MSIsIjk4NjE5YSIsIiNmNWU2NTEiLDE2NywiaW4iLDQyNC4xOA0KIjE5NDkiLCI3M2NtIiwiMTk4NSIsIjIwNyIsIiNlZTlmOTUiLCIyMDI4IiwieiIsInV0YyIsNzMsImNtIiw3Mw0KIjE5MzMiLCI2OWNtIiwiMTkzNCIsTkEsIjE3OWNtIiwiMjAzMCIsImI4ZTE0MiIsImdybiIsNjksImNtIiw2OQ0KTkEsTkEsIjIwMjMiLCIyNzEiLCIjNjZlYzgyIiwiMjAyOSIsIjEwZDlkOCIsIiMzNTNlMGYiLE5BLE5BLE5BDQoiMjAxNCIsIjE2NmNtIiwiMTk1MyIsTkEsTkEsIjIwMjIiLCIjODY2ODU3IiwiYmx1IiwxNjYsImNtIiwxNjYNCiIxOTgxIiwiNTljbSIsIjIwMDkiLE5BLCIxNjJjbSIsIjIwMjUiLCIxMTY3NDIiLCJnbXQiLDU5LCJjbSIsNTkNCiIyMDIzIiwiNjdjbSIsIjE5NjMiLE5BLCI2Mjg1OTMzMiIsIjIwMjgiLCIzZDFmMzQiLCJkbmUiLDY3LCJjbSIsNjcNCiIyMDEzIiwiMTg1Y20iLCIxOTQwIixOQSxOQSwiMjAyOCIsIiM3YzczYTMiLCJoemwiLDE4NSwiY20iLDE4NQ0KIjIwMDMiLCIxNTAiLCIyMDAxIiwiMjkyIiwiOTM0NzMxOTIiLCIxOTIyIiwieiIsIiNkNTZiYmQiLDE1MCxOQSxOQQ0KIjE5MjkiLCIxOTMiLE5BLCIzMzAiLCIzMzc2NTQyNiIsIjIwMzgiLCJ6IiwiIzE4ZTg4MyIsMTkzLE5BLE5BDQoiMjAyNSIsIjE2MmNtIiwiMTk4OSIsIjMxOSIsIjY3MzExMjIyIiwiMTkzOSIsInoiLCJ1dGMiLDE2MiwiY20iLDE2Mg0KIjIwMTkiLCIxNjljbSIsIjIwMTIiLCIyOTgiLCIwNjYwMzE2NTU4IiwiMTkyMCIsIiM4ODg3ODUiLCJ6enoiLDE2OSwiY20iLDE2OQ0KIjE5NjUiLCI2MCIsIjE5MzEiLE5BLCIjYWE1ZmQwIiwiMjAxNyIsIjU3OTI2NiIsImdyeSIsNjAsTkEsTkENCiIxOTk3IiwiNjFjbSIsIjIwMjMiLE5BLCIxNjJjbSIsIjE5ODciLCJ6IiwiIzlmNDU4YyIsNjEsImNtIiw2MQ0KIjIwMTUiLCIxNTBjbSIsIjIwMjIiLE5BLCIxNjdjbSIsIjIwMzIiLCJjYWExNDUiLCIjMDY2NTBhIiwxNTAsImNtIiwxNTANCiIyMDI0IiwiMTgxaW4iLCIyMDI5IiwiOTciLCIjYzM4NjIwIiwiMTk3NiIsInoiLCJ6enoiLDE4MSwiaW4iLDQ1OS43NA0KIjIwMTQiLCIxNzJpbiIsIjE5NTAiLE5BLCIxODdjbSIsIjIwMjgiLCJ6IiwiYnJuIiwxNzIsImluIiw0MzYuODgNCiIyMDExIiwiMTYyY20iLCIxOTkzIixOQSwiNjUzMzk1MTE3NyIsIjIwMjUiLCIjYzA5NDZmIiwiaHpsIiwxNjIsImNtIiwxNjINCiIyMDI1IiwiNTlpbiIsIjE5MjAiLCIyMDIiLCIxODJjbSIsIjIwMzUiLCJ6IiwiIzc5OWYyOSIsNTksImluIiwxNDkuODYNCiIyMDIzIiwiMTYyY20iLCIyMDI1IiwiMzQzIiwiNTA0MjQwMzUiLCIyMDI0IiwiNjA1MjIzIiwib3RoIiwxNjIsImNtIiwxNjINCiIyMDAxIiwiMTcxY20iLCIyMDEzIixOQSwiODkwMDk2ODMyNSIsIjIwMjIiLCI2OTkxMTYiLE5BLDE3MSwiY20iLDE3MQ0KIjIwMDkiLCIxNzJpbiIsIjIwMDUiLCIzMzQiLCIxODljbSIsIjIwMzIiLCJ6IiwieHJ5IiwxNzIsImluIiw0MzYuODgNCiIyMDIwIiwiMTU5Y20iLE5BLE5BLCIxNjZjbSIsIjIwMjYiLCJ6Iiwib3RoIiwxNTksImNtIiwxNTkNCiIyMDE4IiwiMTg4Y20iLCIxOTk5IixOQSwiI2VjM2Q1MyIsIjIwMjQiLCIjY2ViM2ExIiwib3RoIiwxODgsImNtIiwxODgNCiIyMDEwIiwiMTY3IiwiMjAwMyIsIjE2OSIsTkEsIjIwMzciLCI0ODY4MDAiLCIjMjliZGQ2IiwxNjcsTkEsTkENCiIyMDEzIiwiMTg5Y20iLCIxOTkwIixOQSwiMTc2Y20iLCIyMDI0IiwiI2NmYTA3ZCIsImdyeSIsMTg5LCJjbSIsMTg5DQoiMTk4MiIsIjE0NCIsIjIwMTIiLE5BLCIjM2I0M2MxIiwiMjAzMiIsIjJmMjZhYiIsIiNmODlkZjAiLDE0NCxOQSxOQQ0KIjIwMTYiLCIxODZjbSIsIjE5NDciLCI2NCIsTkEsIjIwMjEiLCIjYTk3ODQyIixOQSwxODYsImNtIiwxODYNCiIyMDI0IiwiMTY3aW4iLCIyMDA2IiwiMzQ2IiwiMTk1MzEzNDEiLCIyMDM1IiwieiIsIiNkMzIzMjAiLDE2NywiaW4iLDQyNC4xOA0KIjIwMTUiLCIxNTJjbSIsIjIwMTkiLE5BLCIxODJjbSIsIjIwMjgiLCI0M2Q1NmQiLCJ6enoiLDE1MiwiY20iLDE1Mg0KIjIwMTMiLCIxNzRjbSIsIjE5NzkiLCIyMjgiLCI4MjQyMjQ1MCIsIjIwMjIiLCIjMTgxNzFkIiwiYW1iIiwxNzQsImNtIiwxNzQNCiIxOTU4IiwiMTc2Y20iLE5BLE5BLCIjNGNiNDgwIiwiMjAyNiIsInoiLCJkbmUiLDE3NiwiY20iLDE3Ng0KIjIwMTQiLCIxNjJjbSIsIjE5NTEiLE5BLE5BLCIyMDI5IiwiI2I2NjUyYSIsImJsdSIsMTYyLCJjbSIsMTYyDQoiMjAxMyIsIjc0Y20iLCIyMDA3IiwiMzE0IiwiMTg2Y20iLCIxOTczIiwiMTgwZTBjIiwiaHpsIiw3NCwiY20iLDc0DQoiMjAxMCIsIjE4NGNtIiwiMTk4NSIsTkEsIiMxNzUxMjkiLCIyMDMyIiwiI2ZmZmZmZCIsImh6bCIsMTg0LCJjbSIsMTg0DQoiMjAxNyIsIjE2NmNtIiwiMjAxMiIsIjE3MiIsIiM0MjRhZTQiLCIyMDIyIiwiYjEzMTliIiwiIzY2MzVkOCIsMTY2LCJjbSIsMTY2DQoiMTkyOCIsIjE4NWNtIiwiMTk4NCIsTkEsIiNhYzVhOTAiLCIyMDMwIiwiYWM4ZjQzIiwiYnJuIiwxODUsImNtIiwxODUNCiIyMDIwIiwiNzBjbSIsIjIwMDQiLCIxNjYiLE5BLCIyMDQwIiwiIzczMzgyMCIsImx6ciIsNzAsImNtIiw3MA0KIjIwMTAiLCIxNTVpbiIsIjIwMTYiLCI2MSIsIjk1OTQyODM4MDMiLCIyMDI4IiwiI2NmYTA3ZCIsImdybiIsMTU1LCJpbiIsMzkzLjcNCiIyMDE2IiwiMTU5Y20iLCIxOTkxIixOQSwiOTQ5NjE3MTM4NCIsIjIwMzAiLCIjY2ViM2ExIiwieHJ5IiwxNTksImNtIiwxNTkNCk5BLCI2NmNtIiwiMTkyOCIsIjg3IiwiMjcxMDc5NDYiLCIyMDQwIixOQSwidXRjIiw2NiwiY20iLDY2DQoiMjAyOSIsIjY4IiwiMTk1OSIsTkEsIjkwMTc2MDk0OTciLCIyMDIzIiwiNGUwMjNiIiwiYmx1Iiw2OCxOQSxOQQ0KIjIwMTgiLCIxNTBpbiIsIjE5MzQiLCIxNjMiLCIyODYzMjg0NzU0IiwiMTk3NyIsIiM2MjNhMmYiLCJicm4iLDE1MCwiaW4iLDM4MQ0KIjIwMTgiLCIxNTZjbSIsIjE5NzciLCIyNzgiLE5BLCIyMDI3IiwiIzg4ODc4NSIsImJsdSIsMTU2LCJjbSIsMTU2DQoiMjAyMSIsIjgyIiwiMjAwNyIsIjE5MSIsIiMxY2Y2OWYiLCIyMDM5IiwieiIsImRuZSIsODIsTkEsTkENCiIxOTUwIiwiNjZjbSIsTkEsIjExMSIsIjE4M2NtIiwiMTk0NyIsTkEsIiMwMTZmNmEiLDY2LCJjbSIsNjYNCiIyMDIwIiwiMTYzY20iLCIxOTY3IiwiMzA5IiwiMTI4NTU4NDI5MyIsIjIwMzEiLCIjNzMzODIwIiwib3RoIiwxNjMsImNtIiwxNjMNCiIyMDE1IiwiMTY3Y20iLCIxOTYwIiwiMzEzIixOQSwiMjAyNSIsIiMxODE3MWQiLCJicm4iLDE2NywiY20iLDE2Nw0KIjIwMTMiLCIxODNjbSIsIjE5ODUiLE5BLE5BLCIyMDIyIiwiI2NmYTA3ZCIsImdybiIsMTgzLCJjbSIsMTgzDQoiMjAyMyIsTkEsIjE5NjYiLE5BLCIxNjRjbSIsIjIwMjEiLCJ6IiwidXRjIixOQSxOQSxOQQ0KIjIwMTIiLCIxODBjbSIsIjE5NDMiLCIxNDQiLCIxNTVjbSIsIjIwMzUiLE5BLCJhbWIiLDE4MCwiY20iLDE4MA0KIjIwMjAiLCIxOTNpbiIsIjIwMjYiLCI4MiIsTkEsIjIwMzQiLCIjYjY2NTJhIiwiZ3JuIiwxOTMsImluIiw0OTAuMjINCiIxOTIyIiwiMTAxIiwiMjAxNSIsIjEzNiIsIjE1MWNtIiwiMjA0MCIsIjI0NWNiMyIsImx6ciIsMTAxLE5BLE5BDQoiMjAyOCIsIjE5M2luIiwiMjAyNSIsIjMwOCIsIjkzMzUxNTMyODkiLCIyMDI5IiwieiIsImdyeSIsMTkzLCJpbiIsNDkwLjIyDQo=" download="extract_0006.csv">   <button aria-label="There are 78 &#39;fail&#39; rows available as a CSV file." data-balloon-pos="left" style="background-color:#67C2DC;color:#FFFFFF;border:none;padding:5px;font-weight:bold;cursor:pointer;border-radius:4px;">CSV</button> </a> </div></td>     </tr>     <tr>       <td class="gt_row gt_left" style="background-color: rgba(76,166,76,0.5); height:  40px"></td>       <td class="gt_row gt_right" style="color: #666666; font-size: 13px; font-weight: bold; height:  40px">7</td>       <td class="gt_row gt_left" style="height:  40px"><div class='gt_from_md'><div aria-label="Expect that values in`hgt\_cm`should be between`150`and`193\`.
" data-balloon-pos=“right” style=“width:fit-content;”&gt;

<svg width="30px" height="30px" viewBox="0 0 67 67" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<defs>
<path d="M10.712234,0 L56.712234,0 C62.2350815,-1.01453063e-15 66.712234,4.4771525 66.712234,10 L66.712234,66 L10.712234,66 C5.18938647,66 0.712233968,61.5228475 0.712233968,56 L0.712233968,10 C0.712233968,4.4771525 5.18938647,1.01453063e-15 10.712234,0 Z" id="path-1"></path>
</defs>
<g id="pointblank" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
<g id="col_vals_between" transform="translate(-0.487938, 0.651308)">
<g id="rectangle">
<use fill="#FFFFFF" fill-rule="evenodd" xlink:href="#path-1"></use>
<path stroke="#000000" stroke-width="2" d="M65.712234,65 L65.712234,10 C65.712234,5.02943725 61.6827967,1 56.712234,1 L10.712234,1 C5.74167122,1 1.71223397,5.02943725 1.71223397,10 L1.71223397,56 C1.71223397,60.9705627 5.74167122,65 10.712234,65 L65.712234,65 Z"></path>
</g>
<path d="M11.993484,21.96875 C10.962234,22.082031 10.188797,22.964844 10.212234,24 L10.212234,42 C10.200515,42.722656 10.579422,43.390625 11.204422,43.753906 C11.825515,44.121094 12.598953,44.121094 13.220047,43.753906 C13.845047,43.390625 14.223953,42.722656 14.212234,42 L14.212234,24 C14.220047,23.457031 14.009109,22.9375 13.626297,22.554688 C13.243484,22.171875 12.723953,21.960938 12.180984,21.96875 C12.118484,21.964844 12.055984,21.964844 11.993484,21.96875 Z M55.993484,21.96875 C54.962234,22.082031 54.188797,22.964844 54.212234,24 L54.212234,42 C54.200515,42.722656 54.579422,43.390625 55.204422,43.753906 C55.825515,44.121094 56.598953,44.121094 57.220047,43.753906 C57.845047,43.390625 58.223953,42.722656 58.212234,42 L58.212234,24 C58.220047,23.457031 58.009109,22.9375 57.626297,22.554688 C57.243484,22.171875 56.723953,21.960938 56.180984,21.96875 C56.118484,21.964844 56.055984,21.964844 55.993484,21.96875 Z M16.212234,22 C15.661453,22 15.212234,22.449219 15.212234,23 C15.212234,23.550781 15.661453,24 16.212234,24 C16.763015,24 17.212234,23.550781 17.212234,23 C17.212234,22.449219 16.763015,22 16.212234,22 Z M20.212234,22 C19.661453,22 19.212234,22.449219 19.212234,23 C19.212234,23.550781 19.661453,24 20.212234,24 C20.763015,24 21.212234,23.550781 21.212234,23 C21.212234,22.449219 20.763015,22 20.212234,22 Z M24.212234,22 C23.661453,22 23.212234,22.449219 23.212234,23 C23.212234,23.550781 23.661453,24 24.212234,24 C24.763015,24 25.212234,23.550781 25.212234,23 C25.212234,22.449219 24.763015,22 24.212234,22 Z M28.212234,22 C27.661453,22 27.212234,22.449219 27.212234,23 C27.212234,23.550781 27.661453,24 28.212234,24 C28.763015,24 29.212234,23.550781 29.212234,23 C29.212234,22.449219 28.763015,22 28.212234,22 Z M32.212234,22 C31.661453,22 31.212234,22.449219 31.212234,23 C31.212234,23.550781 31.661453,24 32.212234,24 C32.763015,24 33.212234,23.550781 33.212234,23 C33.212234,22.449219 32.763015,22 32.212234,22 Z M36.212234,22 C35.661453,22 35.212234,22.449219 35.212234,23 C35.212234,23.550781 35.661453,24 36.212234,24 C36.763015,24 37.212234,23.550781 37.212234,23 C37.212234,22.449219 36.763015,22 36.212234,22 Z M40.212234,22 C39.661453,22 39.212234,22.449219 39.212234,23 C39.212234,23.550781 39.661453,24 40.212234,24 C40.763015,24 41.212234,23.550781 41.212234,23 C41.212234,22.449219 40.763015,22 40.212234,22 Z M44.212234,22 C43.661453,22 43.212234,22.449219 43.212234,23 C43.212234,23.550781 43.661453,24 44.212234,24 C44.763015,24 45.212234,23.550781 45.212234,23 C45.212234,22.449219 44.763015,22 44.212234,22 Z M48.212234,22 C47.661453,22 47.212234,22.449219 47.212234,23 C47.212234,23.550781 47.661453,24 48.212234,24 C48.763015,24 49.212234,23.550781 49.212234,23 C49.212234,22.449219 48.763015,22 48.212234,22 Z M52.212234,22 C51.661453,22 51.212234,22.449219 51.212234,23 C51.212234,23.550781 51.661453,24 52.212234,24 C52.763015,24 53.212234,23.550781 53.212234,23 C53.212234,22.449219 52.763015,22 52.212234,22 Z M21.462234,27.96875 C21.419265,27.976563 21.376297,27.988281 21.337234,28 C21.177078,28.027344 21.02864,28.089844 20.899734,28.1875 L15.618484,32.1875 C15.356765,32.375 15.200515,32.679688 15.200515,33 C15.200515,33.320313 15.356765,33.625 15.618484,33.8125 L20.899734,37.8125 C21.348953,38.148438 21.985672,38.058594 22.321609,37.609375 C22.657547,37.160156 22.567703,36.523438 22.118484,36.1875 L19.212234,34 L49.212234,34 L46.305984,36.1875 C45.856765,36.523438 45.766922,37.160156 46.102859,37.609375 C46.438797,38.058594 47.075515,38.148438 47.524734,37.8125 L52.805984,33.8125 C53.067703,33.625 53.223953,33.320313 53.223953,33 C53.223953,32.679688 53.067703,32.375 52.805984,32.1875 L47.524734,28.1875 C47.30989,28.027344 47.040359,27.960938 46.774734,28 C46.743484,28 46.712234,28 46.680984,28 C46.282547,28.074219 45.96614,28.382813 45.884109,28.78125 C45.802078,29.179688 45.970047,29.585938 46.305984,29.8125 L49.212234,32 L19.212234,32 L22.118484,29.8125 C22.520828,29.566406 22.696609,29.070313 22.536453,28.625 C22.380203,28.179688 21.930984,27.90625 21.462234,27.96875 Z M16.212234,42 C15.661453,42 15.212234,42.449219 15.212234,43 C15.212234,43.550781 15.661453,44 16.212234,44 C16.763015,44 17.212234,43.550781 17.212234,43 C17.212234,42.449219 16.763015,42 16.212234,42 Z M20.212234,42 C19.661453,42 19.212234,42.449219 19.212234,43 C19.212234,43.550781 19.661453,44 20.212234,44 C20.763015,44 21.212234,43.550781 21.212234,43 C21.212234,42.449219 20.763015,42 20.212234,42 Z M24.212234,42 C23.661453,42 23.212234,42.449219 23.212234,43 C23.212234,43.550781 23.661453,44 24.212234,44 C24.763015,44 25.212234,43.550781 25.212234,43 C25.212234,42.449219 24.763015,42 24.212234,42 Z M28.212234,42 C27.661453,42 27.212234,42.449219 27.212234,43 C27.212234,43.550781 27.661453,44 28.212234,44 C28.763015,44 29.212234,43.550781 29.212234,43 C29.212234,42.449219 28.763015,42 28.212234,42 Z M32.212234,42 C31.661453,42 31.212234,42.449219 31.212234,43 C31.212234,43.550781 31.661453,44 32.212234,44 C32.763015,44 33.212234,43.550781 33.212234,43 C33.212234,42.449219 32.763015,42 32.212234,42 Z M36.212234,42 C35.661453,42 35.212234,42.449219 35.212234,43 C35.212234,43.550781 35.661453,44 36.212234,44 C36.763015,44 37.212234,43.550781 37.212234,43 C37.212234,42.449219 36.763015,42 36.212234,42 Z M40.212234,42 C39.661453,42 39.212234,42.449219 39.212234,43 C39.212234,43.550781 39.661453,44 40.212234,44 C40.763015,44 41.212234,43.550781 41.212234,43 C41.212234,42.449219 40.763015,42 40.212234,42 Z M44.212234,42 C43.661453,42 43.212234,42.449219 43.212234,43 C43.212234,43.550781 43.661453,44 44.212234,44 C44.763015,44 45.212234,43.550781 45.212234,43 C45.212234,42.449219 44.763015,42 44.212234,42 Z M48.212234,42 C47.661453,42 47.212234,42.449219 47.212234,43 C47.212234,43.550781 47.661453,44 48.212234,44 C48.763015,44 49.212234,43.550781 49.212234,43 C49.212234,42.449219 48.763015,42 48.212234,42 Z M52.212234,42 C51.661453,42 51.212234,42.449219 51.212234,43 C51.212234,43.550781 51.661453,44 52.212234,44 C52.763015,44 53.212234,43.550781 53.212234,43 C53.212234,42.449219 52.763015,42 52.212234,42 Z" id="inside_range" fill="#000000" fill-rule="nonzero"></path>
</g> </g>
</svg>

<code style="font-size:11px;"> col\_vals\_between()</code>
</div>
</div>
</td>

      <td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px"><div class='gt_from_md'><div aria-label="hgt_cm" data-balloon-pos="left">

<p style="margin-top:0;margin-bottom:0;font-size:11px;white-space:nowrap;text-overflow:ellipsis;overflow:hidden;line-height:2em;">
<code><span style="color:purple;">▮</span>hgt\_cm</code>
</p>
</div>
</div>
</td>

      <td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px"><div class='gt_from_md'><div aria-label="[150, 193]" data-balloon-pos="left"><p style="margin-top: 0px; margin-bottom: 0px; font-size: 11px; white-space: nowrap; text-overflow: ellipsis; overflow: hidden;"><code>[150, 193]</code></p></div>

</div>
</td>

      <td class="gt_row gt_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; background-color: #FCFCFC; height:  40px"><div class='gt_from_md'><p><span style="background:transparent;padding:0;color:#333333;vertical-align:middle;font-size:18px;border:none;border-radius:4px;" aria-label="No modifications of the table." data-balloon-pos="left">→</span></p>

</div>
</td>

      <td class="gt_row gt_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; background-color: #FCFCFC; height:  40px"><div class='gt_from_md'><p><span style="background:transparent;padding:5px;color:#4CA64C;vertical-align:middle;font-size:15px;border:none;" aria-label="No evaluation issues." data-balloon-pos="left">✓</span></p>

</div>
</td>

      <td class="gt_row gt_right" style="height:  40px"><code>276</code></td>
      <td class="gt_row gt_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px"><code>206</code><br><code>0.75</code></td>
      <td class="gt_row gt_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px"><code>70</code><br><code>0.25</code></td>
      <td class="gt_row gt_center" style="border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; background-color: #FCFCFC; height:  40px"><div class='gt_from_md'><p>—</p>

</div>
</td>

      <td class="gt_row gt_center" style="background-color: #FCFCFC; height:  40px"><div class='gt_from_md'><p>—</p>

</div>
</td>

      <td class="gt_row gt_center" style="border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; background-color: #FCFCFC; height:  40px"><div class='gt_from_md'><p>—</p>

</div>
</td>

      <td class="gt_row gt_center" style="height:  40px"><div class='gt_from_md'><a href="data:text/csv;base64,Iml5ciIsImhndCIsImJ5ciIsImNpZCIsInBpZCIsImV5ciIsImhjbCIsImVjbCIsImhndF9udW0iLCJoZ3RfdW5pdCIsImhndF9jbSINCiIyMDE1IiwiNTljbSIsIjIwMjkiLCIyMTkiLCI5MzgxNjg4NzUzIiwiMTk5MiIsIiNiNjY1MmEiLCIjN2EwZmE2Iiw1OSwiY20iLDU5DQoiMjAxOCIsTkEsIjE5NjgiLCIzMzUiLCI5NDM2MTQ3NTUiLCIyMDI2IixOQSwiYmx1IixOQSxOQSxOQQ0KIjIwMjUiLCI2NGNtIiwiMjAyOSIsIjI4MSIsIjA2NzI4NTk4NSIsIjE5NDQiLCIjY2ViM2ExIiwiIzA3MjE5YSIsNjQsImNtIiw2NA0KIjIwMjUiLCIxNDEiLCIyMDA2IiwiMzI3IiwiIzFjNDJjYyIsIjE5NTYiLCJ6IiwiI2YyYWZmYyIsMTQxLE5BLE5BDQoiMTkyNCIsIjc0Y20iLCIyMDI0IiwiMTUzIiwiMzYxOTY0MDEiLCIxOTIxIiwiYTRlNGMwIiwiIzNhY2Y1NyIsNzQsImNtIiw3NA0KIjIwMjAiLCIxNTEiLCIyMDEyIiwiMzE0IiwiOTg0MzY0ODYyIiwiMjAyMyIsInoiLCJkbmUiLDE1MSxOQSxOQQ0KIjE5MzMiLCI5MCIsIjIwMjUiLE5BLCI4MTk0MzQ3NTQ0IiwiMjA0MCIsTkEsImRuZSIsOTAsTkEsTkENCiIxOTIwIiwiNjBjbSIsIjIwMjkiLCIxMDciLCIjNTVjZTZiIiwiMjA0MCIsImQzMGY2YiIsImRuZSIsNjAsImNtIiw2MA0KIjIwMTgiLCI2NWNtIiwiMTkzNyIsTkEsIiM0YmZmM2UiLCIyMDI3IiwiNDNmYWZiIiwiZ3J0Iiw2NSwiY20iLDY1DQoiMTkzOCIsIjE3MmluIiwiMjAxOCIsIjMzOSIsIiM2YzEyMTYiLCIyMDM5IiwieiIsIiM1MTA2NzIiLDE3MiwiaW4iLDQzNi44OA0KIjE5OTciLCI5NiIsIjIwMDkiLE5BLCIwNjM2OTE3MjIyIiwiMjAyNiIsInoiLCJoemwiLDk2LE5BLE5BDQoiMTk1MSIsIjE5M2luIiwiMjAxOSIsTkEsIiNjYmMwOGMiLCIyMDAyIiwieiIsIiMzZTlmMmYiLDE5MywiaW4iLDQ5MC4yMg0KTkEsIjE3NWluIiwiMjAxMiIsIjI0MCIsIjU3MTQ3NzE3NiIsIjE5MjkiLCJmNGVmMzIiLCJ1dGMiLDE3NSwiaW4iLDQ0NC41DQoiMjAyNiIsIjcwIiwiMjAyMiIsTkEsIjQwNDYyMjA4MyIsIjE5NzkiLCJjMWJhN2YiLCJsenIiLDcwLE5BLE5BDQpOQSwiODIiLCIyMDE3IiwiMTUzIiwiMTcyNjYxNjE3IiwiMjAzNiIsInoiLCIjYWU2MDdkIiw4MixOQSxOQQ0KIjE5NDUiLCIxNjVpbiIsIjIwMTAiLCIyNzMiLCI0MzcwNzUwNCIsIjIwMjYiLCJ6IiwiZ3J0IiwxNjUsImluIiw0MTkuMQ0KIjIwMjEiLCIxODhpbiIsIjIwMDgiLE5BLCI1Njk1ODM1MDkiLE5BLCJmNzQ4MjMiLCJoemwiLDE4OCwiaW4iLDQ3Ny41Mg0KIjIwMTYiLCI2N2NtIiwiMjAxMCIsIjE0NCIsIjM0OTQ5MjI0MyIsIjIwMjEiLCJ6IiwiIzI0Y2VlZSIsNjcsImNtIiw2Nw0KTkEsIjE2M2luIiwiMjAyOSIsTkEsTkEsIjE5NjIiLCIzYTBjMzAiLCJncnkiLDE2MywiaW4iLDQxNC4wMg0KIjIwMjQiLCIxODAiLCIxOTI3IiwiODciLCI1OTcwOTg5NDAiLCIyMDI3IiwiIzYyM2EyZiIsIiM3ZWE3NzciLDE4MCxOQSxOQQ0KIjIwMjUiLCIxNTIiLCIyMDEyIixOQSwiMzgyMTc0NzQ0IiwiMjAyNCIsIiM4ODg3ODUiLCIjOTVkOGE5IiwxNTIsTkEsTkENCiIxOTk3IiwiNjljbSIsIjIwMTIiLCI1NCIsIjE4NGNtIiwiMjAzNyIsImY4ZWQ0NSIsImdyeSIsNjksImNtIiw2OQ0KIjE5NTUiLCIxNjNpbiIsIjIwMTgiLCIyOTgiLCI4NzE1NTI2NiIsIjIwMjEiLCIjYzA5NDZmIiwiIzIxNjkyMCIsMTYzLCJpbiIsNDE0LjAyDQoiMjAxNyIsIjgzIiwiMTk0MCIsTkEsTkEsIjIwMzIiLCIxNzQ3NzQiLCJ4cnkiLDgzLE5BLE5BDQoiMjAxNSIsIjY2IiwiMTk0MyIsTkEsIjk4MDM1MjY0NSIsIjIwMjMiLCIjYjY2NTJhIiwib3RoIiw2NixOQSxOQQ0KIjIwMTIiLCI2MmNtIiwiMTk4NSIsIjU5IiwiI2FmMTA2YSIsIjE5NTciLCIjNjIzYTJmIiwiZ3J0Iiw2MiwiY20iLDYyDQpOQSwiNjNjbSIsIjIwMDQiLCIxODgiLCI1NjIxMjAwMTM0IiwiMTkyNCIsIjZlZjliYSIsIiNlZjY4ZjUiLDYzLCJjbSIsNjMNCiIyMDIxIiwiMTY3aW4iLCIyMDI1IiwiMjM0IiwiMTgxY20iLCIxOTQxIiwiOTg2MTlhIiwiI2Y1ZTY1MSIsMTY3LCJpbiIsNDI0LjE4DQoiMTk0OSIsIjczY20iLCIxOTg1IiwiMjA3IiwiI2VlOWY5NSIsIjIwMjgiLCJ6IiwidXRjIiw3MywiY20iLDczDQoiMTkzMyIsIjY5Y20iLCIxOTM0IixOQSwiMTc5Y20iLCIyMDMwIiwiYjhlMTQyIiwiZ3JuIiw2OSwiY20iLDY5DQoiMjAyOCIsIjExMSIsIjIwMzAiLCIxODAiLCIxODMzOTE4NjEiLCIxOTU0IiwiMWZiMzBmIiwiIzBkMDE2MCIsMTExLE5BLE5BDQoiMjAxNiIsIjEzNSIsIjIwMTIiLE5BLCI3NDg3ODY4NzciLCIyMDExIiwiYjZlOTYyIiwiZ3J5IiwxMzUsTkEsTkENCk5BLE5BLCIyMDIzIiwiMjcxIiwiIzY2ZWM4MiIsIjIwMjkiLCIxMGQ5ZDgiLCIjMzUzZTBmIixOQSxOQSxOQQ0KIjE5ODEiLCI1OWNtIiwiMjAwOSIsTkEsIjE2MmNtIiwiMjAyNSIsIjExNjc0MiIsImdtdCIsNTksImNtIiw1OQ0KIjIwMjMiLCI2N2NtIiwiMTk2MyIsTkEsIjYyODU5MzMyIiwiMjAyOCIsIjNkMWYzNCIsImRuZSIsNjcsImNtIiw2Nw0KIjIwMTMiLCIxNTdpbiIsIjIwMjMiLCIxODYiLCIyNzE0NTA3NTQiLCIyMDE2IiwiZTIwODgyIiwidXRjIiwxNTcsImluIiwzOTguNzgNCiIyMDE4IiwiMTkzaW4iLCIxOTQzIixOQSwiNzAyMjAwMDI2IiwiMTk2OCIsIiM4ODg3ODUiLCJnbXQiLDE5MywiaW4iLDQ5MC4yMg0KIjIwMTEiLE5BLCIxOTc0IiwiMTgyIiwiOTQ5MzQ0Nzg1IiwiMjAyNSIsIiNjZWIzYTEiLCJncm4iLE5BLE5BLE5BDQoiMjAwMyIsIjE1MCIsIjIwMDEiLCIyOTIiLCI5MzQ3MzE5MiIsIjE5MjIiLCJ6IiwiI2Q1NmJiZCIsMTUwLE5BLE5BDQoiMTkyOSIsIjE5MyIsTkEsIjMzMCIsIjMzNzY1NDI2IiwiMjAzOCIsInoiLCIjMThlODgzIiwxOTMsTkEsTkENCiIxOTY1IiwiNjAiLCIxOTMxIixOQSwiI2FhNWZkMCIsIjIwMTciLCI1NzkyNjYiLCJncnkiLDYwLE5BLE5BDQoiMTk5NyIsIjYxY20iLCIyMDIzIixOQSwiMTYyY20iLCIxOTg3IiwieiIsIiM5ZjQ1OGMiLDYxLCJjbSIsNjENCiIyMDIwIixOQSwiMjAxMCIsTkEsIjA2NTYwNzY0OSIsTkEsIjU5ZTM3NiIsImJsdSIsTkEsTkEsTkENCiIyMDE3IixOQSwiMTkzMiIsIjIwMyIsIjEwNTkyMTA4NSIsTkEsIiM0MTlkNzMiLCJncnkiLE5BLE5BLE5BDQoiMjAyNCIsIjE4MWluIiwiMjAyOSIsIjk3IiwiI2MzODYyMCIsIjE5NzYiLCJ6Iiwienp6IiwxODEsImluIiw0NTkuNzQNCiIyMDE0IiwiMTcyaW4iLCIxOTUwIixOQSwiMTg3Y20iLCIyMDI4IiwieiIsImJybiIsMTcyLCJpbiIsNDM2Ljg4DQoiMjAyNSIsIjU5aW4iLCIxOTIwIiwiMjAyIiwiMTgyY20iLCIyMDM1IiwieiIsIiM3OTlmMjkiLDU5LCJpbiIsMTQ5Ljg2DQoiMjAxMyIsTkEsIjE5NTEiLCIyNTciLCIxMjMwNjU2MzkiLE5BLCIjN2QzYjBjIiwiZ3J5IixOQSxOQSxOQQ0KTkEsIjczY20iLCIxOTI3IixOQSwiNzE5NzM4NDY4IiwiMjAzOSIsTkEsIiNhODJlOTAiLDczLCJjbSIsNzMNCiIyMDA5IiwiMTcyaW4iLCIyMDA1IiwiMzM0IiwiMTg5Y20iLCIyMDMyIiwieiIsInhyeSIsMTcyLCJpbiIsNDM2Ljg4DQoiMjAxMCIsIjE2NyIsIjIwMDMiLCIxNjkiLE5BLCIyMDM3IiwiNDg2ODAwIiwiIzI5YmRkNiIsMTY3LE5BLE5BDQoiMTk4MiIsIjE0NCIsIjIwMTIiLE5BLCIjM2I0M2MxIiwiMjAzMiIsIjJmMjZhYiIsIiNmODlkZjAiLDE0NCxOQSxOQQ0KIjIwMTgiLCIxNzBpbiIsIjE5NzEiLE5BLCIwMzA4NTA3NDkiLCIyMDIzIiwiI2NlYjNhMSIsImh6bCIsMTcwLCJpbiIsNDMxLjgNCiIyMDIwIiwiMTg1aW4iLCIyMDI4IixOQSwiNzczNzM5NzQ0IiwiMTk4NiIsInoiLCJkbmUiLDE4NSwiaW4iLDQ2OS45DQoiMjAyNCIsIjE2N2luIiwiMjAwNiIsIjM0NiIsIjE5NTMxMzQxIiwiMjAzNSIsInoiLCIjZDMyMzIwIiwxNjcsImluIiw0MjQuMTgNCiIyMDE1IiwiNzNjbSIsIjE5NjgiLCI1NCIsIjQ3MDMxNzY5MCIsIjIwMjkiLCIjYzA5NDZmIiwiYmx1Iiw3MywiY20iLDczDQoiMjAxMyIsIjc0Y20iLCIyMDA3IiwiMzE0IiwiMTg2Y20iLCIxOTczIiwiMTgwZTBjIiwiaHpsIiw3NCwiY20iLDc0DQoiMjAyMCIsIjcwY20iLCIyMDA0IiwiMTY2IixOQSwiMjA0MCIsIiM3MzM4MjAiLCJsenIiLDcwLCJjbSIsNzANCiIyMDEwIiwiMTU1aW4iLCIyMDE2IiwiNjEiLCI5NTk0MjgzODAzIiwiMjAyOCIsIiNjZmEwN2QiLCJncm4iLDE1NSwiaW4iLDM5My43DQpOQSwiMTI4IiwiMTk5NyIsIjI5OSIsIjk4NDY3NTE5OCIsIjIwMzciLCIjYjY2NTJhIiwiZ210IiwxMjgsTkEsTkENCiIxOTcxIiwiMTU3aW4iLCIxOTcwIixOQSwiODEyNjE3ODA5IiwiMjAyMCIsIiM3ZDNiMGMiLCJnbXQiLDE1NywiaW4iLDM5OC43OA0KTkEsIjY2Y20iLCIxOTI4IiwiODciLCIyNzEwNzk0NiIsIjIwNDAiLE5BLCJ1dGMiLDY2LCJjbSIsNjYNCiIyMDI5IiwiNjgiLCIxOTU5IixOQSwiOTAxNzYwOTQ5NyIsIjIwMjMiLCI0ZTAyM2IiLCJibHUiLDY4LE5BLE5BDQoiMjAxOCIsIjE1MGluIiwiMTkzNCIsIjE2MyIsIjI4NjMyODQ3NTQiLCIxOTc3IiwiIzYyM2EyZiIsImJybiIsMTUwLCJpbiIsMzgxDQoiMjAyMSIsIjgyIiwiMjAwNyIsIjE5MSIsIiMxY2Y2OWYiLCIyMDM5IiwieiIsImRuZSIsODIsTkEsTkENCiIxOTUwIiwiNjZjbSIsTkEsIjExMSIsIjE4M2NtIiwiMTk0NyIsTkEsIiMwMTZmNmEiLDY2LCJjbSIsNjYNCiIyMDIzIixOQSwiMTk2NiIsTkEsIjE2NGNtIiwiMjAyMSIsInoiLCJ1dGMiLE5BLE5BLE5BDQoiMjAyMCIsIjE5M2luIiwiMjAyNiIsIjgyIixOQSwiMjAzNCIsIiNiNjY1MmEiLCJncm4iLDE5MywiaW4iLDQ5MC4yMg0KIjE5MjIiLCIxMDEiLCIyMDE1IiwiMTM2IiwiMTUxY20iLCIyMDQwIiwiMjQ1Y2IzIiwibHpyIiwxMDEsTkEsTkENCiIyMDI4IiwiMTkzaW4iLCIyMDI1IiwiMzA4IiwiOTMzNTE1MzI4OSIsIjIwMjkiLCJ6IiwiZ3J5IiwxOTMsImluIiw0OTAuMjINCg==" download="extract_0007.csv">

<button aria-label="There are 70 &#39;fail&#39; rows available as a CSV file." data-balloon-pos="left" style="background-color:#67C2DC;color:#FFFFFF;border:none;padding:5px;font-weight:bold;cursor:pointer;border-radius:4px;">CSV</button>
</a>
</div>
</td>

    </tr>

</tbody>
<tfoot class="gt_sourcenotes">
<tr>
<td class="gt_sourcenote" colspan="14">
<span
style="background-color: #FFF;color: #444;padding: 0.5em 0.5em;position: inherit;text-transform: uppercase;margin-left: 10px;border: solid 1px #999999;font-variant-numeric: tabular-nums;border-radius: 0;padding: 2px 10px 2px 10px;font-size: smaller;">2020-12-04
10:33:19 EST</span><span
style="background-color: #FFF;color: #444;padding: 0.5em 0.5em;position: inherit;margin: 5px 1px 5px 0;border: solid 1px #999999;border-left: none;font-variant-numeric: tabular-nums;border-radius: 0;padding: 2px 10px 2px 10px;font-size: smaller;">&lt;
1 s</span><span
style="background-color: #FFF;color: #444;padding: 0.5em 0.5em;position: inherit;text-transform: uppercase;margin: 5px 1px 5px -1px;border: solid 1px #999999;border-left: none;border-radius: 0;padding: 2px 10px 2px 10px;font-size: smaller;">2020-12-04
10:33:19 EST</span>
</td>
</tr>
</tfoot>
</table>
</div>
<!--/html_preserve-->

    get_sundered_data(valid_report, type = "pass") %>% nrow()

    ## [1] 131
