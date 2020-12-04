# Challenge 1

    library(readr)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(purrr)

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

    library(pointblank)

    passport_data %>%
      create_agent() %>% 
      col_vals_between(vars(byr), 1920, 2002) %>% 
      col_vals_between(vars(iyr), 2010, 2020) %>% 
      col_vals_between(vars(eyr), 2020, 2030) %>% 
      col_vals_regex(vars(hcl), "^#(?:[0-9a-fA-F]{6})$") %>% 
      col_vals_in_set(vars(ecl), c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) %>% 
      col_vals_between(vars(pid), 100000000, 999999999) %>% 
      interrogate()

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
style="text-decoration-style:solid;text-decoration-color:#ADD8E6;text-decoration-line:underline;text-underline-position:under;color:#333333;font-variant-numeric:tabular-nums;padding-left:4px;margin-right:5px;padding-right:2px;">\[2020-12-04|08:57:27\]</span>
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

<a href="data:text/csv;base64,Iml5ciIsImhndCIsImJ5ciIsImNpZCIsInBpZCIsImV5ciIsImhjbCIsImVjbCINCiIyMDE1IiwiNTljbSIsIjIwMjkiLCIyMTkiLCI5MzgxNjg4NzUzIiwiMTk5MiIsIiNiNjY1MmEiLCIjN2EwZmE2Ig0KIjIwMjUiLCI2NGNtIiwiMjAyOSIsIjI4MSIsIjA2NzI4NTk4NSIsIjE5NDQiLCIjY2ViM2ExIiwiIzA3MjE5YSINCiIyMDI1IiwiMTQxIiwiMjAwNiIsIjMyNyIsIiMxYzQyY2MiLCIxOTU2IiwieiIsIiNmMmFmZmMiDQoiMTkyNCIsIjc0Y20iLCIyMDI0IiwiMTUzIiwiMzYxOTY0MDEiLCIxOTIxIiwiYTRlNGMwIiwiIzNhY2Y1NyINCiIyMDIwIiwiMTUxIiwiMjAxMiIsIjMxNCIsIjk4NDM2NDg2MiIsIjIwMjMiLCJ6IiwiZG5lIg0KIjE5MzMiLCI5MCIsIjIwMjUiLE5BLCI4MTk0MzQ3NTQ0IiwiMjA0MCIsTkEsImRuZSINCiIxOTIwIiwiNjBjbSIsIjIwMjkiLCIxMDciLCIjNTVjZTZiIiwiMjA0MCIsImQzMGY2YiIsImRuZSINCiIyMDE2IiwiMTcyY20iLE5BLE5BLCIjOThjYWVjIiwiMjAzNiIsInoiLCJkbmUiDQoiMTkzOCIsIjE3MmluIiwiMjAxOCIsIjMzOSIsIiM2YzEyMTYiLCIyMDM5IiwieiIsIiM1MTA2NzIiDQoiMTk5NyIsIjk2IiwiMjAwOSIsTkEsIjA2MzY5MTcyMjIiLCIyMDI2IiwieiIsImh6bCINCiIxOTUxIiwiMTkzaW4iLCIyMDE5IixOQSwiI2NiYzA4YyIsIjIwMDIiLCJ6IiwiIzNlOWYyZiINCk5BLCIxNzVpbiIsIjIwMTIiLCIyNDAiLCI1NzE0NzcxNzYiLCIxOTI5IiwiZjRlZjMyIiwidXRjIg0KIjIwMjYiLCI3MCIsIjIwMjIiLE5BLCI0MDQ2MjIwODMiLCIxOTc5IiwiYzFiYTdmIiwibHpyIg0KIjIwMTgiLCIxNTZjbSIsIjIwMTciLCIxNzkiLCIyOTgzNjEyNCIsIjIwMjMiLCI1NmRlODMiLCJ6enoiDQpOQSwiODIiLCIyMDE3IiwiMTUzIiwiMTcyNjYxNjE3IiwiMjAzNiIsInoiLCIjYWU2MDdkIg0KIjIwMjAiLCI2M2luIixOQSxOQSwiMTQ2NjUwODk0IiwiMjAyNSIsIiNhMzU1YmUiLCJhbWIiDQoiMjAxNiIsIjE5MmNtIixOQSxOQSwiNTMxMzcyOTY1IiwiMjAyNSIsIiNmZmZmZmQiLCJibHUiDQoiMTk0NSIsIjE2NWluIiwiMjAxMCIsIjI3MyIsIjQzNzA3NTA0IiwiMjAyNiIsInoiLCJncnQiDQoiMjAxNSIsIjE1OWNtIixOQSxOQSwiOTE1ODE5MjcyIiwiMjAzMCIsIiM2YjU0NDIiLCJncnkiDQoiMjAyMSIsIjE4OGluIiwiMjAwOCIsTkEsIjU2OTU4MzUwOSIsTkEsImY3NDgyMyIsImh6bCINCiIyMDE2IiwiNjdjbSIsIjIwMTAiLCIxNDQiLCIzNDk0OTIyNDMiLCIyMDIxIiwieiIsIiMyNGNlZWUiDQpOQSwiMTYzaW4iLCIyMDI5IixOQSxOQSwiMTk2MiIsIjNhMGMzMCIsImdyeSINCiIyMDI1IiwiMTUyIiwiMjAxMiIsTkEsIjM4MjE3NDc0NCIsIjIwMjQiLCIjODg4Nzg1IiwiIzk1ZDhhOSINCiIxOTk3IiwiNjljbSIsIjIwMTIiLCI1NCIsIjE4NGNtIiwiMjAzNyIsImY4ZWQ0NSIsImdyeSINCiIxOTU1IiwiMTYzaW4iLCIyMDE4IiwiMjk4IiwiODcxNTUyNjYiLCIyMDIxIiwiI2MwOTQ2ZiIsIiMyMTY5MjAiDQoiMjAxMiIsIjE4NWNtIixOQSxOQSxOQSwiMjAzMCIsIiNiNjY1MmEiLCJncm4iDQpOQSwiNjNjbSIsIjIwMDQiLCIxODgiLCI1NjIxMjAwMTM0IiwiMTkyNCIsIjZlZjliYSIsIiNlZjY4ZjUiDQoiMjAyMSIsIjE2N2luIiwiMjAyNSIsIjIzNCIsIjE4MWNtIiwiMTk0MSIsIjk4NjE5YSIsIiNmNWU2NTEiDQoiMjAyOCIsIjExMSIsIjIwMzAiLCIxODAiLCIxODMzOTE4NjEiLCIxOTU0IiwiMWZiMzBmIiwiIzBkMDE2MCINCk5BLCIxOTFjbSIsIjIwMjMiLE5BLCI3MjcwMjQ2NzYiLCIyMDI1IiwiI2I2NjUyYSIsIiMwYjNiMmQiDQoiMjAxNiIsIjEzNSIsIjIwMTIiLE5BLCI3NDg3ODY4NzciLCIyMDExIiwiYjZlOTYyIiwiZ3J5Ig0KTkEsTkEsIjIwMjMiLCIyNzEiLCIjNjZlYzgyIiwiMjAyOSIsIjEwZDlkOCIsIiMzNTNlMGYiDQoiMjAxMyIsIjE4NWNtIiwiMjAxNCIsTkEsIjgxNjQ4NTA1NCIsIjIwMTkiLCIjZWZjYzk4IiwiZ3JuIg0KIjE5ODEiLCI1OWNtIiwiMjAwOSIsTkEsIjE2MmNtIiwiMjAyNSIsIjExNjc0MiIsImdtdCINCiIyMDEzIiwiMTU3aW4iLCIyMDIzIiwiMTg2IiwiMjcxNDUwNzU0IiwiMjAxNiIsImUyMDg4MiIsInV0YyINCiIyMDE4IiwiMTg2Y20iLE5BLE5BLCIxNzg1MjUxMzIiLCIyMDIzIiwiIzg4ODc4NSIsTkENCiIxOTI5IiwiMTkzIixOQSwiMzMwIiwiMzM3NjU0MjYiLCIyMDM4IiwieiIsIiMxOGU4ODMiDQoiMjAxOSIsIjE2OWNtIiwiMjAxMiIsIjI5OCIsIjA2NjAzMTY1NTgiLCIxOTIwIiwiIzg4ODc4NSIsInp6eiINCiIxOTk3IiwiNjFjbSIsIjIwMjMiLE5BLCIxNjJjbSIsIjE5ODciLCJ6IiwiIzlmNDU4YyINCiIyMDIwIixOQSwiMjAxMCIsTkEsIjA2NTYwNzY0OSIsTkEsIjU5ZTM3NiIsImJsdSINCiIyMDE1IiwiMTUwY20iLCIyMDIyIixOQSwiMTY3Y20iLCIyMDMyIiwiY2FhMTQ1IiwiIzA2NjUwYSINCiIyMDI0IiwiMTgxaW4iLCIyMDI5IiwiOTciLCIjYzM4NjIwIiwiMTk3NiIsInoiLCJ6enoiDQoiMjAxNyIsIjYxaW4iLE5BLCI5MyIsIjgwNzk5MDQ3NiIsIjIwMjciLCIjY2ZhMDdkIiwib3RoIg0KIjIwMjMiLCIxNjJjbSIsIjIwMjUiLCIzNDMiLCI1MDQyNDAzNSIsIjIwMjQiLCI2MDUyMjMiLCJvdGgiDQoiMjAwMSIsIjE3MWNtIiwiMjAxMyIsTkEsIjg5MDA5NjgzMjUiLCIyMDIyIiwiNjk5MTE2IixOQQ0KIjIwMDkiLCIxNzJpbiIsIjIwMDUiLCIzMzQiLCIxODljbSIsIjIwMzIiLCJ6IiwieHJ5Ig0KIjIwMjAiLCIxNTljbSIsTkEsTkEsIjE2NmNtIiwiMjAyNiIsInoiLCJvdGgiDQoiMjAxMCIsIjE2NyIsIjIwMDMiLCIxNjkiLE5BLCIyMDM3IiwiNDg2ODAwIiwiIzI5YmRkNiINCiIyMDI5IiwiMTgwY20iLCIyMDI3IiwiMjA1IiwiNDQzODA5MzM3IiwiMTk4MCIsIiMzNDFlMTMiLCJncnQiDQoiMTk4MiIsIjE0NCIsIjIwMTIiLE5BLCIjM2I0M2MxIiwiMjAzMiIsIjJmMjZhYiIsIiNmODlkZjAiDQoiMjAyMCIsIjE4NWluIiwiMjAyOCIsTkEsIjc3MzczOTc0NCIsIjE5ODYiLCJ6IiwiZG5lIg0KIjIwMjQiLCIxNjdpbiIsIjIwMDYiLCIzNDYiLCIxOTUzMTM0MSIsIjIwMzUiLCJ6IiwiI2QzMjMyMCINCiIyMDE1IiwiMTUyY20iLCIyMDE5IixOQSwiMTgyY20iLCIyMDI4IiwiNDNkNTZkIiwienp6Ig0KIjIwMTEiLCI2NWluIixOQSxOQSwiODUwMTkyNTAyIiwiMjAyMyIsIiM3ZDNiMGMiLCJncnkiDQoiMTk1OCIsIjE3NmNtIixOQSxOQSwiIzRjYjQ4MCIsIjIwMjYiLCJ6IiwiZG5lIg0KIjIwMTMiLCI3NGNtIiwiMjAwNyIsIjMxNCIsIjE4NmNtIiwiMTk3MyIsIjE4MGUwYyIsImh6bCINCiIyMDE3IiwiMTY2Y20iLCIyMDEyIiwiMTcyIiwiIzQyNGFlNCIsIjIwMjIiLCJiMTMxOWIiLCIjNjYzNWQ4Ig0KIjIwMjAiLCI3MGNtIiwiMjAwNCIsIjE2NiIsTkEsIjIwNDAiLCIjNzMzODIwIiwibHpyIg0KIjIwMTAiLCIxNTVpbiIsIjIwMTYiLCI2MSIsIjk1OTQyODM4MDMiLCIyMDI4IiwiI2NmYTA3ZCIsImdybiINCiIyMDExIiwiMTc2Y20iLCIyMDEyIiwiNjQiLCIyMTMyMTMzNTkiLCIxOTcxIiwiYmU3YjEzIiwiZ210Ig0KIjIwMTIiLCIxNjRjbSIsIjIwMDgiLE5BLCI0MjAxNjg0ODEiLCIyMDIzIiwiI2I2NjUyYSIsImdybiINCiIyMDIxIiwiODIiLCIyMDA3IiwiMTkxIiwiIzFjZjY5ZiIsIjIwMzkiLCJ6IiwiZG5lIg0KIjE5NTAiLCI2NmNtIixOQSwiMTExIiwiMTgzY20iLCIxOTQ3IixOQSwiIzAxNmY2YSINCiIyMDIwIiwiMTkzaW4iLCIyMDI2IiwiODIiLE5BLCIyMDM0IiwiI2I2NjUyYSIsImdybiINCiIxOTIyIiwiMTAxIiwiMjAxNSIsIjEzNiIsIjE1MWNtIiwiMjA0MCIsIjI0NWNiMyIsImx6ciINCiIyMDI4IiwiMTkzaW4iLCIyMDI1IiwiMzA4IiwiOTMzNTE1MzI4OSIsIjIwMjkiLCJ6IiwiZ3J5Ig0K" download="extract_0001.csv">
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

<a href="data:text/csv;base64,Iml5ciIsImhndCIsImJ5ciIsImNpZCIsInBpZCIsImV5ciIsImhjbCIsImVjbCINCiIyMDI1IiwiNjRjbSIsIjIwMjkiLCIyODEiLCIwNjcyODU5ODUiLCIxOTQ0IiwiI2NlYjNhMSIsIiMwNzIxOWEiDQoiMTkzMCIsIjE4MWNtIiwiMTk1MCIsTkEsIjYwNDk4MzQ2NiIsIjIwMzkiLCIjYjY2NTJhIiwiIzkwNjU0OCINCiIyMDI1IiwiMTQxIiwiMjAwNiIsIjMyNyIsIiMxYzQyY2MiLCIxOTU2IiwieiIsIiNmMmFmZmMiDQoiMTkyNCIsIjc0Y20iLCIyMDI0IiwiMTUzIiwiMzYxOTY0MDEiLCIxOTIxIiwiYTRlNGMwIiwiIzNhY2Y1NyINCiIxOTMzIiwiOTAiLCIyMDI1IixOQSwiODE5NDM0NzU0NCIsIjIwNDAiLE5BLCJkbmUiDQoiMTkyMCIsIjYwY20iLCIyMDI5IiwiMTA3IiwiIzU1Y2U2YiIsIjIwNDAiLCJkMzBmNmIiLCJkbmUiDQoiMTkzOCIsIjE3MmluIiwiMjAxOCIsIjMzOSIsIiM2YzEyMTYiLCIyMDM5IiwieiIsIiM1MTA2NzIiDQoiMTk5NyIsIjk2IiwiMjAwOSIsTkEsIjA2MzY5MTcyMjIiLCIyMDI2IiwieiIsImh6bCINCiIxOTUxIiwiMTkzaW4iLCIyMDE5IixOQSwiI2NiYzA4YyIsIjIwMDIiLCJ6IiwiIzNlOWYyZiINCk5BLCIxNzVpbiIsIjIwMTIiLCIyNDAiLCI1NzE0NzcxNzYiLCIxOTI5IiwiZjRlZjMyIiwidXRjIg0KIjIwMjYiLCI3MCIsIjIwMjIiLE5BLCI0MDQ2MjIwODMiLCIxOTc5IiwiYzFiYTdmIiwibHpyIg0KTkEsIjgyIiwiMjAxNyIsIjE1MyIsIjE3MjY2MTYxNyIsIjIwMzYiLCJ6IiwiI2FlNjA3ZCINCiIxOTQ1IiwiMTY1aW4iLCIyMDEwIiwiMjczIiwiNDM3MDc1MDQiLCIyMDI2IiwieiIsImdydCINCiIxOTQxIiwiMTYxY20iLCIxOTIzIiwiMjg1IiwiODA4MzkyMzE0IiwiMjAyMCIsIiNlZmNjOTgiLCJncnkiDQpOQSwiMTgxY20iLCIxOTkyIiwiMzIwIiwiMDMyNzY5NzU3IiwiMjAyMiIsIiM3MzM4MjAiLCJncm4iDQoiMjAyMSIsIjE4OGluIiwiMjAwOCIsTkEsIjU2OTU4MzUwOSIsTkEsImY3NDgyMyIsImh6bCINCk5BLCIxNjNpbiIsIjIwMjkiLE5BLE5BLCIxOTYyIiwiM2EwYzMwIiwiZ3J5Ig0KIjIwMjQiLCIxODAiLCIxOTI3IiwiODciLCI1OTcwOTg5NDAiLCIyMDI3IiwiIzYyM2EyZiIsIiM3ZWE3NzciDQoiMjAyNSIsIjE1MiIsIjIwMTIiLE5BLCIzODIxNzQ3NDQiLCIyMDI0IiwiIzg4ODc4NSIsIiM5NWQ4YTkiDQpOQSwiMTgzY20iLCIxOTM0IixOQSxOQSwiMjAyMyIsIiM2YjU0NDIiLCJncm4iDQoiMTk5NyIsIjY5Y20iLCIyMDEyIiwiNTQiLCIxODRjbSIsIjIwMzciLCJmOGVkNDUiLCJncnkiDQoiMTk1NSIsIjE2M2luIiwiMjAxOCIsIjI5OCIsIjg3MTU1MjY2IiwiMjAyMSIsIiNjMDk0NmYiLCIjMjE2OTIwIg0KTkEsIjE4MmNtIiwiMTk5MyIsIjExNyIsIjA3MzAzNTk5OSIsIjIwMzAiLCIjMzQxZTEzIixOQQ0KIjE5NDAiLCIxNzNjbSIsIjE5NDciLE5BLE5BLCIxOTQ3IiwiN2U1MTVjIiwiZ210Ig0KTkEsIjYzY20iLCIyMDA0IiwiMTg4IiwiNTYyMTIwMDEzNCIsIjE5MjQiLCI2ZWY5YmEiLCIjZWY2OGY1Ig0KIjIwMjEiLCIxNjdpbiIsIjIwMjUiLCIyMzQiLCIxODFjbSIsIjE5NDEiLCI5ODYxOWEiLCIjZjVlNjUxIg0KIjE5NDkiLCI3M2NtIiwiMTk4NSIsIjIwNyIsIiNlZTlmOTUiLCIyMDI4IiwieiIsInV0YyINCiIxOTMzIiwiNjljbSIsIjE5MzQiLE5BLCIxNzljbSIsIjIwMzAiLCJiOGUxNDIiLCJncm4iDQoiMjAyOCIsIjExMSIsIjIwMzAiLCIxODAiLCIxODMzOTE4NjEiLCIxOTU0IiwiMWZiMzBmIiwiIzBkMDE2MCINCk5BLCIxOTFjbSIsIjIwMjMiLE5BLCI3MjcwMjQ2NzYiLCIyMDI1IiwiI2I2NjUyYSIsIiMwYjNiMmQiDQpOQSxOQSwiMjAyMyIsIjI3MSIsIiM2NmVjODIiLCIyMDI5IiwiMTBkOWQ4IiwiIzM1M2UwZiINCiIxOTgxIiwiNTljbSIsIjIwMDkiLE5BLCIxNjJjbSIsIjIwMjUiLCIxMTY3NDIiLCJnbXQiDQoiMjAyMyIsIjY3Y20iLCIxOTYzIixOQSwiNjI4NTkzMzIiLCIyMDI4IiwiM2QxZjM0IiwiZG5lIg0KTkEsIjE5MWNtIiwiMTk4OSIsIjExOSIsIjU1NjAxMTQzNCIsIjIwMjUiLCIjODY2ODU3IiwiYW1iIg0KIjIwMDMiLCIxNTAiLCIyMDAxIiwiMjkyIiwiOTM0NzMxOTIiLCIxOTIyIiwieiIsIiNkNTZiYmQiDQoiMTkyOSIsIjE5MyIsTkEsIjMzMCIsIjMzNzY1NDI2IiwiMjAzOCIsInoiLCIjMThlODgzIg0KIjE5NDkiLCIxNjBjbSIsIjE5NTUiLE5BLCI3NDMwOTQzNDUiLCIyMDI3IiwiOGRhZTY3IiwiZ3J5Ig0KIjIwMjUiLCIxNjJjbSIsIjE5ODkiLCIzMTkiLCI2NzMxMTIyMiIsIjE5MzkiLCJ6IiwidXRjIg0KIjE5NjUiLCI2MCIsIjE5MzEiLE5BLCIjYWE1ZmQwIiwiMjAxNyIsIjU3OTI2NiIsImdyeSINCiIxOTk3IiwiNjFjbSIsIjIwMjMiLE5BLCIxNjJjbSIsIjE5ODciLCJ6IiwiIzlmNDU4YyINCk5BLCIxOTJjbSIsIjE5ODMiLCIyNDIiLCI4Mzk2MDg2MTYiLCIyMDI2IiwiI2NlYjNhMSIsImh6bCINCiIyMDI0IiwiMTgxaW4iLCIyMDI5IiwiOTciLCIjYzM4NjIwIiwiMTk3NiIsInoiLCJ6enoiDQoiMjAyNSIsIjU5aW4iLCIxOTIwIiwiMjAyIiwiMTgyY20iLCIyMDM1IiwieiIsIiM3OTlmMjkiDQpOQSwiNzNjbSIsIjE5MjciLE5BLCI3MTk3Mzg0NjgiLCIyMDM5IixOQSwiI2E4MmU5MCINCiIyMDIzIiwiMTYyY20iLCIyMDI1IiwiMzQzIiwiNTA0MjQwMzUiLCIyMDI0IiwiNjA1MjIzIiwib3RoIg0KIjIwMDEiLCIxNzFjbSIsIjIwMTMiLE5BLCI4OTAwOTY4MzI1IiwiMjAyMiIsIjY5OTExNiIsTkENCiIyMDA5IiwiMTcyaW4iLCIyMDA1IiwiMzM0IiwiMTg5Y20iLCIyMDMyIiwieiIsInhyeSINCiIxOTM0IiwiMTgwY20iLCIxOTQyIixOQSwiNDI3MDAxNTk3IiwiMjAzOCIsIiNhOTc4NDIiLCJicm4iDQpOQSwiMTg4Y20iLCIxOTg4IiwiMjY3IiwiNjk2NjE3MjMyIiwiMjAyOCIsIiMxODE3MWQiLCJhbWIiDQoiMjAyOSIsIjE4MGNtIiwiMjAyNyIsIjIwNSIsIjQ0MzgwOTMzNyIsIjE5ODAiLCIjMzQxZTEzIiwiZ3J0Ig0KIjE5ODIiLCIxNDQiLCIyMDEyIixOQSwiIzNiNDNjMSIsIjIwMzIiLCIyZjI2YWIiLCIjZjg5ZGYwIg0KIjIwMjIiLCIxNjBjbSIsIjE5ODgiLE5BLCI3ODg4MDUxNzkiLCIyMDIzIiwiIzg2Njg1NyIsImFtYiINCk5BLCIxNjRjbSIsIjE5OTYiLCIzMzgiLCIyMDg1OTYwMTQiLCIyMDI5IiwiI2VmY2M5OCIsImJsdSINCk5BLCIxOTJjbSIsIjE5NjAiLE5BLCIzNTc2ODAwNjQiLCIyMDI5IiwiI2MwOTQ2ZiIsImdyeSINCiIyMDI0IiwiMTY3aW4iLCIyMDA2IiwiMzQ2IiwiMTk1MzEzNDEiLCIyMDM1IiwieiIsIiNkMzIzMjAiDQoiMTk1OCIsIjE3NmNtIixOQSxOQSwiIzRjYjQ4MCIsIjIwMjYiLCJ6IiwiZG5lIg0KIjE5MjgiLCIxODVjbSIsIjE5ODQiLE5BLCIjYWM1YTkwIiwiMjAzMCIsImFjOGY0MyIsImJybiINCk5BLCIxMjgiLCIxOTk3IiwiMjk5IiwiOTg0Njc1MTk4IiwiMjAzNyIsIiNiNjY1MmEiLCJnbXQiDQoiMTk3MSIsIjE1N2luIiwiMTk3MCIsTkEsIjgxMjYxNzgwOSIsIjIwMjAiLCIjN2QzYjBjIiwiZ210Ig0KTkEsIjY2Y20iLCIxOTI4IiwiODciLCIyNzEwNzk0NiIsIjIwNDAiLE5BLCJ1dGMiDQoiMjAyOSIsIjY4IiwiMTk1OSIsTkEsIjkwMTc2MDk0OTciLCIyMDIzIiwiNGUwMjNiIiwiYmx1Ig0KIjIwMjEiLCI4MiIsIjIwMDciLCIxOTEiLCIjMWNmNjlmIiwiMjAzOSIsInoiLCJkbmUiDQoiMTk1MCIsIjY2Y20iLE5BLCIxMTEiLCIxODNjbSIsIjE5NDciLE5BLCIjMDE2ZjZhIg0KTkEsIjE4OWNtIiwiMTk3NCIsIjIxNCIsIjc4NTY4ODU0MiIsIjIwMzAiLCIjMTgxNzFkIiwiYnJuIg0KIjIwMjMiLE5BLCIxOTY2IixOQSwiMTY0Y20iLCIyMDIxIiwieiIsInV0YyINCiIxOTIyIiwiMTAxIiwiMjAxNSIsIjEzNiIsIjE1MWNtIiwiMjA0MCIsIjI0NWNiMyIsImx6ciINCiIyMDI4IiwiMTkzaW4iLCIyMDI1IiwiMzA4IiwiOTMzNTE1MzI4OSIsIjIwMjkiLCJ6IiwiZ3J5Ig0K" download="extract_0002.csv">
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

<a href="data:text/csv;base64,Iml5ciIsImhndCIsImJ5ciIsImNpZCIsInBpZCIsImV5ciIsImhjbCIsImVjbCINCiIyMDE1IiwiNTljbSIsIjIwMjkiLCIyMTkiLCI5MzgxNjg4NzUzIiwiMTk5MiIsIiNiNjY1MmEiLCIjN2EwZmE2Ig0KIjIwMjUiLCI2NGNtIiwiMjAyOSIsIjI4MSIsIjA2NzI4NTk4NSIsIjE5NDQiLCIjY2ViM2ExIiwiIzA3MjE5YSINCiIyMDEyIiwiMTYzY20iLCIxOTkwIiwiMjc1IiwiMjYwMDQzNTcwIixOQSwiI2I2NjUyYSIsImJybiINCiIxOTMwIiwiMTgxY20iLCIxOTUwIixOQSwiNjA0OTgzNDY2IiwiMjAzOSIsIiNiNjY1MmEiLCIjOTA2NTQ4Ig0KIjIwMjUiLCIxNDEiLCIyMDA2IiwiMzI3IiwiIzFjNDJjYyIsIjE5NTYiLCJ6IiwiI2YyYWZmYyINCiIxOTI0IiwiNzRjbSIsIjIwMjQiLCIxNTMiLCIzNjE5NjQwMSIsIjE5MjEiLCJhNGU0YzAiLCIjM2FjZjU3Ig0KIjE5MzMiLCI5MCIsIjIwMjUiLE5BLCI4MTk0MzQ3NTQ0IiwiMjA0MCIsTkEsImRuZSINCiIxOTIwIiwiNjBjbSIsIjIwMjkiLCIxMDciLCIjNTVjZTZiIiwiMjA0MCIsImQzMGY2YiIsImRuZSINCiIyMDE2IiwiMTcyY20iLE5BLE5BLCIjOThjYWVjIiwiMjAzNiIsInoiLCJkbmUiDQoiMTkzOCIsIjE3MmluIiwiMjAxOCIsIjMzOSIsIiM2YzEyMTYiLCIyMDM5IiwieiIsIiM1MTA2NzIiDQoiMjAxMSIsIjE3MGNtIiwiMTk4OSIsIjE1NSIsIjA3MTU4ODY4MiIsIjE5NTUiLCIjY2ViM2ExIiwiZ3JuIg0KIjIwMTciLCIxNzVjbSIsIjE5NjQiLCIyNjYiLE5BLE5BLCIjYTk3ODQyIiwiYnJuIg0KIjE5NTEiLCIxOTNpbiIsIjIwMTkiLE5BLCIjY2JjMDhjIiwiMjAwMiIsInoiLCIjM2U5ZjJmIg0KTkEsIjE3NWluIiwiMjAxMiIsIjI0MCIsIjU3MTQ3NzE3NiIsIjE5MjkiLCJmNGVmMzIiLCJ1dGMiDQoiMjAyNiIsIjcwIiwiMjAyMiIsTkEsIjQwNDYyMjA4MyIsIjE5NzkiLCJjMWJhN2YiLCJsenIiDQpOQSwiODIiLCIyMDE3IiwiMTUzIiwiMTcyNjYxNjE3IiwiMjAzNiIsInoiLCIjYWU2MDdkIg0KIjIwMjEiLCIxODhpbiIsIjIwMDgiLE5BLCI1Njk1ODM1MDkiLE5BLCJmNzQ4MjMiLCJoemwiDQpOQSwiMTYzaW4iLCIyMDI5IixOQSxOQSwiMTk2MiIsIjNhMGMzMCIsImdyeSINCiIxOTk3IiwiNjljbSIsIjIwMTIiLCI1NCIsIjE4NGNtIiwiMjAzNyIsImY4ZWQ0NSIsImdyeSINCiIyMDE3IiwiODMiLCIxOTQwIixOQSxOQSwiMjAzMiIsIjE3NDc3NCIsInhyeSINCiIxOTQwIiwiMTczY20iLCIxOTQ3IixOQSxOQSwiMTk0NyIsIjdlNTE1YyIsImdtdCINCiIyMDEyIiwiNjJjbSIsIjE5ODUiLCI1OSIsIiNhZjEwNmEiLCIxOTU3IiwiIzYyM2EyZiIsImdydCINCk5BLCI2M2NtIiwiMjAwNCIsIjE4OCIsIjU2MjEyMDAxMzQiLCIxOTI0IiwiNmVmOWJhIiwiI2VmNjhmNSINCiIyMDIxIiwiMTY3aW4iLCIyMDI1IiwiMjM0IiwiMTgxY20iLCIxOTQxIiwiOTg2MTlhIiwiI2Y1ZTY1MSINCiIyMDI4IiwiMTExIiwiMjAzMCIsIjE4MCIsIjE4MzM5MTg2MSIsIjE5NTQiLCIxZmIzMGYiLCIjMGQwMTYwIg0KIjIwMTYiLCIxMzUiLCIyMDEyIixOQSwiNzQ4Nzg2ODc3IiwiMjAxMSIsImI2ZTk2MiIsImdyeSINCiIyMDEzIiwiMTg1Y20iLCIyMDE0IixOQSwiODE2NDg1MDU0IiwiMjAxOSIsIiNlZmNjOTgiLCJncm4iDQoiMjAxMyIsIjE1N2luIiwiMjAyMyIsIjE4NiIsIjI3MTQ1MDc1NCIsIjIwMTYiLCJlMjA4ODIiLCJ1dGMiDQoiMjAxOCIsIjE5M2luIiwiMTk0MyIsTkEsIjcwMjIwMDAyNiIsIjE5NjgiLCIjODg4Nzg1IiwiZ210Ig0KIjIwMDMiLCIxNTAiLCIyMDAxIiwiMjkyIiwiOTM0NzMxOTIiLCIxOTIyIiwieiIsIiNkNTZiYmQiDQoiMTkyOSIsIjE5MyIsTkEsIjMzMCIsIjMzNzY1NDI2IiwiMjAzOCIsInoiLCIjMThlODgzIg0KIjIwMjUiLCIxNjJjbSIsIjE5ODkiLCIzMTkiLCI2NzMxMTIyMiIsIjE5MzkiLCJ6IiwidXRjIg0KIjIwMTkiLCIxNjljbSIsIjIwMTIiLCIyOTgiLCIwNjYwMzE2NTU4IiwiMTkyMCIsIiM4ODg3ODUiLCJ6enoiDQoiMTk2NSIsIjYwIiwiMTkzMSIsTkEsIiNhYTVmZDAiLCIyMDE3IiwiNTc5MjY2IiwiZ3J5Ig0KIjIwMTUiLCIxNThjbSIsIjE5MjgiLCIyMDciLCI0NDMyNDY3OTEiLE5BLCIjMTgxNzFkIiwiYnJuIg0KIjE5OTciLCI2MWNtIiwiMjAyMyIsTkEsIjE2MmNtIiwiMTk4NyIsInoiLCIjOWY0NThjIg0KIjIwMjAiLE5BLCIyMDEwIixOQSwiMDY1NjA3NjQ5IixOQSwiNTllMzc2IiwiYmx1Ig0KIjIwMTUiLCIxNTBjbSIsIjIwMjIiLE5BLCIxNjdjbSIsIjIwMzIiLCJjYWExNDUiLCIjMDY2NTBhIg0KIjIwMTciLE5BLCIxOTMyIiwiMjAzIiwiMTA1OTIxMDg1IixOQSwiIzQxOWQ3MyIsImdyeSINCiIyMDI0IiwiMTgxaW4iLCIyMDI5IiwiOTciLCIjYzM4NjIwIiwiMTk3NiIsInoiLCJ6enoiDQoiMjAyNSIsIjU5aW4iLCIxOTIwIiwiMjAyIiwiMTgyY20iLCIyMDM1IiwieiIsIiM3OTlmMjkiDQoiMjAxMyIsTkEsIjE5NTEiLCIyNTciLCIxMjMwNjU2MzkiLE5BLCIjN2QzYjBjIiwiZ3J5Ig0KTkEsIjczY20iLCIxOTI3IixOQSwiNzE5NzM4NDY4IiwiMjAzOSIsTkEsIiNhODJlOTAiDQoiMjAwOSIsIjE3MmluIiwiMjAwNSIsIjMzNCIsIjE4OWNtIiwiMjAzMiIsInoiLCJ4cnkiDQoiMjAxMCIsIjE2NyIsIjIwMDMiLCIxNjkiLE5BLCIyMDM3IiwiNDg2ODAwIiwiIzI5YmRkNiINCiIxOTM0IiwiMTgwY20iLCIxOTQyIixOQSwiNDI3MDAxNTk3IiwiMjAzOCIsIiNhOTc4NDIiLCJicm4iDQoiMjAxNiIsIjE1MWNtIiwiMTk0OCIsIjMyNyIsIjM0MTk3ODgyMiIsTkEsIiM3MzM4MjAiLCJvdGgiDQoiMjAyOSIsIjE4MGNtIiwiMjAyNyIsIjIwNSIsIjQ0MzgwOTMzNyIsIjE5ODAiLCIjMzQxZTEzIiwiZ3J0Ig0KIjE5ODIiLCIxNDQiLCIyMDEyIixOQSwiIzNiNDNjMSIsIjIwMzIiLCIyZjI2YWIiLCIjZjg5ZGYwIg0KIjIwMjAiLCIxODVpbiIsIjIwMjgiLE5BLCI3NzM3Mzk3NDQiLCIxOTg2IiwieiIsImRuZSINCiIyMDI0IiwiMTY3aW4iLCIyMDA2IiwiMzQ2IiwiMTk1MzEzNDEiLCIyMDM1IiwieiIsIiNkMzIzMjAiDQoiMjAxMyIsIjc0Y20iLCIyMDA3IiwiMzE0IiwiMTg2Y20iLCIxOTczIiwiMTgwZTBjIiwiaHpsIg0KIjIwMTAiLCIxODRjbSIsIjE5ODUiLE5BLCIjMTc1MTI5IiwiMjAzMiIsIiNmZmZmZmQiLCJoemwiDQoiMjAyMCIsIjcwY20iLCIyMDA0IiwiMTY2IixOQSwiMjA0MCIsIiM3MzM4MjAiLCJsenIiDQpOQSwiMTI4IiwiMTk5NyIsIjI5OSIsIjk4NDY3NTE5OCIsIjIwMzciLCIjYjY2NTJhIiwiZ210Ig0KIjIwMTEiLCIxNzZjbSIsIjIwMTIiLCI2NCIsIjIxMzIxMzM1OSIsIjE5NzEiLCJiZTdiMTMiLCJnbXQiDQpOQSwiNjZjbSIsIjE5MjgiLCI4NyIsIjI3MTA3OTQ2IiwiMjA0MCIsTkEsInV0YyINCiIyMDE4IiwiMTUwaW4iLCIxOTM0IiwiMTYzIiwiMjg2MzI4NDc1NCIsIjE5NzciLCIjNjIzYTJmIiwiYnJuIg0KIjIwMTAiLCIxODZjbSIsIjE5NDEiLCIxNDUiLCI3MjIwNTYxMzkiLCIyMDMxIiwiI2NmYTA3ZCIsImh6bCINCiIyMDIxIiwiODIiLCIyMDA3IiwiMTkxIiwiIzFjZjY5ZiIsIjIwMzkiLCJ6IiwiZG5lIg0KIjE5NTAiLCI2NmNtIixOQSwiMTExIiwiMTgzY20iLCIxOTQ3IixOQSwiIzAxNmY2YSINCiIyMDIwIiwiMTYzY20iLCIxOTY3IiwiMzA5IiwiMTI4NTU4NDI5MyIsIjIwMzEiLCIjNzMzODIwIiwib3RoIg0KIjIwMTQiLCIxNjljbSIsIjE5NjYiLE5BLCI2MjE4NzY1MzIiLE5BLCIjZWZjYzk4IiwiZ3J5Ig0KIjIwMTIiLCIxODBjbSIsIjE5NDMiLCIxNDQiLCIxNTVjbSIsIjIwMzUiLE5BLCJhbWIiDQoiMjAyMCIsIjE5M2luIiwiMjAyNiIsIjgyIixOQSwiMjAzNCIsIiNiNjY1MmEiLCJncm4iDQoiMTkyMiIsIjEwMSIsIjIwMTUiLCIxMzYiLCIxNTFjbSIsIjIwNDAiLCIyNDVjYjMiLCJsenIiDQo=" download="extract_0003.csv">
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

<p style="margin-top: 0px; margin-bottom: 0px; font-family: monospace; white-space: nowrap; text-overflow: ellipsis; overflow: hidden;">
<code>^\#(?:\[0-9a-fA-F\]{6})$</code>
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
<code>202</code><br><code>0.73</code>
</td>
<td class="gt_row gt_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">
<code>74</code><br><code>0.27</code>
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

<a href="data:text/csv;base64,Iml5ciIsImhndCIsImJ5ciIsImNpZCIsInBpZCIsImV5ciIsImhjbCIsImVjbCINCiIyMDE4IixOQSwiMTk2OCIsIjMzNSIsIjk0MzYxNDc1NSIsIjIwMjYiLE5BLCJibHUiDQoiMjAyNSIsIjE0MSIsIjIwMDYiLCIzMjciLCIjMWM0MmNjIiwiMTk1NiIsInoiLCIjZjJhZmZjIg0KIjE5MjQiLCI3NGNtIiwiMjAyNCIsIjE1MyIsIjM2MTk2NDAxIiwiMTkyMSIsImE0ZTRjMCIsIiMzYWNmNTciDQoiMjAyMCIsIjE1MSIsIjIwMTIiLCIzMTQiLCI5ODQzNjQ4NjIiLCIyMDIzIiwieiIsImRuZSINCiIxOTMzIiwiOTAiLCIyMDI1IixOQSwiODE5NDM0NzU0NCIsIjIwNDAiLE5BLCJkbmUiDQoiMTkyMCIsIjYwY20iLCIyMDI5IiwiMTA3IiwiIzU1Y2U2YiIsIjIwNDAiLCJkMzBmNmIiLCJkbmUiDQoiMjAxOCIsIjY1Y20iLCIxOTM3IixOQSwiIzRiZmYzZSIsIjIwMjciLCI0M2ZhZmIiLCJncnQiDQoiMjAxNiIsIjE2N2NtIiwiMTk2MyIsIjUyIiwiOTU1MjAzNzgxIiwiMjAyNiIsTkEsImdybiINCiIyMDE2IiwiMTcyY20iLE5BLE5BLCIjOThjYWVjIiwiMjAzNiIsInoiLCJkbmUiDQoiMTkzOCIsIjE3MmluIiwiMjAxOCIsIjMzOSIsIiM2YzEyMTYiLCIyMDM5IiwieiIsIiM1MTA2NzIiDQoiMTk5NyIsIjk2IiwiMjAwOSIsTkEsIjA2MzY5MTcyMjIiLCIyMDI2IiwieiIsImh6bCINCiIxOTUxIiwiMTkzaW4iLCIyMDE5IixOQSwiI2NiYzA4YyIsIjIwMDIiLCJ6IiwiIzNlOWYyZiINCk5BLCIxNzVpbiIsIjIwMTIiLCIyNDAiLCI1NzE0NzcxNzYiLCIxOTI5IiwiZjRlZjMyIiwidXRjIg0KIjIwMjYiLCI3MCIsIjIwMjIiLE5BLCI0MDQ2MjIwODMiLCIxOTc5IiwiYzFiYTdmIiwibHpyIg0KIjIwMTQiLCIxNTNjbSIsIjE5NTAiLE5BLCI0MTEwMzI2NTkiLCIyMDIwIixOQSwiaHpsIg0KIjIwMTgiLCIxNTZjbSIsIjIwMTciLCIxNzkiLCIyOTgzNjEyNCIsIjIwMjMiLCI1NmRlODMiLCJ6enoiDQpOQSwiODIiLCIyMDE3IiwiMTUzIiwiMTcyNjYxNjE3IiwiMjAzNiIsInoiLCIjYWU2MDdkIg0KIjE5NDUiLCIxNjVpbiIsIjIwMTAiLCIyNzMiLCI0MzcwNzUwNCIsIjIwMjYiLCJ6IiwiZ3J0Ig0KIjIwMjEiLCIxODhpbiIsIjIwMDgiLE5BLCI1Njk1ODM1MDkiLE5BLCJmNzQ4MjMiLCJoemwiDQoiMjAxNiIsIjY3Y20iLCIyMDEwIiwiMTQ0IiwiMzQ5NDkyMjQzIiwiMjAyMSIsInoiLCIjMjRjZWVlIg0KTkEsIjE2M2luIiwiMjAyOSIsTkEsTkEsIjE5NjIiLCIzYTBjMzAiLCJncnkiDQoiMTk5NyIsIjY5Y20iLCIyMDEyIiwiNTQiLCIxODRjbSIsIjIwMzciLCJmOGVkNDUiLCJncnkiDQoiMjAxNyIsIjgzIiwiMTk0MCIsTkEsTkEsIjIwMzIiLCIxNzQ3NzQiLCJ4cnkiDQoiMTk0MCIsIjE3M2NtIiwiMTk0NyIsTkEsTkEsIjE5NDciLCI3ZTUxNWMiLCJnbXQiDQpOQSwiNjNjbSIsIjIwMDQiLCIxODgiLCI1NjIxMjAwMTM0IiwiMTkyNCIsIjZlZjliYSIsIiNlZjY4ZjUiDQoiMjAyMSIsIjE2N2luIiwiMjAyNSIsIjIzNCIsIjE4MWNtIiwiMTk0MSIsIjk4NjE5YSIsIiNmNWU2NTEiDQoiMTk0OSIsIjczY20iLCIxOTg1IiwiMjA3IiwiI2VlOWY5NSIsIjIwMjgiLCJ6IiwidXRjIg0KIjE5MzMiLCI2OWNtIiwiMTkzNCIsTkEsIjE3OWNtIiwiMjAzMCIsImI4ZTE0MiIsImdybiINCiIyMDI4IiwiMTExIiwiMjAzMCIsIjE4MCIsIjE4MzM5MTg2MSIsIjE5NTQiLCIxZmIzMGYiLCIjMGQwMTYwIg0KIjIwMTYiLCIxMzUiLCIyMDEyIixOQSwiNzQ4Nzg2ODc3IiwiMjAxMSIsImI2ZTk2MiIsImdyeSINCk5BLE5BLCIyMDIzIiwiMjcxIiwiIzY2ZWM4MiIsIjIwMjkiLCIxMGQ5ZDgiLCIjMzUzZTBmIg0KIjE5ODEiLCI1OWNtIiwiMjAwOSIsTkEsIjE2MmNtIiwiMjAyNSIsIjExNjc0MiIsImdtdCINCiIyMDIzIiwiNjdjbSIsIjE5NjMiLE5BLCI2Mjg1OTMzMiIsIjIwMjgiLCIzZDFmMzQiLCJkbmUiDQoiMjAxMyIsIjE1N2luIiwiMjAyMyIsIjE4NiIsIjI3MTQ1MDc1NCIsIjIwMTYiLCJlMjA4ODIiLCJ1dGMiDQoiMjAwMyIsIjE1MCIsIjIwMDEiLCIyOTIiLCI5MzQ3MzE5MiIsIjE5MjIiLCJ6IiwiI2Q1NmJiZCINCiIxOTI5IiwiMTkzIixOQSwiMzMwIiwiMzM3NjU0MjYiLCIyMDM4IiwieiIsIiMxOGU4ODMiDQoiMTk0OSIsIjE2MGNtIiwiMTk1NSIsTkEsIjc0MzA5NDM0NSIsIjIwMjciLCI4ZGFlNjciLCJncnkiDQoiMjAyNSIsIjE2MmNtIiwiMTk4OSIsIjMxOSIsIjY3MzExMjIyIiwiMTkzOSIsInoiLCJ1dGMiDQoiMjAxNCIsIjE3MWNtIiwiMTk5NyIsIjMwMiIsIjEwMTM2MzM2NyIsIjIwMjUiLCJ6IiwiYW1iIg0KIjIwMTAiLCIxNjRjbSIsIjE5NDciLCI5NiIsIjE2NjExNTQ0MiIsIjIwMzAiLCI0YmMyMGEiLCJvdGgiDQoiMTk2NSIsIjYwIiwiMTkzMSIsTkEsIiNhYTVmZDAiLCIyMDE3IiwiNTc5MjY2IiwiZ3J5Ig0KIjE5OTciLCI2MWNtIiwiMjAyMyIsTkEsIjE2MmNtIiwiMTk4NyIsInoiLCIjOWY0NThjIg0KIjIwMjAiLE5BLCIyMDEwIixOQSwiMDY1NjA3NjQ5IixOQSwiNTllMzc2IiwiYmx1Ig0KIjIwMTUiLCIxNTBjbSIsIjIwMjIiLE5BLCIxNjdjbSIsIjIwMzIiLCJjYWExNDUiLCIjMDY2NTBhIg0KIjIwMTAiLCIxNzljbSIsIjE5NDAiLCIxNTMiLCI3NDA2OTIzMjEiLCIyMDI3IixOQSwiYmx1Ig0KIjIwMjQiLCIxODFpbiIsIjIwMjkiLCI5NyIsIiNjMzg2MjAiLCIxOTc2IiwieiIsInp6eiINCiIyMDE4IiwiMTYzY20iLCIxOTk5IiwiMjA5IiwiNDAxNjA2NTcxIiwiMjAyMyIsIjZmMjlhNiIsImx6ciINCiIyMDE0IiwiMTcyaW4iLCIxOTUwIixOQSwiMTg3Y20iLCIyMDI4IiwieiIsImJybiINCiIyMDI1IiwiNTlpbiIsIjE5MjAiLCIyMDIiLCIxODJjbSIsIjIwMzUiLCJ6IiwiIzc5OWYyOSINCk5BLCI3M2NtIiwiMTkyNyIsTkEsIjcxOTczODQ2OCIsIjIwMzkiLE5BLCIjYTgyZTkwIg0KIjIwMjMiLCIxNjJjbSIsIjIwMjUiLCIzNDMiLCI1MDQyNDAzNSIsIjIwMjQiLCI2MDUyMjMiLCJvdGgiDQoiMjAwMSIsIjE3MWNtIiwiMjAxMyIsTkEsIjg5MDA5NjgzMjUiLCIyMDIyIiwiNjk5MTE2IixOQQ0KIjIwMDkiLCIxNzJpbiIsIjIwMDUiLCIzMzQiLCIxODljbSIsIjIwMzIiLCJ6IiwieHJ5Ig0KIjIwMjAiLCIxNTljbSIsTkEsTkEsIjE2NmNtIiwiMjAyNiIsInoiLCJvdGgiDQoiMjAxMCIsIjE2NyIsIjIwMDMiLCIxNjkiLE5BLCIyMDM3IiwiNDg2ODAwIiwiIzI5YmRkNiINCiIxOTgyIiwiMTQ0IiwiMjAxMiIsTkEsIiMzYjQzYzEiLCIyMDMyIiwiMmYyNmFiIiwiI2Y4OWRmMCINCiIyMDIwIiwiMTc5Y20iLCIxOTQwIixOQSwiNDM3ODIwMjU0IiwiMjAyNiIsTkEsImdyeSINCiIyMDIwIiwiMTg1aW4iLCIyMDI4IixOQSwiNzczNzM5NzQ0IiwiMTk4NiIsInoiLCJkbmUiDQoiMjAyNCIsIjE2N2luIiwiMjAwNiIsIjM0NiIsIjE5NTMxMzQxIiwiMjAzNSIsInoiLCIjZDMyMzIwIg0KIjIwMTEiLCIxNjJjbSIsIjE5NjEiLCI1NCIsIjg5MTM5Nzk4MiIsIjIwMjAiLE5BLCJicm4iDQoiMjAxNSIsIjE1MmNtIiwiMjAxOSIsTkEsIjE4MmNtIiwiMjAyOCIsIjQzZDU2ZCIsInp6eiINCiIxOTU4IiwiMTc2Y20iLE5BLE5BLCIjNGNiNDgwIiwiMjAyNiIsInoiLCJkbmUiDQoiMjAxMyIsIjc0Y20iLCIyMDA3IiwiMzE0IiwiMTg2Y20iLCIxOTczIiwiMTgwZTBjIiwiaHpsIg0KIjIwMTciLCIxNjZjbSIsIjIwMTIiLCIxNzIiLCIjNDI0YWU0IiwiMjAyMiIsImIxMzE5YiIsIiM2NjM1ZDgiDQoiMTkyOCIsIjE4NWNtIiwiMTk4NCIsTkEsIiNhYzVhOTAiLCIyMDMwIiwiYWM4ZjQzIiwiYnJuIg0KIjIwMTEiLCIxNzZjbSIsIjIwMTIiLCI2NCIsIjIxMzIxMzM1OSIsIjE5NzEiLCJiZTdiMTMiLCJnbXQiDQpOQSwiNjZjbSIsIjE5MjgiLCI4NyIsIjI3MTA3OTQ2IiwiMjA0MCIsTkEsInV0YyINCiIyMDI5IiwiNjgiLCIxOTU5IixOQSwiOTAxNzYwOTQ5NyIsIjIwMjMiLCI0ZTAyM2IiLCJibHUiDQoiMjAyMSIsIjgyIiwiMjAwNyIsIjE5MSIsIiMxY2Y2OWYiLCIyMDM5IiwieiIsImRuZSINCiIxOTUwIiwiNjZjbSIsTkEsIjExMSIsIjE4M2NtIiwiMTk0NyIsTkEsIiMwMTZmNmEiDQoiMjAyMyIsTkEsIjE5NjYiLE5BLCIxNjRjbSIsIjIwMjEiLCJ6IiwidXRjIg0KIjIwMTIiLCIxODBjbSIsIjE5NDMiLCIxNDQiLCIxNTVjbSIsIjIwMzUiLE5BLCJhbWIiDQoiMTkyMiIsIjEwMSIsIjIwMTUiLCIxMzYiLCIxNTFjbSIsIjIwNDAiLCIyNDVjYjMiLCJsenIiDQoiMjAyOCIsIjE5M2luIiwiMjAyNSIsIjMwOCIsIjkzMzUxNTMyODkiLCIyMDI5IiwieiIsImdyeSINCg==" download="extract_0004.csv">
<button aria-label="There are 74 &#39;fail&#39; rows available as a CSV file." data-balloon-pos="left" style="background-color:#67C2DC;color:#FFFFFF;border:none;padding:5px;font-weight:bold;cursor:pointer;border-radius:4px;">CSV</button>
</a>

</td>
</tr>
<tr>
<td class="gt_row gt_left" style="background-color: rgba(76,166,76,0.5); height:  40px">
</td>
<td class="gt_row gt_right" style="color: #666666; font-size: 13px; font-weight: bold; height:  40px">
5
</td>
<td class="gt_row gt_left" style="height:  40px">

<svg width="30px" height="30px" viewBox="0 0 67 67" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<defs>
<path d="M10.712234,0 L56.712234,0 C62.2350815,-1.01453063e-15 66.712234,4.4771525 66.712234,10 L66.712234,66 L10.712234,66 C5.18938647,66 0.712233968,61.5228475 0.712233968,56 L0.712233968,10 C0.712233968,4.4771525 5.18938647,1.01453063e-15 10.712234,0 Z" id="path-1"></path>
</defs>
<g id="pointblank" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
<g id="col_vals_in_set" transform="translate(-0.487938, 0.859210)">
<g id="rectangle">
<use fill="#FFFFFF" fill-rule="evenodd" xlink:href="#path-1"></use>
<path stroke="#000000" stroke-width="2" d="M65.712234,65 L65.712234,10 C65.712234,5.02943725 61.6827967,1 56.712234,1 L10.712234,1 C5.74167122,1 1.71223397,5.02943725 1.71223397,10 L1.71223397,56 C1.71223397,60.9705627 5.74167122,65 10.712234,65 L65.712234,65 Z"></path>
</g>
<path d="M44.127969,41.1538382 L31.0814568,41.1538382 C29.9510748,41.1536429 28.8827052,40.9256134 27.9079888,40.5136953 C26.4467442,39.8960136 25.19849,38.8599685 24.3189894,37.5577099 C23.8792391,36.906727 23.5314818,36.1899233 23.2936866,35.4252675 C23.2130217,35.16589 23.1460289,34.9005554 23.0913409,34.6307286 L44.1278714,34.6307286 C45.028466,34.6306309 45.7586488,33.9004481 45.7586488,32.9998535 C45.7586488,32.0992589 45.028466,31.3690761 44.1278714,31.3690761 L23.0905596,31.3690761 C23.1990567,30.8337194 23.3597028,30.3180894 23.5675173,29.8264831 C24.185199,28.3652386 25.2212442,27.1169844 26.5236004,26.2374838 C27.1745833,25.7977334 27.891387,25.4499762 28.6560428,25.2122786 C29.4208939,24.9744833 30.2334994,24.8459665 31.0813591,24.8459665 L44.1277737,24.8459665 C45.0283683,24.8459665 45.7585511,24.1157837 45.7585511,23.2151891 C45.7585511,22.3145945 45.0283683,21.5844117 44.1277737,21.5844117 L31.0813591,21.5844117 C29.5096643,21.5844117 28.0039858,21.9038483 26.6373711,22.4820765 C24.5866678,23.3498583 22.8469049,24.7950871 21.6163267,26.616296 C20.3856508,28.4362354 19.665136,30.6413347 19.6658196,33.0000488 C19.6656243,34.5717436 19.9852563,36.0774222 20.5635822,37.4440369 C21.4312663,39.4947402 22.8765927,41.2345031 24.697704,42.4650813 C26.5176434,43.6957572 28.7227427,44.4155883 31.0814568,44.4155883 L44.1278714,44.4155883 C45.028466,44.4155883 45.7586488,43.6854055 45.7586488,42.7848109 C45.7586488,41.8842163 45.0285636,41.1538382 44.127969,41.1538382 Z" id="set_of" fill="#000000" fill-rule="nonzero"></path>
</g> </g>
</svg>

<code style="font-size:11px;"> col\_vals\_in\_set()</code>

</td>
<td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">

<p style="margin-top:0;margin-bottom:0;font-size:11px;white-space:nowrap;text-overflow:ellipsis;overflow:hidden;line-height:2em;">
<code><span style="color:purple;">▮</span>ecl</code>
</p>

</td>
<td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">

<p style="margin-top: 0px; margin-bottom: 0px; font-family: monospace; white-space: nowrap; text-overflow: ellipsis; overflow: hidden;">
<code>amb, blu, brn, gry, grn, hzl, oth</code>
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
<code>204</code><br><code>0.74</code>
</td>
<td class="gt_row gt_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">
<code>72</code><br><code>0.26</code>
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

<a href="data:text/csv;base64,Iml5ciIsImhndCIsImJ5ciIsImNpZCIsInBpZCIsImV5ciIsImhjbCIsImVjbCINCiIyMDE1IiwiNTljbSIsIjIwMjkiLCIyMTkiLCI5MzgxNjg4NzUzIiwiMTk5MiIsIiNiNjY1MmEiLCIjN2EwZmE2Ig0KIjIwMjUiLCI2NGNtIiwiMjAyOSIsIjI4MSIsIjA2NzI4NTk4NSIsIjE5NDQiLCIjY2ViM2ExIiwiIzA3MjE5YSINCiIxOTMwIiwiMTgxY20iLCIxOTUwIixOQSwiNjA0OTgzNDY2IiwiMjAzOSIsIiNiNjY1MmEiLCIjOTA2NTQ4Ig0KIjIwMjUiLCIxNDEiLCIyMDA2IiwiMzI3IiwiIzFjNDJjYyIsIjE5NTYiLCJ6IiwiI2YyYWZmYyINCiIxOTI0IiwiNzRjbSIsIjIwMjQiLCIxNTMiLCIzNjE5NjQwMSIsIjE5MjEiLCJhNGU0YzAiLCIjM2FjZjU3Ig0KIjIwMjAiLCIxNTEiLCIyMDEyIiwiMzE0IiwiOTg0MzY0ODYyIiwiMjAyMyIsInoiLCJkbmUiDQoiMTkzMyIsIjkwIiwiMjAyNSIsTkEsIjgxOTQzNDc1NDQiLCIyMDQwIixOQSwiZG5lIg0KIjE5MjAiLCI2MGNtIiwiMjAyOSIsIjEwNyIsIiM1NWNlNmIiLCIyMDQwIiwiZDMwZjZiIiwiZG5lIg0KIjIwMTgiLCI2NWNtIiwiMTkzNyIsTkEsIiM0YmZmM2UiLCIyMDI3IiwiNDNmYWZiIiwiZ3J0Ig0KIjIwMTQiLCIxNjZjbSIsIjE5MjQiLCIxNjMiLE5BLCIyMDI0IiwiIzE4MTcxZCIsTkENCiIyMDE2IiwiMTcyY20iLE5BLE5BLCIjOThjYWVjIiwiMjAzNiIsInoiLCJkbmUiDQoiMTkzOCIsIjE3MmluIiwiMjAxOCIsIjMzOSIsIiM2YzEyMTYiLCIyMDM5IiwieiIsIiM1MTA2NzIiDQoiMTk1MSIsIjE5M2luIiwiMjAxOSIsTkEsIiNjYmMwOGMiLCIyMDAyIiwieiIsIiMzZTlmMmYiDQpOQSwiMTc1aW4iLCIyMDEyIiwiMjQwIiwiNTcxNDc3MTc2IiwiMTkyOSIsImY0ZWYzMiIsInV0YyINCiIyMDI2IiwiNzAiLCIyMDIyIixOQSwiNDA0NjIyMDgzIiwiMTk3OSIsImMxYmE3ZiIsImx6ciINCiIyMDE4IiwiMTU2Y20iLCIyMDE3IiwiMTc5IiwiMjk4MzYxMjQiLCIyMDIzIiwiNTZkZTgzIiwienp6Ig0KIjIwMTQiLCIxOTBjbSIsIjE5OTgiLE5BLCI1NjU1MjQ1NzQiLCIyMDIwIiwiIzg2Njg1NyIsTkENCk5BLCI4MiIsIjIwMTciLCIxNTMiLCIxNzI2NjE2MTciLCIyMDM2IiwieiIsIiNhZTYwN2QiDQoiMTk0NSIsIjE2NWluIiwiMjAxMCIsIjI3MyIsIjQzNzA3NTA0IiwiMjAyNiIsInoiLCJncnQiDQoiMjAxNiIsIjY3Y20iLCIyMDEwIiwiMTQ0IiwiMzQ5NDkyMjQzIiwiMjAyMSIsInoiLCIjMjRjZWVlIg0KIjIwMjQiLCIxODAiLCIxOTI3IiwiODciLCI1OTcwOTg5NDAiLCIyMDI3IiwiIzYyM2EyZiIsIiM3ZWE3NzciDQoiMjAyNSIsIjE1MiIsIjIwMTIiLE5BLCIzODIxNzQ3NDQiLCIyMDI0IiwiIzg4ODc4NSIsIiM5NWQ4YTkiDQoiMTk1NSIsIjE2M2luIiwiMjAxOCIsIjI5OCIsIjg3MTU1MjY2IiwiMjAyMSIsIiNjMDk0NmYiLCIjMjE2OTIwIg0KTkEsIjE4MmNtIiwiMTk5MyIsIjExNyIsIjA3MzAzNTk5OSIsIjIwMzAiLCIjMzQxZTEzIixOQQ0KIjIwMTciLCI4MyIsIjE5NDAiLE5BLE5BLCIyMDMyIiwiMTc0Nzc0IiwieHJ5Ig0KIjE5NDAiLCIxNzNjbSIsIjE5NDciLE5BLE5BLCIxOTQ3IiwiN2U1MTVjIiwiZ210Ig0KIjIwMTIiLCI2MmNtIiwiMTk4NSIsIjU5IiwiI2FmMTA2YSIsIjE5NTciLCIjNjIzYTJmIiwiZ3J0Ig0KTkEsIjYzY20iLCIyMDA0IiwiMTg4IiwiNTYyMTIwMDEzNCIsIjE5MjQiLCI2ZWY5YmEiLCIjZWY2OGY1Ig0KIjIwMTkiLCIxNzFjbSIsIjE5NzYiLE5BLCIwNDE5MjYzNTQiLCIyMDIwIiwiI2E5Nzg0MiIsTkENCiIyMDIxIiwiMTY3aW4iLCIyMDI1IiwiMjM0IiwiMTgxY20iLCIxOTQxIiwiOTg2MTlhIiwiI2Y1ZTY1MSINCiIxOTQ5IiwiNzNjbSIsIjE5ODUiLCIyMDciLCIjZWU5Zjk1IiwiMjAyOCIsInoiLCJ1dGMiDQoiMjAyOCIsIjExMSIsIjIwMzAiLCIxODAiLCIxODMzOTE4NjEiLCIxOTU0IiwiMWZiMzBmIiwiIzBkMDE2MCINCk5BLCIxOTFjbSIsIjIwMjMiLE5BLCI3MjcwMjQ2NzYiLCIyMDI1IiwiI2I2NjUyYSIsIiMwYjNiMmQiDQpOQSxOQSwiMjAyMyIsIjI3MSIsIiM2NmVjODIiLCIyMDI5IiwiMTBkOWQ4IiwiIzM1M2UwZiINCiIxOTgxIiwiNTljbSIsIjIwMDkiLE5BLCIxNjJjbSIsIjIwMjUiLCIxMTY3NDIiLCJnbXQiDQoiMjAyMyIsIjY3Y20iLCIxOTYzIixOQSwiNjI4NTkzMzIiLCIyMDI4IiwiM2QxZjM0IiwiZG5lIg0KIjIwMTMiLCIxNTdpbiIsIjIwMjMiLCIxODYiLCIyNzE0NTA3NTQiLCIyMDE2IiwiZTIwODgyIiwidXRjIg0KIjIwMTgiLCIxOTNpbiIsIjE5NDMiLE5BLCI3MDIyMDAwMjYiLCIxOTY4IiwiIzg4ODc4NSIsImdtdCINCiIyMDE4IiwiMTg2Y20iLE5BLE5BLCIxNzg1MjUxMzIiLCIyMDIzIiwiIzg4ODc4NSIsTkENCiIyMDAzIiwiMTUwIiwiMjAwMSIsIjI5MiIsIjkzNDczMTkyIiwiMTkyMiIsInoiLCIjZDU2YmJkIg0KIjE5MjkiLCIxOTMiLE5BLCIzMzAiLCIzMzc2NTQyNiIsIjIwMzgiLCJ6IiwiIzE4ZTg4MyINCiIyMDI1IiwiMTYyY20iLCIxOTg5IiwiMzE5IiwiNjczMTEyMjIiLCIxOTM5IiwieiIsInV0YyINCiIyMDE5IiwiMTY5Y20iLCIyMDEyIiwiMjk4IiwiMDY2MDMxNjU1OCIsIjE5MjAiLCIjODg4Nzg1Iiwienp6Ig0KIjE5OTciLCI2MWNtIiwiMjAyMyIsTkEsIjE2MmNtIiwiMTk4NyIsInoiLCIjOWY0NThjIg0KIjIwMTUiLCIxNTBjbSIsIjIwMjIiLE5BLCIxNjdjbSIsIjIwMzIiLCJjYWExNDUiLCIjMDY2NTBhIg0KIjIwMjQiLCIxODFpbiIsIjIwMjkiLCI5NyIsIiNjMzg2MjAiLCIxOTc2IiwieiIsInp6eiINCiIyMDE4IiwiMTYzY20iLCIxOTk5IiwiMjA5IiwiNDAxNjA2NTcxIiwiMjAyMyIsIjZmMjlhNiIsImx6ciINCiIyMDI1IiwiNTlpbiIsIjE5MjAiLCIyMDIiLCIxODJjbSIsIjIwMzUiLCJ6IiwiIzc5OWYyOSINCk5BLCI3M2NtIiwiMTkyNyIsTkEsIjcxOTczODQ2OCIsIjIwMzkiLE5BLCIjYTgyZTkwIg0KIjIwMDEiLCIxNzFjbSIsIjIwMTMiLE5BLCI4OTAwOTY4MzI1IiwiMjAyMiIsIjY5OTExNiIsTkENCiIyMDA5IiwiMTcyaW4iLCIyMDA1IiwiMzM0IiwiMTg5Y20iLCIyMDMyIiwieiIsInhyeSINCiIyMDEwIiwiMTY3IiwiMjAwMyIsIjE2OSIsTkEsIjIwMzciLCI0ODY4MDAiLCIjMjliZGQ2Ig0KIjIwMTgiLCIxNzBjbSIsIjE5NDAiLCIyNzMiLCI1ODgxNDI3NzEiLCIyMDIyIiwiIzczMzgyMCIsIiNhNjA4ZmUiDQoiMjAyOSIsIjE4MGNtIiwiMjAyNyIsIjIwNSIsIjQ0MzgwOTMzNyIsIjE5ODAiLCIjMzQxZTEzIiwiZ3J0Ig0KIjE5ODIiLCIxNDQiLCIyMDEyIixOQSwiIzNiNDNjMSIsIjIwMzIiLCIyZjI2YWIiLCIjZjg5ZGYwIg0KIjIwMjAiLCIxODVpbiIsIjIwMjgiLE5BLCI3NzM3Mzk3NDQiLCIxOTg2IiwieiIsImRuZSINCiIyMDE2IiwiMTg2Y20iLCIxOTQ3IiwiNjQiLE5BLCIyMDIxIiwiI2E5Nzg0MiIsTkENCiIyMDI0IiwiMTY3aW4iLCIyMDA2IiwiMzQ2IiwiMTk1MzEzNDEiLCIyMDM1IiwieiIsIiNkMzIzMjAiDQoiMjAxNSIsIjE1MmNtIiwiMjAxOSIsTkEsIjE4MmNtIiwiMjAyOCIsIjQzZDU2ZCIsInp6eiINCiIxOTU4IiwiMTc2Y20iLE5BLE5BLCIjNGNiNDgwIiwiMjAyNiIsInoiLCJkbmUiDQoiMjAxOCIsIjE2OGNtIiwiMTkzMiIsIjE2NSIsIjc0NTg2NzMzNSIsIjIwMzAiLCIjYzA5NDZmIiwiZ3J0Ig0KIjIwMTciLCIxNjZjbSIsIjIwMTIiLCIxNzIiLCIjNDI0YWU0IiwiMjAyMiIsImIxMzE5YiIsIiM2NjM1ZDgiDQoiMjAyMCIsIjcwY20iLCIyMDA0IiwiMTY2IixOQSwiMjA0MCIsIiM3MzM4MjAiLCJsenIiDQpOQSwiMTI4IiwiMTk5NyIsIjI5OSIsIjk4NDY3NTE5OCIsIjIwMzciLCIjYjY2NTJhIiwiZ210Ig0KIjIwMTYiLCIxNTljbSIsIjE5OTEiLE5BLCI5NDk2MTcxMzg0IiwiMjAzMCIsIiNjZWIzYTEiLCJ4cnkiDQoiMTk3MSIsIjE1N2luIiwiMTk3MCIsTkEsIjgxMjYxNzgwOSIsIjIwMjAiLCIjN2QzYjBjIiwiZ210Ig0KIjIwMTEiLCIxNzZjbSIsIjIwMTIiLCI2NCIsIjIxMzIxMzM1OSIsIjE5NzEiLCJiZTdiMTMiLCJnbXQiDQpOQSwiNjZjbSIsIjE5MjgiLCI4NyIsIjI3MTA3OTQ2IiwiMjA0MCIsTkEsInV0YyINCiIyMDIxIiwiODIiLCIyMDA3IiwiMTkxIiwiIzFjZjY5ZiIsIjIwMzkiLCJ6IiwiZG5lIg0KIjE5NTAiLCI2NmNtIixOQSwiMTExIiwiMTgzY20iLCIxOTQ3IixOQSwiIzAxNmY2YSINCiIyMDIzIixOQSwiMTk2NiIsTkEsIjE2NGNtIiwiMjAyMSIsInoiLCJ1dGMiDQoiMTkyMiIsIjEwMSIsIjIwMTUiLCIxMzYiLCIxNTFjbSIsIjIwNDAiLCIyNDVjYjMiLCJsenIiDQo=" download="extract_0005.csv">
<button aria-label="There are 72 &#39;fail&#39; rows available as a CSV file." data-balloon-pos="left" style="background-color:#67C2DC;color:#FFFFFF;border:none;padding:5px;font-weight:bold;cursor:pointer;border-radius:4px;">CSV</button>
</a>

</td>
</tr>
<tr>
<td class="gt_row gt_left" style="background-color: rgba(76,166,76,0.5); height:  40px">
</td>
<td class="gt_row gt_right" style="color: #666666; font-size: 13px; font-weight: bold; height:  40px">
6
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
<code><span style="color:purple;">▮</span>pid</code>
</p>

</td>
<td class="gt_row gt_left" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">

<p style="margin-top: 0px; margin-bottom: 0px; font-size: 11px; white-space: nowrap; text-overflow: ellipsis; overflow: hidden;">
<code>\[100,000,000, 999,999,999\]</code>
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
<code>177</code><br><code>0.64</code>
</td>
<td class="gt_row gt_right" style="border-left-width: 1px; border-left-style: dashed; border-left-color: #E5E5E5; height:  40px">
<code>99</code><br><code>0.36</code>
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

<a href="data:text/csv;base64,Iml5ciIsImhndCIsImJ5ciIsImNpZCIsInBpZCIsImV5ciIsImhjbCIsImVjbCINCiIyMDI1IiwiNjRjbSIsIjIwMjkiLCIyODEiLCIwNjcyODU5ODUiLCIxOTQ0IiwiI2NlYjNhMSIsIiMwNzIxOWEiDQoiMjAyNSIsIjE0MSIsIjIwMDYiLCIzMjciLCIjMWM0MmNjIiwiMTk1NiIsInoiLCIjZjJhZmZjIg0KIjIwMTYiLCIxNTljbSIsIjE5ODAiLE5BLCIxMzkwNjMxMzkiLCIyMDIwIiwiI2VmY2M5OCIsImJybiINCiIxOTIwIiwiNjBjbSIsIjIwMjkiLCIxMDciLCIjNTVjZTZiIiwiMjA0MCIsImQzMGY2YiIsImRuZSINCiIyMDE4IiwiNjVjbSIsIjE5MzciLE5BLCIjNGJmZjNlIiwiMjAyNyIsIjQzZmFmYiIsImdydCINCiIyMDE0IiwiMTY2Y20iLCIxOTI0IiwiMTYzIixOQSwiMjAyNCIsIiMxODE3MWQiLE5BDQoiMjAxOSIsIjE3OGNtIiwiMTkyNSIsIjI0MSIsIjA2OTc2MDc5NyIsIjIwMjkiLCIjNjIzYTJmIiwiYmx1Ig0KIjIwMTYiLCIxNzJjbSIsTkEsTkEsIiM5OGNhZWMiLCIyMDM2IiwieiIsImRuZSINCiIxOTM4IiwiMTcyaW4iLCIyMDE4IiwiMzM5IiwiIzZjMTIxNiIsIjIwMzkiLCJ6IiwiIzUxMDY3MiINCiIyMDExIiwiMTcxY20iLCIxOTcyIixOQSwiMTkwOTExODAzIiwiMjAyNSIsIiNlZmNjOTgiLCJicm4iDQoiMTk5NyIsIjk2IiwiMjAwOSIsTkEsIjA2MzY5MTcyMjIiLCIyMDI2IiwieiIsImh6bCINCiIyMDExIiwiMTcwY20iLCIxOTg5IiwiMTU1IiwiMDcxNTg4NjgyIiwiMTk1NSIsIiNjZWIzYTEiLCJncm4iDQoiMjAxNyIsIjE3NWNtIiwiMTk2NCIsIjI2NiIsTkEsTkEsIiNhOTc4NDIiLCJicm4iDQoiMjAxNCIsIjE2MmNtIiwiMTk5OSIsTkEsIjAyNjgzNTc5MSIsIjIwMjIiLCIjN2QzYjBjIiwiYnJuIg0KIjE5NTEiLCIxOTNpbiIsIjIwMTkiLE5BLCIjY2JjMDhjIiwiMjAwMiIsInoiLCIjM2U5ZjJmIg0KIjIwMTYiLCIxNzNjbSIsIjE5NzMiLE5BLE5BLCIyMDI4IiwiIzg4ODc4NSIsImJsdSINCiIyMDE0IiwiMTkwY20iLCIxOTg3IixOQSwiMDI4ODI1MTIwIiwiMjAyMyIsIiM3ZDNiMGMiLCJvdGgiDQpOQSwiODIiLCIyMDE3IiwiMTUzIiwiMTcyNjYxNjE3IiwiMjAzNiIsInoiLCIjYWU2MDdkIg0KIjIwMjAiLCI2M2luIixOQSxOQSwiMTQ2NjUwODk0IiwiMjAyNSIsIiNhMzU1YmUiLCJhbWIiDQoiMjAxNyIsIjE2MWNtIiwiMTk4MyIsTkEsIjA4MTkxNzYxMSIsIjIwMjUiLCIjNjAyOTI3Iiwib3RoIg0KTkEsIjE4MWNtIiwiMTk5MiIsIjMyMCIsIjAzMjc2OTc1NyIsIjIwMjIiLCIjNzMzODIwIiwiZ3JuIg0KTkEsIjE2M2luIiwiMjAyOSIsTkEsTkEsIjE5NjIiLCIzYTBjMzAiLCJncnkiDQoiMjAyMCIsIjE4N2NtIiwiMTk1NiIsTkEsIjE4NzgwODk1OSIsIjIwMjYiLCIjNzMzODIwIiwib3RoIg0KIjIwMTMiLCIxNzFjbSIsIjE5ODIiLCIzMzUiLCIwMTg5MDA2MzkiLCIyMDMwIiwiI2VmY2M5OCIsImh6bCINCk5BLCIxODNjbSIsIjE5MzQiLE5BLE5BLCIyMDIzIiwiIzZiNTQ0MiIsImdybiINCiIxOTk3IiwiNjljbSIsIjIwMTIiLCI1NCIsIjE4NGNtIiwiMjAzNyIsImY4ZWQ0NSIsImdyeSINCiIyMDE0IiwiMTU3Y20iLCIxOTI4IixOQSwiMDAyNTI4MTk0IiwiMjAyMSIsIiM3MzM4MjAiLCJncm4iDQoiMjAxNCIsIjE2OWNtIiwiMTkzMCIsTkEsIjE1MTkxMDA3OSIsIjIwMjkiLCIjMTgxNzFkIiwib3RoIg0KTkEsIjE4MmNtIiwiMTk5MyIsIjExNyIsIjA3MzAzNTk5OSIsIjIwMzAiLCIjMzQxZTEzIixOQQ0KIjIwMTAiLCIxOTFjbSIsIjE5ODYiLE5BLCIwODk5OTU1OTAiLCIyMDIzIiwiIzczMzgyMCIsImFtYiINCiIyMDE3IiwiODMiLCIxOTQwIixOQSxOQSwiMjAzMiIsIjE3NDc3NCIsInhyeSINCiIxOTQwIiwiMTczY20iLCIxOTQ3IixOQSxOQSwiMTk0NyIsIjdlNTE1YyIsImdtdCINCiIyMDEyIiwiMTg1Y20iLE5BLE5BLE5BLCIyMDMwIiwiI2I2NjUyYSIsImdybiINCiIyMDEyIiwiNjJjbSIsIjE5ODUiLCI1OSIsIiNhZjEwNmEiLCIxOTU3IiwiIzYyM2EyZiIsImdydCINCiIyMDE5IiwiMTcxY20iLCIxOTc2IixOQSwiMDQxOTI2MzU0IiwiMjAyMCIsIiNhOTc4NDIiLE5BDQoiMjAyMSIsIjE2N2luIiwiMjAyNSIsIjIzNCIsIjE4MWNtIiwiMTk0MSIsIjk4NjE5YSIsIiNmNWU2NTEiDQoiMTk0OSIsIjczY20iLCIxOTg1IiwiMjA3IiwiI2VlOWY5NSIsIjIwMjgiLCJ6IiwidXRjIg0KIjE5MzMiLCI2OWNtIiwiMTkzNCIsTkEsIjE3OWNtIiwiMjAzMCIsImI4ZTE0MiIsImdybiINCiIyMDI4IiwiMTExIiwiMjAzMCIsIjE4MCIsIjE4MzM5MTg2MSIsIjE5NTQiLCIxZmIzMGYiLCIjMGQwMTYwIg0KTkEsTkEsIjIwMjMiLCIyNzEiLCIjNjZlYzgyIiwiMjAyOSIsIjEwZDlkOCIsIiMzNTNlMGYiDQoiMjAxNCIsIjE2NmNtIiwiMTk1MyIsTkEsTkEsIjIwMjIiLCIjODY2ODU3IiwiYmx1Ig0KIjE5ODEiLCI1OWNtIiwiMjAwOSIsTkEsIjE2MmNtIiwiMjAyNSIsIjExNjc0MiIsImdtdCINCiIyMDE4IiwiMTg2Y20iLE5BLE5BLCIxNzg1MjUxMzIiLCIyMDIzIiwiIzg4ODc4NSIsTkENCiIyMDEzIiwiMTg1Y20iLCIxOTQwIixOQSxOQSwiMjAyOCIsIiM3YzczYTMiLCJoemwiDQoiMjAxNCIsIjE3MWNtIiwiMTk5NyIsIjMwMiIsIjEwMTM2MzM2NyIsIjIwMjUiLCJ6IiwiYW1iIg0KIjIwMTAiLCIxNjRjbSIsIjE5NDciLCI5NiIsIjE2NjExNTQ0MiIsIjIwMzAiLCI0YmMyMGEiLCJvdGgiDQoiMjAxOSIsIjE2OWNtIiwiMjAxMiIsIjI5OCIsIjA2NjAzMTY1NTgiLCIxOTIwIiwiIzg4ODc4NSIsInp6eiINCiIxOTY1IiwiNjAiLCIxOTMxIixOQSwiI2FhNWZkMCIsIjIwMTciLCI1NzkyNjYiLCJncnkiDQoiMTk5NyIsIjYxY20iLCIyMDIzIixOQSwiMTYyY20iLCIxOTg3IiwieiIsIiM5ZjQ1OGMiDQoiMjAyMCIsTkEsIjIwMTAiLE5BLCIwNjU2MDc2NDkiLE5BLCI1OWUzNzYiLCJibHUiDQoiMjAxNSIsIjE1MGNtIiwiMjAyMiIsTkEsIjE2N2NtIiwiMjAzMiIsImNhYTE0NSIsIiMwNjY1MGEiDQoiMjAxNyIsTkEsIjE5MzIiLCIyMDMiLCIxMDU5MjEwODUiLE5BLCIjNDE5ZDczIiwiZ3J5Ig0KIjIwMTciLCIxNzFjbSIsIjE5MzIiLCIyMjAiLCIwODUzMDk3MDkiLCIyMDI0IiwiIzYyM2EyZiIsImdyeSINCiIyMDI0IiwiMTgxaW4iLCIyMDI5IiwiOTciLCIjYzM4NjIwIiwiMTk3NiIsInoiLCJ6enoiDQoiMjAxOCIsIjE4MWNtIiwiMTk5NiIsTkEsIjAyMjI4NTIxOSIsIjIwMjEiLCIjNmI1NDQyIiwiYW1iIg0KIjIwMTQiLCIxNzJpbiIsIjE5NTAiLE5BLCIxODdjbSIsIjIwMjgiLCJ6IiwiYnJuIg0KIjIwMjUiLCI1OWluIiwiMTkyMCIsIjIwMiIsIjE4MmNtIiwiMjAzNSIsInoiLCIjNzk5ZjI5Ig0KIjIwMTciLCIxODVjbSIsIjE5ODkiLE5BLCIxOTUyNzYyMDciLCIyMDIyIiwiIzczMzgyMCIsImJsdSINCiIyMDEzIixOQSwiMTk1MSIsIjI1NyIsIjEyMzA2NTYzOSIsTkEsIiM3ZDNiMGMiLCJncnkiDQoiMjAwOSIsIjE3MmluIiwiMjAwNSIsIjMzNCIsIjE4OWNtIiwiMjAzMiIsInoiLCJ4cnkiDQoiMjAyMCIsIjE1OWNtIixOQSxOQSwiMTY2Y20iLCIyMDI2IiwieiIsIm90aCINCiIyMDE4IiwiMTg4Y20iLCIxOTk5IixOQSwiI2VjM2Q1MyIsIjIwMjQiLCIjY2ViM2ExIiwib3RoIg0KIjIwMTAiLCIxNjciLCIyMDAzIiwiMTY5IixOQSwiMjAzNyIsIjQ4NjgwMCIsIiMyOWJkZDYiDQoiMjAxMyIsIjE4OWNtIiwiMTk5MCIsTkEsIjE3NmNtIiwiMjAyNCIsIiNjZmEwN2QiLCJncnkiDQoiMjAyMCIsIjE3MmNtIiwiMTk3OCIsTkEsIjA5MzMxNzk5MCIsIjIwMjIiLCIjY2ViM2ExIiwib3RoIg0KIjIwMTgiLCIxNzRjbSIsIjE5ODgiLE5BLCIwOTY4OTE0MDkiLCIyMDI5IiwiI2Q4MjgyMiIsImh6bCINCiIxOTgyIiwiMTQ0IiwiMjAxMiIsTkEsIiMzYjQzYzEiLCIyMDMyIiwiMmYyNmFiIiwiI2Y4OWRmMCINCiIyMDE4IiwiMTcwaW4iLCIxOTcxIixOQSwiMDMwODUwNzQ5IiwiMjAyMyIsIiNjZWIzYTEiLCJoemwiDQoiMjAxNiIsIjE4NmNtIiwiMTk0NyIsIjY0IixOQSwiMjAyMSIsIiNhOTc4NDIiLE5BDQoiMjAyNCIsIjE2N2luIiwiMjAwNiIsIjM0NiIsIjE5NTMxMzQxIiwiMjAzNSIsInoiLCIjZDMyMzIwIg0KIjIwMTUiLCIxNTJjbSIsIjIwMTkiLE5BLCIxODJjbSIsIjIwMjgiLCI0M2Q1NmQiLCJ6enoiDQoiMTk1OCIsIjE3NmNtIixOQSxOQSwiIzRjYjQ4MCIsIjIwMjYiLCJ6IiwiZG5lIg0KIjIwMTkiLCIxNTdjbSIsIjE5NTgiLE5BLCIxNDA5MjI0ODQiLCIyMDI1IiwiIzYyM2EyZiIsImJybiINCiIyMDE0IiwiMTYyY20iLCIxOTUxIixOQSxOQSwiMjAyOSIsIiNiNjY1MmEiLCJibHUiDQoiMjAyMCIsIjE3OGNtIiwiMTk0OSIsIjIyMCIsIjExNjAwMzM0MyIsIjIwMjgiLCIjYzA5NDZmIiwiaHpsIg0KIjIwMTMiLCI3NGNtIiwiMjAwNyIsIjMxNCIsIjE4NmNtIiwiMTk3MyIsIjE4MGUwYyIsImh6bCINCiIyMDEyIiwiMTU3Y20iLCIxOTYzIixOQSwiMDA1Mzg3NTcwIiwiMjAyMyIsIiM4NjY4NTciLCJicm4iDQoiMjAxNyIsIjE1M2NtIiwiMTk4MyIsTkEsIjAzOTk5NTE3MSIsIjIwMjQiLCIjYTk3ODQyIiwiYW1iIg0KIjIwMTEiLCIxNTdjbSIsIjE5NzkiLE5BLCIxMTA4NTU1NDIiLCIyMDIxIiwiI2MwOTQ2ZiIsImJsdSINCiIyMDEwIiwiMTg0Y20iLCIxOTg1IixOQSwiIzE3NTEyOSIsIjIwMzIiLCIjZmZmZmZkIiwiaHpsIg0KIjIwMTciLCIxNzJjbSIsIjE5NDciLE5BLCIwMTM0MTUzODciLCIyMDIzIiwiIzczMzgyMCIsImJsdSINCiIyMDE3IiwiMTY2Y20iLCIyMDEyIiwiMTcyIiwiIzQyNGFlNCIsIjIwMjIiLCJiMTMxOWIiLCIjNjYzNWQ4Ig0KIjE5MjgiLCIxODVjbSIsIjE5ODQiLE5BLCIjYWM1YTkwIiwiMjAzMCIsImFjOGY0MyIsImJybiINCiIyMDIwIiwiNzBjbSIsIjIwMDQiLCIxNjYiLE5BLCIyMDQwIiwiIzczMzgyMCIsImx6ciINCiIyMDIwIiwiMTc4Y20iLCIxOTQxIiwiMjcxIiwiMDkzMzYxMjQwIiwiMjAyNyIsIiNjZmEwN2QiLCJicm4iDQoiMjAxOCIsIjE1NmNtIiwiMTk3NyIsIjI3OCIsTkEsIjIwMjciLCIjODg4Nzg1IiwiYmx1Ig0KIjIwMjEiLCI4MiIsIjIwMDciLCIxOTEiLCIjMWNmNjlmIiwiMjAzOSIsInoiLCJkbmUiDQoiMTk1MCIsIjY2Y20iLE5BLCIxMTEiLCIxODNjbSIsIjE5NDciLE5BLCIjMDE2ZjZhIg0KIjIwMTEiLCIxOTNjbSIsIjE5NTIiLE5BLCIwMzMzODIzMzgiLCIyMDI2IiwiIzZiNTQ0MiIsImdybiINCiIyMDIwIiwiMTYzY20iLCIxOTY3IiwiMzA5IiwiMTI4NTU4NDI5MyIsIjIwMzEiLCIjNzMzODIwIiwib3RoIg0KIjIwMjAiLCIxNTNjbSIsIjE5OTAiLE5BLCIwMTgyODMwNDQiLCIyMDI0IiwiIzYwMjkyNyIsImdyeSINCiIyMDE1IiwiMTY3Y20iLCIxOTYwIiwiMzEzIixOQSwiMjAyNSIsIiMxODE3MWQiLCJicm4iDQoiMjAxMSIsIjE4MGNtIiwiMTk3MCIsIjM0MiIsIjEzNzI4NzQ0OSIsIjIwMjkiLCIjYTk3ODQyIiwib3RoIg0KIjIwMTMiLCIxODNjbSIsIjE5ODUiLE5BLE5BLCIyMDIyIiwiI2NmYTA3ZCIsImdybiINCiIyMDIzIixOQSwiMTk2NiIsTkEsIjE2NGNtIiwiMjAyMSIsInoiLCJ1dGMiDQoiMjAxMiIsIjE4MGNtIiwiMTk0MyIsIjE0NCIsIjE1NWNtIiwiMjAzNSIsTkEsImFtYiINCiIyMDIwIiwiMTkzaW4iLCIyMDI2IiwiODIiLE5BLCIyMDM0IiwiI2I2NjUyYSIsImdybiINCiIxOTIyIiwiMTAxIiwiMjAxNSIsIjEzNiIsIjE1MWNtIiwiMjA0MCIsIjI0NWNiMyIsImx6ciINCiIyMDE0IiwiMTYzY20iLCIxOTIyIiwiMTY5IiwiMTQ3NzY4ODI2IiwiMjAzMCIsIiNjZWIzYTEiLCJibHUiDQo=" download="extract_0006.csv">
<button aria-label="There are 99 &#39;fail&#39; rows available as a CSV file." data-balloon-pos="left" style="background-color:#67C2DC;color:#FFFFFF;border:none;padding:5px;font-weight:bold;cursor:pointer;border-radius:4px;">CSV</button>
</a>

</td>
</tr>
</tbody>
<tfoot class="gt_sourcenotes">
<tr>
<td class="gt_sourcenote" colspan="14">
<span
style="background-color: #FFF;color: #444;padding: 0.5em 0.5em;position: inherit;text-transform: uppercase;margin-left: 10px;border: solid 1px #999999;font-variant-numeric: tabular-nums;border-radius: 0;padding: 2px 10px 2px 10px;font-size: smaller;">2020-12-04
08:57:27 EST</span><span
style="background-color: #FFF;color: #444;padding: 0.5em 0.5em;position: inherit;margin: 5px 1px 5px 0;border: solid 1px #999999;border-left: none;font-variant-numeric: tabular-nums;border-radius: 0;padding: 2px 10px 2px 10px;font-size: smaller;">&lt;
1 s</span><span
style="background-color: #FFF;color: #444;padding: 0.5em 0.5em;position: inherit;text-transform: uppercase;margin: 5px 1px 5px -1px;border: solid 1px #999999;border-left: none;border-radius: 0;padding: 2px 10px 2px 10px;font-size: smaller;">2020-12-04
08:57:27 EST</span>
</td>
</tr>
</tfoot>
</table>
</div>
<!--/html_preserve-->
