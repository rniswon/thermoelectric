# Thermoelectric Water-use Models Results
The files in this directory are used for reformatting thermoelectric
model results, for use by the visualization team.

# Post processing of final results

A draft copy of the spec. may be found
[here](https://doimspp.sharepoint.com/sites/WBEEP/_layouts/15/Doc.aspx?sourcedoc={596c494e-2b7d-4b7f-87e0-1206bfd66699}&action=edit&wd=target%28Workflow.one%7Cc5395211-5f35-485e-bcc8-259f10bbac2a%2FPost-processing%20WU%20results%7C2ec8a499-db62-44ea-8a2b-8dee5e33fb5c%2F%29&wdorigin=NavigationUrl).

1. Outputs from the FEWSR and Tower models will include monthly median,
minimum, and maximum consumption and withdrawal estimates for each
power plant in the CONUS for each year 2008-2020.

   1. For FEWS output examples, see
   https://github.com/rniswon/Thermoelectric/tree/master/targets_2015/output/fews

      1. There is one tab each (6 total) for median (a.k.a. "best"),
         minimum, and maximum consumption and withdrawal estimates

      2. Ignore tabs "Max withdrawal with 0% cushion" and "Min withdrawal
         with 0% cushion"

   2. Tower output examples, may be found
      [here](https://github.com/rniswon/Thermoelectric/tree/master/targets_2015/model_archive/tower/Output).

      1. For tower outputs, we are only interested in
         tower_model_consumption_out; for now, ignore tower_model_evap_out.

      2. Ken has added withdrawal calculations to the tower model, so I'm
         not sure what the outputs will look like, but they will probably
         look like the consumption out files.

2. COMPLEX-cooled plants with separate cooling system types: 

   1. Example: Plant ID 3 has a once-through system that is separate
      from its tower system.

   2. Monthly median, minimum, and maximum consumption and withdrawal
      for each cooling type will need to be summed to the plant level.

   3. I would also like to see the results by cooling type, too, before
      summing to the plant level.

3. Complex-cooled plants with two or more cooling types reported as one unit  

   1. Example: Plant ID 46 has a once-through system and a tower system
      that's reported as one unit.

   2. We allocate condenser duty between the cooling types (e.g.,
      once-through 80%, tower 20%) for median WD and CU estimates, and
      then allocate 100% of the CD to each cooling type for minimum and
      maximum estimates.

      1. Median WD and CU = sum median estimates for each cooling type to
         the plant level.

      2. Minimum WD = the minimum estimate from 100% allocation of CD to
         the recirculating system which will either be a tower or a pond.

      3. Maximum WD = the maximum estimate from 100% allocation of CD to
         the once-through system (river, lake, saline).

      4. Minimum CU = the minimum estimate from 100% allocation of CD to
         the once-through system (river, lake, saline).

      5. Maximum CU = the maximum estimate from 100% allocation of CD to
         the recirculating system which will either be a tower or a pond.

4. Formatting for our own QA/QC  

   1. Formatted: plant IDs as rows; WD and CU (min., med., max.) as headers

   2. See `2015_monthly_TE_WD_CU.xlsx` in Teams folder:
      `Water Use Models > Thermoelectric > 2015_TE_estimates`

      1. Jan-Dec median withdrawal | Jan-Dec median consumption | Jan-Dec
         minimum WD | Jan-Dec maximum WD | Jan-Dec minimum CU | Jan-Dec
         maximum CU

   3. I would also like to include plant characteristic data in these
      files (at some point) such as those in the 2015 annual estimates
      we published in `2015_TE_Model_Estimates.xlsx` in the same Teams
      folder linked above. These include: plant name, county, state,
      name of water source, lat./long., cooling type, generation type,
      water source code, water type code, and net generation.

   4. One file for each year 2008-2020, 13 separate files at the plant
      level, and 13 separate files at the cooling-type level (where
      complex-cooled plants are separate by each of their cooling
      types). Example:
      | EIA_Plant_ID | Jan_WD_Med | Jan_CU_med | Jan_WD_Min | Jan_WD_Max | Jan_CU_Min | Jan_CU_Max |
      | ---          | ---        | ---        | ---        | ---        | ---        | ---        |
      | 3            | -          | -          | -          | -          | -          | -          |
      | 7            | -          | -          | -          | -          | -          | -          |
      | 8            | -          | -          | -          | -          | -          | -          |
      | 10           | -          | -          | -          | -          | -          | -          |
      | 26           | -          | -          | -          | -          | -          | -          |

5. Formatting for the data visualization team (chosen so that we don't
   have multiple variables in the same file);

   1. This matches the other teams' format;

   2. Formatted: plant IDs are in columns (headers) and each month-year
      (e.g., "Jan-08") WU estimate is a row;

   3. 6 separate files: 3 for WD (min., med., max.) and 3 for CU (min., med.,
      max.) Example:
      | WD-med | 3   | 7   | 8   | 10  | 26  |
      | ---    | --- | --- | --- | --- | --- |
      | Jan-08 | -   | -   | -   | -   | -   |
      | Feb-08 | -   | -   | -   | -   | -   |
      | Mar-08 | -   | -   | -   | -   | -   |

See `Thermoelectric/targets_2015/output/fews/*.xlsx` and
`Thermoelectric/tower/Output/*.csv` in the source tree for examples of
output format.
