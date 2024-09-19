# Stimuli, data, and analysis code for Tandoc et al. - Object feature memory is distorted by category structure

This repository includes the **stimuli, data, and code** used in the manuscript. For a comprehensive overview of the stimuli, data collection, and analysis methods, please refer to the Methods and Results sections of the manuscript.

If you have any additional questions, please contact Marlie (tandoc@sas.upenn.edu).

## File: ```analyze.R```
Description: R script to analyze the data. Contains code to run statistical tests and models in the manuscript.

## File: ```data/data_main.csv```
Description: Contains trial-level data from the main phase (interleaved part and color trials) for both experiments. Each row is a trial. This dataframe is read in by ```analyze.R``` to recreate the manuscript results and statistics.

Columns (19):
- **participant**: Participant identifier
- **experiment**: Experiment number (1 or 2)
- **phase**: Phase of experiment (main)
- **trial_type**: Type of trial (part or color)
- **trial_num**: Trial number (1-96)
- **block_num**: Block number (1-6)
- **category**: Category identifier (A or B) of the satellite 
- **codename**: Exemplar identifier (e.g. funda, sorex, etc.) of the satellite
- **cued_part**: Part of the satellite that was cued (tail, back, head, leg)
- **part_attempts_total**: Total of attempts it took to choose the correct part (relevant only for part trial_type)
- **part_first_attempt_corr**: Whether the participant chose the correct part on their first attempt (relevant only for part trial_type)
- **target_hex**: The HEX code of the correct part's color (relevant only for color trial_type)
- **target_l**: The L value in CIELAB color space of the correct part's color (relevant only for color trial_type)
- **target_a:** The A value in CIELAB color space of the correct part's color (relevant only for color trial_type)
- **target_b**: The B value in CIELAB color space of the correct part's color (relevant only for color trial_type)
- **color_response_bias**: The color bias value assigned based on what color option (color_response_type) the participant chose (1 = attract, 0 = target, orthogonal, -1 = repel) (relevant only for color trial_type)
- **color_response_correct**: Whether the participant chose the correct/target color (1 = correct, 0 = incorrect) (relevant only for color trial_type)
- **color_response_type**: The color option the participant chose on that trial (target, attract, repel, orthogonal) (relevant only for color trial_type)

## File: ```data/data_test.csv```
Description: Contains trial-level data from the test phase (post-learning) in Experiment 2. Each row is a trial. This dataframe is read in by ```analyze.R``` to recreate the manuscript results and statistics.

Data columns are identical to **data/data_main.csv** except it does not contain the column **part_attempts_total** and **part_first_attempt_corr** because in the test phase participant's had only once attempt at each part trial. There is also one new column **part_response_correct** which is described below

Columns: (18) 
- **part_response_correct**: Whether the participant chose the correct part (relevant only for part trial_type)


## Folder: ```stimuli/```
Description: Contains images with transparent backgrounds (PNG) used to create the satellite stimuli. Each image is a different satellite part. t = tail, h = head, b = back, l = leg. 1-6 = part identifier. Satellite base can be used in conjunction with the parts to give the satellite a body.


