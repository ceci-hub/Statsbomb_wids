# About the code
- ballreceipts.R looks at successful ball receipts, distance to closest opponet, players on each team with most number of ball receipts, plots of ball receipts in different ways
- statsbombfunctionstoreaddata.R contains the functions to read in free 360events and free events due to issue loading the functions using the [StatsBombR](https://github.com/statsbomb/StatsBombR) package. Note that free events function is fixed in the latest update and the code now uses the modified function. Only the free360 function is used in this file.
- createpitch.R contains a function [create_statsbomb_pitch](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/2.CreateAPitchForStatsBomb.md). The function in this file has been modified from the original based on this [link](https://stackoverflow.com/questions/63398825/building-a-shotmap-in-ggplot2-with-facet-wrap-and-receive-error-width-must-be-o) to allow for facet_wrap to work.

-  passes_under_pressure.r: shows all the matches that played Italy and England, how successfull were the passes (completed passes). So, it is the Quality of passes received by them
