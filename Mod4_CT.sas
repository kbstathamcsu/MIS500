data TestScores;
         input TScore_before TScore_after @@;
         datalines;
   123 133   135 136   129 135   117 137
   120 142   138 135   140 121   145 147
   136 168   140 152   146 129   137 145
   ;
   run;

 

proc print data=TestScores;

run;

 

 ods graphics on;

 proc ttest;

 paired TScore_before*TScore_after;

 run;

 ods graphics off;