set key
title "FOMC predictions"
plot "$datafile" u 1:(1./60.) title "2013"  s cumul , "$datafile" u 2:(1./60.) title "2014" s cumul, "$datafile" u 3:(1./60.) title "2015"  s cumul ,"$datafile" u 4:(1./60.) title "2016"  s cumul 
set term png
set output "~/org-table-xtras/example2.png"
replot
exit