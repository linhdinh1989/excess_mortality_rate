
libname chile 'C:\Users\PC\Dropbox\1918 Flu Project\SAS excess mortality code and SAS data';
/*
data chile.alldeathsarizona; set chile.alldeathsarizona;
respu20=respu5+resp520;
respo50=resp5070+respo70;
ACu20=ACu5+AC520;
ACo50=AC5070+ACo70;
popu20=popu5+pop520;
popo50=pop5070+popo70;

nweek=_n_;
date2=intnx('week','03JAN1915'd,nweek-1);
format date2 date9.;
drop date;
rename date2=date;
popallage=pop;
acallage=ac;
respallage=resp;
run;
*/


%macro serflingage(age);

data out99r; set chile.alldeathsarizona;
length age $10.;
death_resp=resp&age;
inc_resp_r=resp&age/pop&age*10000;
death_ac=AC&age;
inc_ac_r=AC&age/pop&age*10000;
agepop=pop&age;
age="&age";

death_resp_199=resp;
inc_resp_199_r=resp/pop*10000;
death_199=AC;
inc_ac_199_r=AC/pop*10000;
mergeid=1;
run;

PROC EXPAND data= out99r OUT=out99 METHOD=NONE;
 CONVERT inc_resp_r  = inc_resp  / TRANSFORM = ( CMOVAVE 3 );
 CONVERT inc_ac_r  = inc_ac  / TRANSFORM = ( CMOVAVE 3 );
CONVERT inc_resp_199_r  = inc_resp_199  / TRANSFORM = ( CMOVAVE 3 );
CONVERT inc_ac_199_r  = inc_ac_199  / TRANSFORM = ( CMOVAVE 3 );
 RUN;


proc means data=out99 noprint;
var inc_resp inc_ac inc_resp_199;
output out=statsbaseline p90=p90_inc_resp p90_inc_ac p90_inc_resp_199;
where date < '01APR1918'd;
run;

data statsbaseline; set statsbaseline; mergeid=1;run;
data out100; merge out99 statsbaseline;by mergeid;run;

symbol1 value=none interpol=join color=blue;
symbol2 value=none interpol=join color=blue line=2;

symbol3 value=none interpol=join color=red;
symbol4 value=none interpol=join color=red line=2;

title "All cause and resp death rate per 10,000, age = &age";
proc gplot data=out100;
plot (inc_ac p90_inc_ac inc_resp p90_inc_resp)*date / overlay;run;quit;


title "All cause and resp death rate per 10,000, all ages";
proc gplot data=out100;
plot (inc_resp_199 p90_inc_resp_199 inc_ac_199  )*date /overlay;run;quit;




data serfling; set out100;
/* Creates and labels influenza seasons */
/*if              (  '01NOV1915'd <= date < '01APR1916'd)  then season='1915-16';
if              (  '01NOV1916'd <= date < '01APR1917'd)  then season='1916-17';
if              (  '01NOV1917'd <= date < '01APR1918'd)  then season='1917-18';*/
if              (  '01APR1918'd <= date < '01AUG1918'd)  then season='1918 summ';
if              (  '01AUG1918'd <= date < '01APR1919'd)  then season='1918 fall';
else if (  '01JAN1920'd <= date < '01APR1920'd)  then season='1920 win';
epi=0;
modeath=month(date);
year=year(date);
if season ne '' then epi=1;
IF epi=0 then inc_resp_mod=inc_resp; else inc_resp_mod=.;
/*if inc_resp>p90_inc_resp then inc_resp_mod=.;*/
IF epi=0 then inc_ac_mod=inc_ac; else inc_ac_mod=.;
/*if inc_ac>p90_inc_ac then inc_ac_mod=.;*/

IF epi=0 then inc_resp_199_mod=inc_resp_199; else inc_resp_199_mod=.;
/*if inc_resp_199>p90_inc_resp_199 then inc_resp_199_mod=.;*/

TIME = NWEEK/100;
square= TIME*TIME;
cube=TIME**3;
quad=TIME**4;
omega=(2*3.14159265)/52.17;
omega2=(4*3.14159265)/52.17;
omega3=(8*3.14159265)/52.17;


cos1=cos(omega*nweek);
sin1=sin(omega*nweek);

cos2=cos(omega2*nweek);
sin2=sin(omega2*nweek);

cos3=cos(omega3*nweek);
sin3=sin(omega3*nweek);

epim=5*epi;
run;



title "Regression for resp mortality per 10,000 for all ages ";
proc reg data=serfling;
 model inc_resp_199_mod= nweek SQUARE /*CUBE quad quint*/ cos1 sin1 cos2 sin2 cos3 sin3 ;
/*plot residual.*obs.;*/
output out=outnew  u95=u_inc_resp_199 p=p_inc_resp_199 /*residual=r_I*/;
run;


title "Regression for resp mortality per 10,000, age =&age ";
proc reg data=outnew;
 model inc_resp_mod= nweek SQUARE /*CUBE quad quint*/ cos1 sin1 cos2 sin2 cos3 sin3;
/*plot residual.*obs.;*/
output out=outnew2  u95=u_inc_resp p=p_inc_resp /*residual=r_I*/;
run;


title "Regression for all-cause mortality per 10,000, age =&age ";
proc reg data=outnew2;
 model inc_ac_mod= nweek SQUARE /*CUBE quad quint*/ cos1 sin1 cos2 sin2 cos3 sin3;
/*plot residual.*obs.;*/
output out=outnew3  u95=u_inc_ac p=p_inc_ac /*residual=r_I*/;
run;

symbol1 color=green value= interpol=spline
             height=1 cm width=2;
symbol2 color=blue value= interpol=spline
             height=1 cm width=2 line=1;
symbol3 color=red value= interpol=spline
             height=1 cm width=2;


title "time series of RESP weekly deaths per 10,000, all ages ";
proc gplot data=outnew3;
plot (inc_resp_199 p_inc_resp_199  u_inc_resp_199 epim)*date/overlay;
run;

title "time series of RESP weekly deaths per 10,000, age =&age ";
proc gplot data=outnew3;
plot (inc_resp p_inc_resp  u_inc_resp epim)*date/overlay;
run;

title "time series of all-cause weekly deaths per 10,000, age =&age ";
proc gplot data=outnew3;
plot (inc_ac p_inc_ac  u_inc_ac epim)*date/overlay;
run;

PROC PRINT DATA=outnew3; 
   TITLE "all cause";
   VAR date inc_ac p_inc_ac u_inc_ac;
RUN;

PROC PRINT DATA=outnew3; 
   TITLE "Resp";
   VAR date inc_resp p_inc_resp u_inc_resp;
RUN;

/* Excess mortality estimates */

                data output; set outnew3;
                excess_resp=0;
                excess_resp2=0;
                excess_resp3=0;
                excess_ac=0;
                excess_ac2=0;
                excess_ac3=0;
                if inc_resp_199 > u_inc_resp_199 then epid95=1;
                else epid95=0;

                /*epid=0;

                if '10NOV1918'd <=date <= '01DEC1918'd then epid=1;
                if '10AUG1919'd <=date <= '22FEB1920'd then epid=1;
                */

                excess_resp = (inc_resp - p_inc_resp);
                excess_ac = (inc_Ac - p_inc_ac);

                if epid95=1 then excess_resp2=(inc_resp - p_inc_resp);
                if epid95=1 then excess_ac2=(inc_ac - p_inc_ac);

                excess_resp3=max(0,excess_resp2);
                excess_ac3=max(0,excess_ac2);
                run;


                proc means data=output noprint sum nway;
                var excess_resp
                excess_resp2
                excess_resp3
                excess_ac
                excess_ac2
                excess_ac3
                epid95;

                output out=sexcess sum=excess_resp
                excess_resp2
                excess_resp3
                excess_ac
                excess_ac2
                excess_ac3
                epid95;
                class season age agepop ;
                where season ne "";
                run;

title "Excess mortality rates per 10,000, age= &age";
proc print data=sexcess;
run;

%mend serflingage;


options MPRINT;

%serflingage(u5);
proc append data=sexcess base=summary;run;


%serflingage(515);
proc append data=sexcess base=summary;run;

%serflingage(1525);
proc append data=sexcess base=summary;run;


%serflingage(2545);
proc append data=sexcess base=summary;run;


%serflingage(4565);
proc append data=sexcess base=summary;run;


%serflingage(o65);
proc append data=sexcess base=summary;run;

%serflingage(allage);
proc append data=sexcess base=summary;run;

data chile.summaryexcessArizona2; set summary;run;

proc print;run;

