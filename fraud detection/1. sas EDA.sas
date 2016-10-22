* read dataset ;
* read .csv file;
proc iml;if exist("card") then call delete("work", "card");
proc import datafile='C:\Users\Richard\Desktop\R\data\fraud\card.csv' out= card;run;

*@@ read a .dat file;
data richard.card;
infile card;
input card_id disp_id type $ issued_date $ ;
run;
proc print data=richard.card (obs=10);run;

*wriite a .dat file;
data _null_;
set card;
file 'c:\users\richard\desktop\sas\card.dat';
put card_id disp_id type $ issued_date $ ;
run;

* @@@@write a .csv file;
data _null_;
	set card;
	file 'c:\users\richard\desktop\sas\card.csv' dlm=',';
	put card_id disp_id type $ issued_date $ ;
run;

* see head and tail;
proc print data = card (firstobs=1 obs=10) label;
label issued = 'issued_date';
run;

* summarzie;
proc means data=card;
by type;
run; *only present numeric variables;

* check the dimensions;
proc contents data=card; run;


* change the variable name;
data new; set card;
rename disp_id=disposition_id issued=issued_date;
run;
proc print data=new (obs=10);run;



* 2???;
data card1 card2;set card;run;
data conbine; set card1 card2;run;
proc contents data=conbine;run;

* filter ;
data new; set card;
where card_id < 20 and disp_id < 100 and type contains "cla";
run;
proc print data=new; run;

*@@ create multiple data set at one time;
data classic gold junior;
set card;
if type="classic" then output classic;
if type="gold" then output gold;
if type="junior" then output junior;
run;
proc print data = junior (obs=10);run;


* sort; 
proc sort data=card out=new_card;  * create a new dataset, and will not change the original data set;
by type descending card_id;  * descending, add "descending", otherwise no add;
run;
proc print data=new_card (obs=10); 
var type card_id issue ;
run;

* select / delete variable (subseting variables);
data new(keep=card_id type issued);
set card;
run;

data new(drop=type);
set card;
run;

* mutation;
proc iml;if exist("client") then call delete("work", "client");
proc import datafile='C:\Users\Richard\Desktop\R\data\fraud\client.csv' out= client;run;

data client_new;
set client;
year = floor(birth_number/10000);
month = floor(mod(birth_number,10000)/100);
day = floor(mod(birth_number,100));
gender = 'Male';
if month > 12 then do;
	gender = 'Female';
	month = month-50;
end;
run;

*group by;
proc means data = card;
class type;
run;

* merge;
proc iml;if exist("client") then call delete("work", "disposition");
proc import datafile='C:\Users\Richard\Desktop\R\data\fraud\disposition.csv' out= disposition;run;

proc iml;if exist("client") then call delete("work", "account");
proc import datafile='C:\Users\Richard\Desktop\R\data\fraud\account.csv' out= account;run;

proc sort data = disposition; by account_id;run;
proc sort data = account; by account_id;run;

data a;
   merge disposition account;
   by account_id;
run;

proc sort data=a; by client_id, district_id;run;
data b;
merge a(in=a) client(in=b);
by client_id district_id;
if a=1 and b=1;
run;
proc contents data =b;run;

proc sort data=b; by disp_id;run;
proc sort data=card; by disp_id;run;
data df;
merge b(in=a rename=(disp_id=ds_id)) card(in=b rename=(disp_id=ds_id));
by ds_id;
run;
proc contents data =df;run;


* formating issue - printing label;
proc print data = card (firstobs=1 obs=10) label;
label issued = 'issued_date';
run;

* distinct;
proc sort data =card out = test nodupkey;
by type;
run;
proc print data =test;
var type;
run;
