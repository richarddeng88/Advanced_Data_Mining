* read dataset ;
proc iml;if exist("client") then call delete("work", "client");
proc import datafile='C:\Users\Richard\Desktop\R\data\fraud\client.csv' out= client;run;


proc iml;if exist("card") then call delete("work", "card");
proc import datafile='C:\Users\Richard\Desktop\R\data\fraud\card.csv' out= card;run;




