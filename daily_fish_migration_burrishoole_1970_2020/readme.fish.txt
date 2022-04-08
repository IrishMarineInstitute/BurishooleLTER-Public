Metadata for Burrishoole Fish migration data

Filename: “daily_fish_migration_burrishoole_1970_2020_processed.csv”

Columns:
•	"index"   
integer

•	"date"   
Daily time step, format="%Y-%m-%d", tz="UTC"

•	"latitude"   
decimal degrees

•	"longitude"   
decimal degrees

•	"station_id"   
Can be "Combined_MillRace_SalmonLeap_Trap"; "MillRace_Trap"; "SalmonLeap_Trap".
If "Combined_MillRace_SalmonLeap_Trap", it's the sum of the  "MillRace_Trap" and "SalmonLeap_Trap". 
For salmon, we provide all three counts.

•	"species"  
Can be "Salmo salar"; "Salmo trutta"; "Anguilla anguilla

aphia_id

•	"aphia_id" 
WoRMS id code for taxon details

•	"stream" 
can be either "upstream" or "downstream". 
Upstream indicates moving from Lough Furnace (oceanic) to Lough Feeagh (freshwater).
Downstream indicates moving from Lough Feeagh (freshwater) to Lough Furnace (oceanic)

•	"daily_count" 
Daily count of fish through the trap




