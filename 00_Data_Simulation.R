# Created by Adrien Hernandez

# Data can be simulated in SUMO through the use of system commands
# We will use SUMO's user friendly interface to download the part of Quebec city's road network
# Note that in the interface we are only interested by selecting a part of the city, we won't tune any 
# parameters since we will do it later in the project.

# Python is required to run the following commands in order to generate the simulation.
# If python is not installed in your system, I found that it is possible to run python through R thanks to the package reticulate
install.packages("reticulate")
library(reticulate)

# Calling Python

system('python "C:\\Program Files (x86)\\Eclipse\\Sumo\\tools\\osmWebWizard.py"')

# Once the scenario is generated, we get a folder with a name corresponding to the current date.
# The folder contains 10 files: osm.net.xml, osm.netccfg, osm.passenger.trip.xml, osm.poly.xml,
# osm.polyccfg, osm.view.xml, osm_bbox.osm.xml, run.bat and osm.sumoccfg

system('python "C:\\Program Files (x86)\\Eclipse\\Sumo\\tools\\randomTrips.py" -n Quebec_data_2\\osm.net.xml -o Quebec_data_2\\nightAM.passenger.trips.xml -b 1800 -e 23400 -p  1.5 --prefix 01 --fringe-factor 1 --min-distance 1500.0 --seed 28 --route-file out.rou.xml --weights-output-prefix weights --additional-file Quebec_data_2\\osm.add.xml --validate')
system('python "C:\\Program Files (x86)\\Eclipse\\Sumo\\tools\\randomTrips.py" -n Quebec_data_2\\osm.net.xml -o Quebec_data_2\\rushAM.passenger.trips.xml -b 23400 -e 32400 -p  0.3 --prefix 02 --fringe-factor 1 --min-distance 1500.0 --seed 28 --route-file out.rou.xml --weights-output-prefix weights --additional-file Quebec_data_2\\osm.add.xml --validate')
system('python "C:\\Program Files (x86)\\Eclipse\\Sumo\\tools\\randomTrips.py" -n Quebec_data_2\\osm.net.xml -o Quebec_data_2\\other.passenger.trips.xml -b 32400 -e 54000 -p  1.5 --prefix 03 --fringe-factor 1 --min-distance 1500.0 --seed 28 --route-file out.rou.xml --weights-output-prefix weights --additional-file Quebec_data_2\\osm.add.xml --validate')
system('python "C:\\Program Files (x86)\\Eclipse\\Sumo\\tools\\randomTrips.py" -n Quebec_data_2\\osm.net.xml -o Quebec_data_2\\rushPM.passenger.trips.xml -b 54000 -e 64800 -p  0.3 --prefix 04  --fringe-factor 1 --min-distance 1500.0 --seed 28 --route-file out.rou.xml --weights-output-prefix weights --additional-file Quebec_data_2\\osm.add.xml --validate')
system('python "C:\\Program Files (x86)\\Eclipse\\Sumo\\tools\\randomTrips.py" -n Quebec_data_2\\osm.net.xml -o Quebec_data_2\\nightPM.passenger.trips.xml -b 64800 -e 82800 -p  1.5 --prefix 05 --fringe-factor 1 --min-distance 1500.0 --seed 28 --route-file out.rou.xml --weights-output-prefix weights --additional-file Quebec_data_2\\osm.add.xml --validate')

system('python "C:\\Program Files (x86)\\Eclipse\\Sumo\\tools\\randomTrips.py" -n Quebec_data_2\\osm.net.xml -o Quebec_data_2\\normal.passenger.trips.xml -b 1800 -e 82400 -p  2 --prefix 01 --fringe-factor 1 --min-distance 1500.0 --seed 28 --route-file out.rou.xml --weights-output-prefix weights --additional-file Quebec_data_2\\osm.add.xml --validate')
system('python "C:\\Program Files (x86)\\Eclipse\\Sumo\\tools\\randomTrips.py" -n Quebec_data_2\\osm.net.xml -o Quebec_data_2\\rushAM.passenger.trips.xml -b 23400 -e 32400 -p  0.3 --prefix 02 --fringe-factor 1 --min-distance 1500.0 --seed 28 --route-file out.rou.xml --weights-output-prefix weights --additional-file Quebec_data_2\\osm.add.xml --validate')
system('python "C:\\Program Files (x86)\\Eclipse\\Sumo\\tools\\randomTrips.py" -n Quebec_data_2\\osm.net.xml -o Quebec_data_2\\rushPM.passenger.trips.xml -b 54000 -e 64800 -p  0.3 --prefix 04  --fringe-factor 1 --min-distance 1500.0 --seed 28 --route-file out.rou.xml --weights-output-prefix weights --additional-file Quebec_data_2\\osm.add.xml --validate')


# Then we add the files generated into our osm.sumocfg file in order to start the simulation

# We modify all the hyperparameters in osm.sumoccfg

# We run the simulation
system('sumo.exe -c Quebec_data_2\\osm.sumocfg')
