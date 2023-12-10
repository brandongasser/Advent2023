for DAY in 1 2 3 4 5 6 7 8 9
do
for PART in 1 2
do
echo "Day $DAY Part $PART"
./run.bash $DAY $PART
done
done