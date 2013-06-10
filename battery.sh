#! /bin/bash

# A better method; requires the acpi package

STATUS=$(acpi -b)
# Get percentage and charging status ("C"=charging, "D"=dischaging)
VALUE=$(echo $STATUS | cut -d ' ' -f 4 | sed 's/%.*//')
CHARGING=$(echo $STATUS | cut -d ' ' -f 3 | head -c 1)

# Calculate colour
if [ "$CHARGING" == "C" ]; then
	COLOUR="yellow"
else
	if [ "$VALUE" -gt "10" ]; then
		COLOUR="green"
	else
		COLOUR="red"
	fi
fi

# Format and print
echo "<fc=$COLOUR>$VALUE</fc>%"
