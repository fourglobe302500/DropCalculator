# Drop Calculator

A basic program made for rolling drop tables for RPG mobs and hoards (D&D).

All data is stored [here](data/Drops.json). With the format defined [here](format.json)

## The commands currently implemented are:

- `list`: to list the loaded mobs.
- `exit`: to quit execution.
- `hoard <mob name>`: rolls a hoard for a mob \<mob name\>.
- `kill (<mob name> (count)?)+`: rolls the kill table for all mob name with their respective count.

## For the future:

- Exporting csv files with the drops.
- Accepting load tables mid execution (and as parameters)
- Add internal help