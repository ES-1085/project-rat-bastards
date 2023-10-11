# data

Place data file(s) in this folder.

Then, include codebooks (variables, and their descriptions) for your data file(s)
using the following format.

## name of data file

- `variable1`: Description of variable 1
- `variable2`: Description of variable 2
- `variable3`: Description of variable 3
- ...

## hurricane_plants.csv

- `date`: The date that the observation of the plant was captured
- `life_form`: The category of form that the plant takes (shrub, vine, tree, herb)
- `species`: The Latin name for the species of plant observed
- `initial_emergence`: Logical variable that signifies the presence of an initial emergence or new growth of the plant
- `breaking_leaf_buds_count`: A discrete count of amount of leaf buds on the plant
- `leaf_presence `: Logical variable that signifies the presence of young unfolded leaves
- `percent_unfolded_leaves`: The percent of leaves unfolded
- `unfolded_leaves_count`: Count of the amount of leaves unfolded
- `percent_full_size_leaf`: The percent of leaves that are full sized
- `percent_leaves_colorful`: The percent of leaves that are displaying fall color
- `fallen_leaf_presence`: Logical variable that signifies the presence of fallen leaves
- `breaking_needle_bud_count `: Count of the amount of needle buds which are breaking
- `young_needle_count`: Count of the amount of young needle bundles which have emerged
- `percent_stalk_growth`: The percent of a fern stalk that has grown
- `percent_fiddlehead_unrolled`: The percent of a fern fiddlehead that has unrolled
- `buds_and_flowers_count`: Count of the amount of flowers or buds present on a plant
- `percent_open_flowers`: Percent of flowers which have opened, ready for pollination
- `pollen_amount `: The amount of pollen present on a plant, represented by a ordinal categorical variable (little, some, lots)
- `pollen_cone_count`: Count of the amount of pollen cones on the plant
- `percent_open_pollen_cones`: Percent of pollen cones open on the plant
- `fruit_count`: Count of the amount of fruits on the plant
- `unripe_seed_cone_count`: Count of the amount of unripe seeds on a plant
- `percent_ripe_fruits`: Percent of the fruits that are fully ripened on the plant
- `ripe_seed_cone_count `: Count of the amount of seed cones which are fully ripe
- `dropped_fruit_count`: Count of the amount of dropped or removed fruits or seeds
- `notes`: Any notes recorded for the individual on the date observed, recorded as a string
