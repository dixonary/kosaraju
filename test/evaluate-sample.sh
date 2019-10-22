#!/bin/bash

for I in {0..30}
do
    # Replace currentval in main
    sed -i '' -e "s/^currentVal.*$/currentVal = $I/" ../app/Main.hs
    stack build

    export KOSARAJU_SOLVER=z3
    Z3_TIME=`gtime --format="%e" stack exec -- kosaraju 2>&1`
    
    export KOSARAJU_SOLVER=cvc4
    CVC4_TIME=`gtime --format="%e" stack exec -- kosaraju 2>&1`

    echo -e "$I,$Z3_TIME,$CVC4_TIME" >> sample-results.csv
done