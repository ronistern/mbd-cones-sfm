#!/bin/bash

if [ $# -eq 0 ]; then
    echo ""
    echo "USAGE: ./run.sh <algorithm index> <system file> <obs index>"
    echo "       - algorithm index 1 = no cones"
    echo "       - algorithm index 2 = pessimistic"
    echo "       - algorithm index 3 = weak optimistic"
    echo "       - algorithm index 4 = strong optimistic"
    echo "       - algorithm index 5 = smart weak optimistic"
    echo "       - algorithm index 6 = smart strong optimistic"
    echo "USAGE: ./run.sh script <system id> <timeout>"
    echo "USAGE: ./run.sh raw2res <system id>"
    echo "USAGE: ./run.sh raw2csv <system id>"
    echo "USAGE: ./run.sh createOutputFolder <system id>"
    echo ""
    exit 1
fi

if [ "$1" = "1" ]
then
  swipl -G100g -T20g -L1g -g "['code/main.pl'], nl, solveObs_no_cones('$2', iscas85, $3), nl, halt" -t 'halt(1).'
  exit
fi

if [ "$1" = "2" ]
then
  swipl -G100g -T20g -L1g -g "['code/main.pl'], nl, solveObs_pessimistic('$2', iscas85, $3), nl, halt" -t 'halt(1).'
  exit
fi

if [ "$1" = "3" ]
then
  swipl -G100g -T20g -L1g -g "['code/main.pl'], nl, solveObs_weak_optimistic('$2', iscas85, $3), nl, halt" -t 'halt(1).'
  exit
fi

if [ "$1" = "4" ]
then
  swipl -G100g -T20g -L1g -g "['code/main.pl'], nl, solveObs_strong_optimistic('$2', iscas85, $3), nl, halt" -t 'halt(1).'
  exit
fi

if [ "$1" = "5" ]
then
  swipl -G100g -T20g -L1g -g "['code/main.pl'], nl, solveObs_smart_weak_optimistic('$2', iscas85, $3), nl, halt" -t 'halt(1).'
  exit
fi

if [ "$1" = "6" ]
then
  swipl -G100g -T20g -L1g -g "['code/main.pl'], nl, solveObs_smart_strong_optimistic/3('$2', iscas85, $3), nl, halt" -t 'halt(1).'
  exit
fi

if [ "$1" = "script" ]
then
  swipl -G100g -T20g -L1g -g "['code/main.pl'], nl, createScriptFile('$2', iscas85, $3), nl, halt" -t 'halt(1).'
  exit
fi

if [ "$1" = "raw2res" ]
then
  swipl -G100g -T20g -L1g -g "['code/main.pl'], nl, raw2results('$2'), nl, halt" -t 'halt(1).'
fi

if [ "$1" = "raw2csv" ]
then
  swipl -G100g -T20g -L1g -g "['code/main.pl'], nl, raw2csv('$2'), nl, halt" -t 'halt(1).'
fi

if [ "$1" = "createOutputFolder" ]
then
  swipl -G100g -T20g -L1g -g "['code/main.pl'], nl, createNewOutputFolder('$2'), nl, halt" -t 'halt(1).'
fi