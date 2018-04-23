# ./timeoutscript.sh -t300 swipl -G100g -T20g -L1g -g "['code/main.pl'], nl, solveObs_pessimistic('c1355_0.25', iscas85, 1), halt" -t 'halt(1).'
./timeoutscript.sh -t300 swipl -G100g -T20g -L1g -g "['code/sandbox.pl'], nl, test_appendToEndOfList, halt" -t 'halt(1).'
