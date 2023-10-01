#!/bin/bash

RED='\033[0;31m'
GREEN='\033[1;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

test_dir="auto-tests/src"
shift
prog_name=l22
DIFF="diff"

for test in $test_dir/*.l22; do
    test_name="${test%.l22}"
    echo -e "${YELLOW}Running test $test_name${NC}"
    ./$prog_name $test
    asm="$test_name.asm"
    yasm -felf32 $asm -o $test_name.o
    ld -m elf_i386 -o $test_name.bin $test_name.o -L$HOME/compiladores/root/usr/lib -lrts
    ./$test_name.bin > $test_name.myout

    $DIFF $test_name.myout $test_name.out
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}Test ${test_in} PASSED!\n${NC}"
        rm $test_name.myout
        rm $test_name.bin
        rm $test_name.o
        rm $asm
    else
        echo -e "${RED}Test ${test_in} FAILURE!\n${NC}"
        echo -e "${RED}Falhou este teste $test_name${NC}"

        exit 1
    fi

done