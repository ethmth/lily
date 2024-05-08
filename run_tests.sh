#!/bin/bash

if ! [[ $EUID -ne 0 ]]; then
	echo "This script should not be run with root/sudo privileges."
	exit 1
fi

LLI_COMMAND="lli-16"
MAKE_DIR="src"
MAKE_TARGET="lily"
EXECUTABLE_NAME="lily.native"

TEST_DIR="test"
TEST_EXT=".lily"
OUT_EXT=".out"

WORKING_DIR="working"
PARSER_DIR="parser_error"
PARSER_ERROR="Parse_error"
SEMANT_DIR="semantics_error"
SEMANT_ERROR="Semantics Error"
SCANNER_DIR="scanner_error"
SCANNER_ERROR="Scanner Error"

# Get script path
SCRIPT_PATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

# Make LILY compiler
cd $MAKE_DIR
make $MAKE_TARGET

EXECUTABLE_PATH="$SCRIPT_PATH/$MAKE_DIR/$EXECUTABLE_NAME"

# Create a list of directories
directories=(
    "$SCRIPT_PATH/$TEST_DIR/$WORKING_DIR"
    # "$SCRIPT_PATH/$TEST_DIR/$SCANNER_DIR"
    "$SCRIPT_PATH/$TEST_DIR/$PARSER_DIR"
    "$SCRIPT_PATH/$TEST_DIR/$SEMANT_DIR"
)

# And their matching error message
actions=(
    "OUTFILE"
    # "$SCANNER_ERROR"
    "$PARSER_ERROR"
    "$SEMANT_ERROR"
)

count=0
total=0

# Iterate over each directory specified in $directories
for i in "${!directories[@]}"; do
    directory="${directories[$i]}"
    action="${actions[$i]}"

    curr_count=0
    curr_total=0

    printf "========================================================\n"
    printf "\tRunning tests for $action\n"
    printf "========================================================\n"
    for file in "$directory"/*; do
        passed=1
        if ! [[ -f "$file" && "$file" == *.lily ]]; then
            continue
        fi
        base_file=$(basename -- "$file")
        base_file="${base_file%.*}"

        output=$(cat $file | $EXECUTABLE_PATH 2>&1)

        if [ "$action" == "OUTFILE" ]; then
            output=$(echo $output | $LLI_COMMAND 2>&1)
            output="${output#"${output%%[![:space:]]*}"}"
            output="${output%"${output##*[![:space:]]}"}"
            if ! [ -f "$directory/$base_file$OUT_EXT" ]; then
                passed=0
                output="$base_file$OUT_EXT does not exist."
            else
                expected_output=$(cat "$directory/$base_file$OUT_EXT")
                expected_output="${expected_output#"${expected_output%%[![:space:]]*}"}"
                expected_output="${expected_output%"${expected_output##*[![:space:]]}"}"
                if [ "$output" != "$expected_output" ]; then 
                    passed=0
                fi
            fi
        else
            target_string=$action
            if ! [[ "$output" == *"$target_string"* ]]; then
                passed=0
            fi
        fi

        if ((passed)); then
            ((curr_count=curr_count+1))
            ((count=count+1))
            printf "\t\tPassed $base_file\n"
        else
            printf "\t\t\e[1;37mFAILED\e[0m $base_file test: Expected $action, got:\n"
            echo -e "\e[1;31m$output\e[0m"
        fi
        ((total=total+1))
        ((curr_total=curr_total+1))
    done

    printf "========================================================\n"
    if [ "$curr_count" == "$curr_total" ]; then
        printf "\tPassed $curr_count/$curr_total tests for $action\n"
    else
        printf "\t\e[1;37mFAILED\e[0m tests for $action with $curr_count/$curr_total correct\n"
    fi
    printf "========================================================\n"
done

if [ "$count" == "$total" ]; then
    printf "All $total TESTS Passed.\n"
else
    printf "\e[1;37mFAILED\e[0m: Only passed $count/$total tests.\n"
fi