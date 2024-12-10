get-data day:
    curl --header "Cookie: ${AOC_COOKIE}" --output input/day{{day}}.txt https://adventofcode.com/2024/day/{{day}}/input
