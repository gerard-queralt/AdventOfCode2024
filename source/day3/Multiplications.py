from fileReaders.PythonFileReader import read_input_of_day
import re
from functools import reduce


def find_valid_mul_and_dos_commands(input: list[str]) -> list[str]:
    # using this regex we'll already get a list of tuples
    regex = re.compile(
        r"mul\((?P<first>\d*),(?P<second>\d*)\)|(?P<do>do\(\))|(?P<dont>don't\(\))")

    asSingleString = ''.join(input)
    return regex.findall(asSingleString)


def run_mul_and_dos_commands(input: list[str]):
    valid_commands = find_valid_mul_and_dos_commands(input)
    acc = 0
    doing = True
    for command in valid_commands:
        first, second, do, dont = command
        if first and second:
            if doing:
                acc += int(first) * int(second)
        elif do:
            doing = True
        elif dont:
            doing = False
        else:  # should not happen
            print("Unknown command: {command}")
    return acc


def find_valid_mul_commands(input: list[str]) -> list[str]:
    # using this regex we'll already get a list of tuples
    regex = re.compile(r"mul\((?P<first>\d*),(?P<second>\d*)\)")

    asSingleString = ''.join(input)
    return regex.findall(asSingleString)


def run_mul_commands(input: list[str]):
    valid_commands = find_valid_mul_commands(input)
    acc = 0
    for command in valid_commands:
        first, second = command
        acc += int(first) * int(second)
    return acc


if __name__ == "__main__":
    input = read_input_of_day(3).readlines()
    print("Part 1: {}".format(run_mul_commands(input)))
    print("Part 2: {}".format(run_mul_and_dos_commands(input)))
