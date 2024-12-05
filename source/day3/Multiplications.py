from fileReaders.PythonFileReader import read_input_of_day
import re
from functools import reduce


def find_valid_mul_commands(input: list[str]) -> list[str]:
    # using this regex we'll already get a list of tuples
    regex = "mul\((?P<first>\d*),(?P<second>\d*)\)"

    asSingleString = ''.join(input.readlines())
    return re.findall(regex, asSingleString)


def run_mul_commands(input: list[str]):
    valid_commands = find_valid_mul_commands(input)
    return reduce((lambda acc, pair: acc + int(pair[0]) * int(pair[1])), valid_commands, 0)


if __name__ == "__main__":
    input = read_input_of_day(3)
    print("Part 1: {}".format(run_mul_commands(input)))
