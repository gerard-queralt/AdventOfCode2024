from pathlib import Path


def read_input_of_day(day: int):
    source_path = Path(__file__).resolve().parent.parent
    day_path = source_path / f"day{day}"
    input_of_day = day_path / "input.txt"
    return input_of_day.open("r")
