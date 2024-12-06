enum Direction
{
    UpLeft,
    Up,
    UpRight,
    Left,
    Right,
    DownLeft,
    Down,
    DownRight
}

public class WordSearch
{

    static List<List<char>> ConvertInputToCharMatrix(string i_input)
    {
        List<List<char>> result = new List<List<char>>();
        string[] lines = i_input.Split('\n');
        foreach (string line in lines)
        {
            result.Add(line.ToCharArray().ToList());
        }
        return result;
    }

    static (int, int) AdvanceInDirection(Direction i_direction, int i_rowIndex, int i_columnIndex)
    {
        switch (i_direction)
        {
            case Direction.UpLeft:
                return (i_rowIndex - 1, i_columnIndex - 1);
            case Direction.Up:
                return (i_rowIndex - 1, i_columnIndex);
            case Direction.UpRight:
                return (i_rowIndex - 1, i_columnIndex + 1);
            case Direction.Left:
                return (i_rowIndex, i_columnIndex - 1);
            case Direction.Right:
                return (i_rowIndex, i_columnIndex + 1);
            case Direction.DownLeft:
                return (i_rowIndex + 1, i_columnIndex - 1);
            case Direction.Down:
                return (i_rowIndex + 1, i_columnIndex);
            case Direction.DownRight:
                return (i_rowIndex + 1, i_columnIndex + 1);
        }
        // should never happen
        return (i_rowIndex, i_columnIndex);
    }


    static bool IsIndexValid(List<List<char>> i_matrix, int i_rowIndex, int i_columnIndex)
    {
        if (i_rowIndex < 0 || i_matrix.Count <= i_rowIndex)
        {
            return false;
        }
        if (i_columnIndex < 0 || i_matrix[i_rowIndex].Count <= i_columnIndex)
        {
            return false;
        }
        return true;
    }

    static bool IsValidCrossMas(List<List<char>> i_matrix, int i_rowIndex, int i_columnIndex)
    {
        if (i_matrix[i_rowIndex][i_columnIndex] != 'A')
        {
            return false;
        }

        Direction[] diagonalLeft = { Direction.UpLeft, Direction.DownRight };
        Direction[] diagonalRight = { Direction.UpRight, Direction.DownLeft };
        Direction[][] diagonals = { diagonalLeft, diagonalRight };

        foreach (Direction[] diagonal in diagonals)
        {
            List<char> valuesToFind = new() { 'M', 'S' };
            foreach (Direction direction in diagonal)
            {
                (int, int) nextIndexes = AdvanceInDirection(direction, i_rowIndex, i_columnIndex);
                int rowIndex = nextIndexes.Item1;
                int columnIndex = nextIndexes.Item2;
                if (!IsIndexValid(i_matrix, rowIndex, columnIndex))
                {
                    return false;
                }
                char elementInIndex = i_matrix[rowIndex][columnIndex];
                if (valuesToFind.Contains(elementInIndex))
                {
                    valuesToFind.Remove(elementInIndex);
                }
                else
                {
                    // Does not form a cross MAS
                    return false;
                }
            }
        }
        return true;
    }

    static uint CountCrossMas(List<List<char>> i_matrix)
    {
        uint count = 0;
        for (int i = 0; i < i_matrix.Count; ++i)
        {
            for (int j = 0; j < i_matrix[i].Count; ++j)
            {
                if (IsValidCrossMas(i_matrix, i, j))
                {
                    ++count;
                }
            }
        }
        return count;
    }

    static uint CountValidXmas(List<List<char>> i_matrix, int i_rowIndex, int i_columnIndex)
    {
        if (i_matrix[i_rowIndex][i_columnIndex] != 'X')
        {
            return 0;
        }

        uint count = 0;
        char[] expectedSequence = { 'M', 'A', 'S' };

        foreach (Direction direction in Enum.GetValues(typeof(Direction)))
        {
            int rowIndex = i_rowIndex;
            int columnIndex = i_columnIndex;
            bool isValidDirection = true;
            foreach (char expectedChar in expectedSequence)
            {
                (int, int) nextIndexes = AdvanceInDirection(direction, rowIndex, columnIndex);
                rowIndex = nextIndexes.Item1;
                columnIndex = nextIndexes.Item2;
                if (!IsIndexValid(i_matrix, rowIndex, columnIndex))
                {
                    isValidDirection = false;
                    break;
                }
                if (i_matrix[rowIndex][columnIndex] != expectedChar)
                {
                    isValidDirection = false;
                    break;
                }
            }
            if (isValidDirection)
            {
                ++count;
            }
        }
        return count;
    }

    static uint CountXmas(List<List<char>> i_matrix)
    {
        uint count = 0;
        for (int i = 0; i < i_matrix.Count; ++i)
        {
            for (int j = 0; j < i_matrix[i].Count; ++j)
            {
                count += CountValidXmas(i_matrix, i, j);
            }
        }
        return count;
    }

    static void Main(string[] args)
    {
        string input = CsharpFileReader.ReadInputOfDay(4);
        List<List<char>> matrix = ConvertInputToCharMatrix(input);
        Console.WriteLine("Part 1: " + CountXmas(matrix));
        Console.WriteLine("Part 2: " + CountCrossMas(matrix));
    }
}