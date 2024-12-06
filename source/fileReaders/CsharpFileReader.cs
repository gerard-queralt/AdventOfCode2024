public class CsharpFileReader
{
    public static string ReadInputOfDay(int day)
    {
        string dayInputPath = Path.Combine("day" + day, "input.txt");
        return File.ReadAllText(dayInputPath);
    }
}