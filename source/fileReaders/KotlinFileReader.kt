package fileReaders

import java.io.File

class KotlinFileReader {
    companion object{
        fun readInputOfDay(day: Int): File {
            val dayPath = File("source", "day$day").absolutePath
            return File(dayPath + File.separator + "input.txt")
        }
    }
}