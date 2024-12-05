package day2

import fileReaders.KotlinFileReader
import kotlin.math.abs

fun main() {
    val input = KotlinFileReader.readInputOfDay(2)

    val safeReports = input.useLines {
        it.count { report ->
            isReportSafe(report)
        }
    }

    println("Part 1: $safeReports")
}

fun isReportSafe(i_report: String): Boolean {
    val values = i_report.split(" ").map { it.toInt() }

    var increasing: Boolean? = null

    return values.zipWithNext().all { (previous, current) ->
        // check distance
        if (abs(previous - current) > 3) return@all false

        // check consistency in increasing or decreasing pattern
        if (previous == current) return@all false

        val pairIsIncreasing = previous < current
        if (increasing == null) {
            // initialize flag
            increasing = pairIsIncreasing
        }
        if (increasing!! && !pairIsIncreasing) return@all false
        if (!increasing!! && pairIsIncreasing) return@all false

        true
    }
}