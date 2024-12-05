package day2

import fileReaders.KotlinFileReader
import kotlin.math.abs

fun main() {
    val input = KotlinFileReader.readInputOfDay(2)

    val safeReportsPart1 = input.useLines {
        it.count { report ->
            isReportSafe(rawReportToList(report))
        }
    }
    println("Part 1: $safeReportsPart1")

    val safeReportsPart2 = input.useLines {
        it.count { report ->
            isReportSafeWithDampener(rawReportToList(report))
        }
    }
    println("Part 2 (honest attempt): $safeReportsPart2")

    val safeReportsPart2BruteForce = input.useLines {
        it.count { report ->
            val reportAsList = rawReportToList(report)

            if (isReportSafe(reportAsList)) {
                return@count true
            }
            reportAsList.forEachIndexed { i, _ ->
                if (isReportSafe(reportAsList.filterIndexed { j, _ -> i != j })) {
                    return@count true
                }
            }
            false
        }
    }
    println("Part 2 (brute force): $safeReportsPart2BruteForce")

    // Let's debug the honest attempt
    val honestAttemptRes = input.useLines {
        it.filter { report ->
            isReportSafeWithDampener(rawReportToList(report))
        }.toList()
    }
    val bruteForceRes = input.useLines {
        it.filter { report ->
            val reportAsList = rawReportToList(report)

            if (isReportSafe(reportAsList)) {
                return@filter true
            }
            reportAsList.forEachIndexed { i, _ ->
                if (isReportSafe(reportAsList.filterIndexed { j, _ -> i != j })) {
                    return@filter true
                }
            }
            false
        }.toList()
    }
    // Diff
    val onlyInHonest = honestAttemptRes.filter { it !in bruteForceRes }
    val onlyInBruteForce = bruteForceRes.filter { it !in honestAttemptRes }
    println("Only in honest: $onlyInHonest")
    println("Only in bruteForce: $onlyInBruteForce")
}

fun isPairSafe(i_leftValue: Int, i_rightValue: Int, i_increasing: Boolean): Boolean {
// check distance
    if (abs(i_leftValue - i_rightValue) > 3) return false

    // check consistency in increasing or decreasing pattern
    if (i_leftValue == i_rightValue) return false

    val pairIsIncreasing = i_leftValue < i_rightValue
    if (i_increasing && !pairIsIncreasing) return false
    if (!i_increasing && pairIsIncreasing) return false

    return true
}

fun rawReportToList(i_report: String): List<Int> {
    return i_report.split(" ").map { it.toInt() }
}

fun isReportSafe(i_report: List<Int>): Boolean {
    var increasing: Boolean? = null

    return i_report.zipWithNext().all { (previous, current) ->
        if (increasing == null) {
            // initialize flag
            increasing = previous < current
        }
        isPairSafe(previous, current, increasing!!)
    }
}

// I figured that the cases where this method is wrong is when eliminating the first element fixes the list
// but the first pair of elements is safe
// honestly, since the brute force approach works, I won't bother fixing it ¯\_(ツ)_/¯
fun isReportSafeWithDampener(i_report: List<Int>): Boolean {
    var increasing: Boolean? = null
    var dampenedError = false

    var i = 0
    while (i < i_report.size - 1) {
        val previous = i_report[i]
        val current = i_report[i + 1]

        if (isPairSafe(previous, current, increasing ?: (previous < current))) {
            increasing = increasing ?: (previous < current)
            ++i
            continue
        }

        if (dampenedError) {
            // already used the dampener!
            return false
        }

        dampenedError = true
        if (i == 0) {
            // special case, failed on the first iteration
            // skip it and try again
            ++i
            continue
        }

        if (i + 2 == i_report.size) {
            // removing the last value fixes it!
            // return already, nothing to do
            return true
        }
        if (!isPairSafe(previous, i_report[i + 2], increasing!!)) {
            // couldn't dampen error
            return false
        }
        // skip next iteration
        i += 2
    }

    return true
}