package com.github.fe2s

/**
 * @author Oleksiy Dyagilev
 */
object TextUtils {

  def tokenizeWords(s: String): Array[String] = {
    s.replaceAll("[^a-zA-Z ]", "").split("\\s+")
  }

  def countWordFreq(s: String, wordsToCount: Set[String]): Map[String, Double] = {
    val wordsInString = tokenizeWords(s)
    countFreq(wordsInString, wordsToCount)
  }

  def countCharFreq(s: String, chars: Set[Char]): Map[Char, Double] = {
    countFreq(s, chars)
  }

  private def countFreq[T](coll: Iterable[T], itemsToCount: Set[T]): Map[T, Double] = {
    val initCountsMap = itemsToCount.map(i => (i, 0)).toMap
    val counts = coll.foldLeft(initCountsMap) { (map, item) =>
      if (itemsToCount.contains(item)) {
        map.updated(item, map(item) + 1)
      } else {
        map
      }
    }

    val totalItemsNum = coll.size

    def round(d: Double) = math.round(d * 100d) / 100d

    counts.mapValues(count => round(100d * count / totalItemsNum))
  }



}
