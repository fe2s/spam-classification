package com.github.fe2s

import org.scalatest.FunSuite

/**
 * @author Oleksiy Dyagilev
 */
class TextUtilsTest extends FunSuite {

  test("test tokenize words no punctuation") {
    val words = TextUtils.tokenizeWords("the cow jumped over the moon")
    assert(words.sameElements(Array("the", "cow", "jumped", "over", "the", "moon")))
  }

  test("test tokenize words with punctuation") {
    val words = TextUtils.tokenizeWords("(the cow, jumped over the moon!)")
    assert(words.sameElements(Array("the", "cow", "jumped", "over", "the", "moon")))
  }
  
  test("words freq") {
    val str = "(the cow, jumped over the moon!)"
    val counts = TextUtils.countWordFreq(str, Set("the", "moon", "over", "aaa"))
    assert(counts == Map("the" -> 33.33, "moon" -> 16.67, "over" -> 16.67, "aaa" -> 0.0))
  }

  test("chars freq") {
    val str = "((the cow, jumped over the moon!))"
    val counts = TextUtils.countCharFreq(str, Set('(', '!', '#'))
    assert(counts == Map('(' -> 5.88, '!' -> 2.94, '#' -> 0.0))
  }


}
