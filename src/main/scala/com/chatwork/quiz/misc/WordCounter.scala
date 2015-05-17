package com.chatwork.quiz.misc

/**
 * ワードをカウントするオブジェクト。
 */
object WordCounter {

  /**
   * 文字列から単語数をカウントする。
   *
   * @param words 文字列
   * @return 単語がキー、単語数がヴァリューのマップ
   */
  def countWords(words: List[String]): Map[String, Int] =
    words.map(_.split(' ')).flatMap(x => x).groupBy(identity).mapValues(_.length)
//    words.map(_.split(" ")).flatMap(x => x).groupBy(identity).mapValues(_.length)

}

