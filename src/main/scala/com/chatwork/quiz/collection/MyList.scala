package com.chatwork.quiz.collection

import com.chatwork.quiz.{MySome, MyNone, MyOption}

import scala.annotation.tailrec

sealed trait MyList[+A] {

  // Easy
  def length: Int = {
    @tailrec
    def loop(num: Int, list: MyList[A]): Int = list match {
      case MyNil         => num
      case MyCons(x, xs) => loop(num + 1, xs)
    }
    loop(0, this)
  }

  // Normal
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(result: B, myList: MyList[A]):B = myList match {
      case MyNil => result
      case MyCons(x, xs) => loop(f(result, x), xs)
    }
    loop(z, this)
  }

  // 難易度選択制
  // Normal: 条件 - 特にありません、気の向くままに実装してください。
  // Hard:   条件 - foldLeftを使って実装してください。
  def foldRight[B](z: B)(f: (A, B) => B): B = {
    @tailrec
    def loop(result: B, myList: MyList[A]):B = myList match {
      case MyNil => result
      case MyCons(x, xs) => loop(f(x, result), xs)
    }
    loop(z, this.reverse)
//    reverse.foldLeft(z)(f)
  }

  // Normal
  // scalastyle:off
  def ::[B >: A](b: B): MyList[B] = {
    MyCons(b, this)
  }
  // scalastyle:on

  // Normal
  def reverse: MyList[A] = {
    @tailrec
    def loop(result: MyList[A], myList: MyList[A]): MyList[A] = myList match {
      case MyNil => result
      case MyCons(x, xs) => loop(MyCons(x, result), xs)
    }
    loop(MyNil, this)
  }

  // Normal
  // scalastyle:off
  def ++[B >: A](b: MyList[B]): MyList[B] = {
    @tailrec
    def loop(result: MyList[B], myList: MyList[A]): MyList[B] = myList match {
      case MyNil => result
      case MyCons(x, xs) => loop(MyCons(x, result), xs)
    }
    loop(b,this.reverse)
  }
  // scalastyle:on

  // Normal
  def map[B](f: A => B): MyList[B] = {
    @tailrec
    def loop(result: MyList[B], myList: MyList[A]): MyList[B] = myList match {
      case MyNil => result
      case MyCons(x, xs) => loop(MyCons(f(x), result), xs)
    }
    loop(MyNil, this.reverse)
  }

  // Normal
  def flatMap[B](f: A => MyList[B]): MyList[B] = {
    @tailrec
    def loop(result: MyList[B], myList: MyList[A]): MyList[B] = myList match {
      case MyNil => result
      case MyCons(x, xs) => loop(result ++ f(x), xs)
    }
    loop(MyNil, this)
  }

  // Normal
  def filter(f: A => Boolean): MyList[A] = {
    @tailrec
    def loop(result: MyList[A], myList: MyList[A]): MyList[A] = myList match {
      case MyNil => result
      case MyCons(x, xs) if f(x) => loop(MyCons(x, result), xs)
      case MyCons(x, xs) => loop(result, xs)
    }
    loop(MyNil, this.reverse)
  }

  // Normal: 条件 - filterと同様の実装でも構いません。
  // Hard:   条件 - 中間リストを生成しないように実装してください。
  def withFilter(f: A => Boolean): MyList[A] = filter(f)

  // Normal
  def find(f: A => Boolean): MyOption[A] = this match {
    case MyNil => MyNone
    case MyCons(x, xs) if f(x) => MySome(x)
    case MyCons(x, xs) => xs.find(f)
  }

  // Normal
  def startsWith[B >: A](prefix: MyList[B]): Boolean = ???

}

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  // Easy
  def empty[A]: MyList[A] = MyNil

  // Normal
  def apply[A](as: A*): MyList[A] = {
    @tailrec
    def loop(list: MyList[A], as: A*): MyList[A] = {
      if (as.isEmpty) list
      else loop(MyCons(as.head, list), as.tail: _*)
    }
    loop(empty, as.reverse: _*)
  }

}
