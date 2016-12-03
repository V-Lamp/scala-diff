package vlamp.scalaDiff

import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest._
import vlamp.scalaDiff.Delta.{Modified, Unchanged}
import vlamp.scalaDiff.Diff._

class DiffSpecs extends FunSuite with Matchers with PropertyChecks {
  
  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 100)
  
  def isValidDiff[T, D](implicit diff: Diff[T, D], gen: Arbitrary[T]) = {
    forAll { (first: T, second: T) =>
      val delta = diff(first, second)
      println(s"Delta: ($first,$second) => $delta")
      diff.recoverFirst(second, delta) shouldBe first
      diff.recoverSecond(first, delta) shouldBe second
      assert(first != second === diff.hasChanges(delta))
    }
  }
  
  test("Int")(isValidDiff[Int, Delta[Int]])
  test("String")(isValidDiff[String, Delta[String]])
  test("Option[Int]")(isValidDiff[Option[Int], Option[Delta[Int]]])
  test("Set[Int]")(isValidDiff[Set[Int], Set[Delta[Int]]])
  test("(Int, Int)")(isValidDiff[(Int, Int), (Delta[Int], Delta[Int])])
  
  test("Option[Set[Int]]")(isValidDiff[Option[Set[Int]], Option[Set[Delta[Int]]]])
  test("Map[String, Int]")(isValidDiff[Map[String, Int], Map[Delta[String], Delta[Int]]])
  test("Map[String, Set[Int]]")(isValidDiff[Map[String, Set[Int]], Map[Delta[String], Set[Delta[Int]]]])
  
  test("Map[String, Map[String,Int]]")(isValidDiff[Map[String, Map[String,Int]], Map[Delta[String], Map[Delta[String],Delta[Int]]]])
  
  test("Set[Map[String, Set[Int]]]")(isValidDiff[Set[Map[String, Set[Int]]], Set[Map[Delta[String], Set[Delta[Int]]]]])
  test("Set[Map[String, Set[Int]]]")(isValidDiff[Set[Map[String, Set[Int]]], Set[Map[Delta[String], Set[Delta[Int]]]]])
  test("Set[Map[String, Set[Int]]]")(isValidDiff[Set[(String,Int)], Set[(Delta[String],Delta[Int])]])
  
}
