package cz.cvut.fit.prl.scala.implicits.extractor
import org.scalatest.Matchers

import scala.meta.testkit.DiffAssertions

trait CaseClassAssertions extends DiffAssertions with Matchers {

  def checkElementsSorted[T:Ordering](actual: Iterable[T], expected: Iterable[T]): Unit = {
    checkElements(actual.toList.sorted, expected.toList.sorted)
  }

  def checkElements[T](actual: Iterable[T], expected: Iterable[T]): Unit = {
    actual.size shouldBe expected.size

    actual.zip(expected).foreach { case (a, e) => checkElement(a, e) }
  }

  def checkElement[T](actual: T, expected: T): Unit = {
    if (actual != expected) {
      val expectedText =
        pprint.apply(expected, height = Integer.MAX_VALUE, indent = 2).plainText
      val actualText =
        pprint.apply(actual, height = Integer.MAX_VALUE, indent = 2).plainText

      assertNoDiffOrPrintExpected(expectedText, actualText)
    }
  }
}
