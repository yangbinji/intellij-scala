package org.jetbrains.plugins.scala.lang.completion3

import com.intellij.codeInsight.completion.CompletionType
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter.normalize
import org.jetbrains.plugins.scala.codeInsight.ScalaCodeInsightTestBase
import org.jetbrains.plugins.scala.lang.completion.ScalaTextLookupItem
import org.junit.Assert

/**
  * Created by Kate Ustiuzhanin on 24/03/2017.
  */
class ScalaUnresolvedCompletionTest extends ScalaCodeInsightTestBase {

  def testFieldVal(): Unit = {
    val fileText =
      """
        |class Test {
        |  def method(): Unit ={
        |    val doubleValue = 34.4
        |    if (doubleValue > intValue) {
        |      methodWithParams("Hey!")
        |    } else {
        |      methodWithoutParams()
        |    }
        |  }
        |
        |  val <caret>
        |}
      """

    doTest(normalize(fileText), Seq("intValue"), Seq("methodWithParams(s: String)", "methodWithoutParams()"))
  }


  def testFieldVar(): Unit = {
    val fileText =
      """
        |def foo(a: Int, b: Int): Unit = {
        |  var <caret>
        |  val mid = a + (b - a) / 2
        |
        |  if (mid == value) {
        |    field1 = "got result"
        |    println(field1)
        |  } else if (mid > value) {
        |    foo1(a, mid - 1)
        |  } else {
        |    foo2(mid + 1, value)
        |  }
        |}
      """

    doTest(fileText, Seq("field1", "value"), Seq("foo1(i: Int, i1: Int)", "foo2(i: Int, value: Any)"))
  }

  def testMethodWithUnresolvedParams(): Unit = {
    val fileText =
      """
        |def foo(a: Int, b: Int): Unit = {
        |  def <caret>
        |  val mid = a + (b - a) / 2
        |
        |  if (mid == value) {
        |    field1 = "got result"
        |    println(field1)
        |  } else if (mid > value) {
        |    foo1(a, mid - 1)
        |  } else {
        |    foo2(mid + 1, value)
        |  }
        |}
      """

    doTest(fileText, Seq("field1", "value", "foo1(i: Int, i1: Int)", "foo2(i: Int, value: Any)"))
  }

  def testMethod(): Unit = {
    val fileText =
      """
        |class Test {
        |  def method(): Unit ={
        |    val doubleValue = 34.4
        |    if (doubleValue > intValue) {
        |      methodWithParams("Hey!")
        |    } else {
        |      methodWithoutParams()
        |    }
        |  }
        |
        |  def <caret>
        |}
      """

    doTest(normalize(fileText), Seq("methodWithParams(str: String)", "methodWithoutParams()", "intValue"))
  }

  def testMethodWithNamedParams(): Unit = {
    val fileText =
      """
        |class Test {
        |  def method(): Unit = {
        |    val doubleValue = 34.4
        |    if (doubleValue > intValue) {
        |      methodWithParams(a = "Hey!", b = 4, 23)
        |    } else {
        |      methoda(12, 23, " ", 23.3,  34, " ")
        |    }
        |  }
        |
        |  def <caret>
        |}
      """

    doTest(normalize(fileText),
      Seq("methodWithParams(a: String, b: Int, i: Int)", "methoda(i: Int, i1: Int, str: String, d: Double, i2: Int, str1: String)", "intValue"))
  }

  def testObject(): Unit = {
    val fileText =
      """
        |class Test {
        |  def method(): Unit ={
        |    val doubleValue = 34.4
        |    if (doubleValue > intValue) {
        |      methodWithParams("Hey!")
        |    } else {
        |      methodWithoutParams()
        |    }
        |  }
        |
        |  object <caret>
        |}
      """

    doTest(normalize(fileText), Seq("intValue"))
  }

  def testCaseClass(): Unit = {
    val fileText =
      """
        |class Test {
        |  def method(): Unit ={
        |    val doubleValue = 34.4
        |    if (doubleValue > intValue) {
        |      methodWithParams("Hey!")
        |    } else {
        |      methodWithoutParams()
        |    }
        |  }
        |
        |  case class <caret>
        |}
      """

    doTest(normalize(fileText), Seq("methodWithParams(str: String)", "methodWithoutParams()"), Seq("intValue"))
  }

  def testClass(): Unit = {

    val fileText =
      """
        |object Test{
        | sealed trait <caret>
        |
        | case class ClassWithParams(firstParam: Long, secondParam: Option[String]) extends Base
        |
        | println(foo(3, 4))
        |
        | val typedVal: NewType = "Hey!"
        |}
      """

    doTest(normalize(fileText), Seq("Base", "NewType"), Seq("foo"))
  }

  def testTypeAlias(): Unit = {
    val fileText =
      """
        |object Test{
        | type <caret>
        |
        | case class ClassWithParams(firstParam: Long, secondParam: Option[String]) extends Base
        |
        | val typedVal: NewType = "Hey!"
        |
        | println(unresolved)
        |}
      """

    doTest(normalize(fileText), Seq("Base", "NewType"), Seq("unresolved"))
  }

  def testClassAfterNew(): Unit = {
    val fileText =
      """
        |class <caret>
        |new Test(12, "hi!")
      """.stripMargin

    doTest(normalize(fileText), Seq("Test(i: Int, str: String)"))
  }

  private def doTest(fileText: String, includedSeq: Seq[String], excludedSeq: Seq[String] = Seq.empty): Unit = {

    configureFromFileTextAdapter("dummy.scala", fileText.stripMargin.replaceAll("\r", "").trim)
    val (activeLookup, _) = complete(1, CompletionType.BASIC)

    val expected = activeLookup.filter(_.isInstanceOf[ScalaTextLookupItem]).map(_.getLookupString).sorted

    Assert.assertEquals(null, expected.mkString("\n"), includedSeq.sorted.mkString("\n"))
    assertContaints(expected, excludedSeq)
  }


  private def assertContaints(expected: Seq[String], excluded: Seq[String]): Unit = {
    val intersection = expected.intersect(excluded)
    assert(intersection.isEmpty, "Completion list contains unexpected elements:\n" + intersection.mkString("\n"))
  }
}
