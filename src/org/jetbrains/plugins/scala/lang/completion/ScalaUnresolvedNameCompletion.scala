package org.jetbrains.plugins.scala.lang.completion

import com.intellij.codeInsight.completion._
import com.intellij.codeInsight.lookup.{LookupElement, LookupElementWeigher}
import com.intellij.patterns.PlatformPatterns
import com.intellij.psi._
import com.intellij.psi.util.PsiTreeUtil.getNextSiblingOfType
import com.intellij.util.ProcessingContext
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.ScalaRecursiveElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReferenceElement
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDeclaration, ScTypeAliasDeclaration}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.base.ScFieldIdImpl
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.Any
import org.jetbrains.plugins.scala.lang.psi.types.result.{Failure, Success}
import org.jetbrains.plugins.scala.lang.refactoring.namesSuggester.NameSuggester
import org.jetbrains.plugins.scala.lang.resolve.ResolveTargets._

import scala.collection.mutable

/** Contributer adds unresolved names in current scope to completion list.
  * Unresolved reference name adds to completion list, according to [[ScReferenceElement.getKinds()]]
  *
  * Usages:
  * def <caret>
  * val/var <caret>
  * type <caret>
  * trait <caret>
  * [case] class/object <caret>
  *
  * For elements with parameters(methods, classes), completion list contains parameters with types.
  * Created by  Kate Ustyuzhanina on 17/03/2017.
  */
class ScalaUnresolvedNameCompletion extends ScalaCompletionContributor {
  extend(CompletionType.BASIC, PlatformPatterns.psiElement(), new CompletionProvider[CompletionParameters] {
    override def addCompletions(completionParameters: CompletionParameters, processingContext: ProcessingContext, completionResultSet: CompletionResultSet): Unit = {
      val position = positionFromParameters(completionParameters)

      position.getContext match {
        case (_: ScTypeDefinition | _: ScTypeAliasDeclaration | _: ScFunctionDeclaration | _: ScFieldIdImpl) =>
          val result = addElementsOrderSorter(completionParameters, completionResultSet)
          runCompletion(position, result)
        case _ =>
      }
    }
  })

  def runCompletion(position: PsiElement, completionResultSet: CompletionResultSet): Unit = {
    val scope = computeScope(position)
    val unresolvedNames = unresolvedReference(scope, position.getContext)

    unresolvedNames.foreach { case (name, args) =>
      completionResultSet.addElement(ScalaTextLookupItem(createTextForLookupElement(name, args)))
    }
  }

  def unresolvedReference(scope: PsiElement, declarationType: PsiElement): Seq[(String, Option[ScArgumentExprList])] = {
    val buffer = new mutable.HashSet[(String, Option[ScArgumentExprList])]()

    def nonResolved(implicit ref: PsiReference): Boolean = ref.resolve() == null

    def isKindAcceptible(implicit ref: ScReferenceElement) = {
      def refWithMethodCallParent = ref.parent.exists(_.isInstanceOf[ScMethodCall])

      def refKinds = ref.getKinds(incomplete = false)

      declarationType match {
        case _: ScFieldIdImpl if !refWithMethodCallParent =>
          refKinds.contains(VAL) || refKinds.contains(VAR)
        case _: ScObject if !refWithMethodCallParent => refKinds.contains(OBJECT)
        case cclass: ScClass if cclass.isCase && refWithMethodCallParent =>
          refKinds.contains(OBJECT) || refKinds.contains(CLASS)
        case _: ScTypeDefinition | _: ScTypeAliasDeclaration => refKinds.contains(CLASS)
        case _: ScFunctionDeclaration => refKinds.contains(METHOD)
        case _ => false
      }
    }

    def argList(implicit ref: ScReferenceElement): Option[ScArgumentExprList] =
      ref.parent.flatMap {
        case mc: ScMethodCall => Option(mc.args)
        case psiElement => Option(getNextSiblingOfType(psiElement, classOf[ScArgumentExprList]))
      }

    def handleRef(implicit ref: ScReferenceElement): Unit = {
      def parentIsRef = ref.parent.exists(_.isInstanceOf[PsiReference])

      if (!parentIsRef && !reservedNames.contains(ref.refName) && isKindAcceptible && nonResolved) {
        buffer.add((ref.refName, argList(ref)))
      }
    }

    scope.accept(new ScalaRecursiveElementVisitor() {
      override def visitReference(reference: ScReferenceElement): Unit = {
        implicit val ref: ScReferenceElement = reference
        handleRef
      }

      override def visitAssignmentStatement(stmt: ScAssignStmt): Unit = {
        if (!stmt.parent.exists(_.isInstanceOf[ScArgumentExprList])) {
          super.visitAssignmentStatement(stmt) //don't add named params to completion list
        }
      }
    })

    buffer.toSeq
  }

  protected def createTextForLookupElement(name: String, args: Option[ScArgumentExprList]): String = {
    case class Parameter(name: Option[String], `type`: Option[ScType]) {
      override def toString: String = name.zip(`type`).map { case (n, t) => s"$n: $t" }.headOption.getOrElse("")
    }

    def computeType(exprs: ScExpression): Option[ScType] =
      Option(exprs.getType() match {
        case Success(result, _) => result
        case Failure(_, _) => Any
      })

    val names: mutable.Map[String, Int] = mutable.HashMap.empty[String, Int]

    def suggestUniqueName(`type`: ScType): String = {
      val name = NameSuggester.suggestNamesByType(`type`).headOption.getOrElse("value")

      val result = if (names.contains(name)) name + names(name) else name
      names.put(name, names.getOrElse(name, 0) + 1)
      result
    }

    def handleOneExpression: ScExpression => Parameter = {
      case assign: ScAssignStmt =>
        Parameter(assign.assignName, assign.getRExpression.flatMap(computeType))
      case e =>
        val `type` = computeType(e)
        Parameter(`type`.map(suggestUniqueName), `type`)
    }

    val arguments = args.map(_.exprs.map(handleOneExpression).map(_.toString).mkString(", ")).map("(" + _ + ")").getOrElse("")

    name + arguments
  }

  private def computeScope(position: PsiElement): PsiElement = {
    val scopes = position.scopes
    if (scopes.hasNext) scopes.next()
    else position.getContainingFile
  }

  private def addElementsOrderSorter(parameters: CompletionParameters, result: CompletionResultSet): CompletionResultSet = {

    case class NameComparable(name: String) extends Comparable[NameComparable] {
      def compareTo(o: NameComparable): Int = {
        name.compareTo(o.name)
      }
    }

    class PreferByParamsOrder extends LookupElementWeigher("unresolvedOnTop") {
      override def weigh(item: LookupElement): Comparable[_] = {
        item match {
          case ScalaTextLookupItem(name) => NameComparable(name)
          case _ => null
        }
      }
    }

    var sorter = CompletionSorter.defaultSorter(parameters, result.getPrefixMatcher)
    sorter = sorter.weighAfter("prefix", new PreferByParamsOrder())
    result.withRelevanceSorter(sorter)
  }

  private val reservedNames =
    Seq(
      "+", "-", "*", "/", "%",
      "&&", "||", "&", "|", "==", "!=", "^",
      "<<", ">>", ">>>")
}

case class ScalaTextLookupItem(text: String) extends LookupElement {
  override def getLookupString: String = text
}


