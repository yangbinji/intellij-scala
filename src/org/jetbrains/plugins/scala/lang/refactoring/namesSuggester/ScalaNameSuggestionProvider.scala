package org.jetbrains.plugins.scala
package lang
package refactoring
package namesSuggester

import java.{util => ju}

import com.intellij.psi.codeStyle.SuggestedNameInfo
import com.intellij.psi.{PsiElement, PsiNamedElement}
import com.intellij.refactoring.rename.NameSuggestionProvider
import org.jetbrains.plugins.scala.extensions.PsiNamedElementExt
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScNewTemplateDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypedDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable

/**
  * User: Alexander Podkhalyuzin
  * Date: 23.11.2008
  */
class ScalaNameSuggestionProvider extends AbstractNameSuggestionProvider {

  import NameSuggester.suggestNames
  import ScalaNameSuggestionProvider._

  override protected def suggestedNames(element: PsiElement): Seq[String] = element match {
    case definition: ScNewTemplateDefinition => suggestNames(definition)
    case definition: ScTemplateDefinition => Seq(definition.name)
    case typed: ScTypedDefinition =>
      typed.name +: suggestedNamesByType(typed)
    case named: PsiNamedElement => Seq(named.name)
    case expr: ScExpression => suggestNames(expr)
    case typeElement: ScTypeElement => suggestedNamesByType(typeElement)
    case _ => Seq.empty
  }
}

object ScalaNameSuggestionProvider {

  import NameSuggester.suggestNamesByType

  private def suggestedNamesByType(typeable: Typeable): Seq[String] =
    typeable.getType()
      .toOption.toSeq
      .flatMap(suggestNamesByType)
}

abstract class AbstractNameSuggestionProvider extends NameSuggestionProvider {

  override final def getSuggestedNames(element: PsiElement, context: PsiElement, result: ju.Set[String]): SuggestedNameInfo = {
    val names = suggestedNames(element)

    import scala.collection.JavaConversions._
    result.addAll(names)

    new SuggestedNameInfo(names.toArray) {}
  }

  protected def suggestedNames(element: PsiElement): Seq[String]
}