package org.jetbrains.plugins.scala.macroAnnotations

import org.jetbrains.plugins.scala.macroAnnotations.ModCount.ModCount

import scala.annotation.{StaticAnnotation, tailrec}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
  * If you annotate a function with @Cached annotation, the compiler will generate code to cache it.
  *
  * If an annotated function has parameters, one field will be generated (a HashMap).
  * If an annotated function has no parameters, two fields will be generated: result and modCount
  *
  * NOTE !IMPORTANT!: function annotated with @Cached must be on top-most level because generated code generates fields
  * right outside the cached function and if this function is inner it won't work.
  *
  * Author: Svyatoslav Ilinskiy
  * Date: 9/18/15.
  */
class Cached(synchronized: Boolean, modificationCount: ModCount, psiElement: Any) extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro Cached.cachedImpl
}

object Cached {
  def cachedImpl(c: whitebox.Context)(annottees: c.Tree*): c.Expr[Any] = {
    import CachedMacroUtil._
    import c.universe._
    implicit val x: c.type = c

    def abort(message: String) = c.abort(c.enclosingPosition, message)

    def parameters: (Boolean, ModCount.Value, Tree) = {
      @tailrec
      def modCountParam(modCount: c.universe.Tree): ModCount.Value = modCount match {
        case q"modificationCount = $v" => modCountParam(v)
        case q"ModCount.$v" => ModCount.withName(v.toString)
        case q"$v" => ModCount.withName(v.toString)
      }

      c.prefix.tree match {
        case q"new Cached(..$params)" if params.length == 3 =>
          val synch: Boolean = params.head match {
            case q"synchronized = $v" => c.eval[Boolean](c.Expr(v))
            case q"$v" => c.eval[Boolean](c.Expr(v))
          }
          val modCount: ModCount.Value = modCountParam(params(1))
          val psiElement = params(2)
          (synch, modCount, psiElement)
        case _ => abort("Wrong parameters")
      }
    }

    //annotation parameters
    val (synchronized, modCount, psiElement) = parameters

    annottees.toList match {
      case DefDef(mods, name, tpParams, paramss, retTp, rhs) :: Nil =>
        if (retTp.isEmpty) {
          abort("You must specify return type")
        }
        //generated names
        val cachedFunName = generateTermName("cachedFun")
        val cacheStatsName = generateTermName("cacheStats")
        val keyId = c.freshName(name.toString + "$cacheKey")
        val mapAndCounterName = generateTermName(name.toString + "$mapAndCounter")
        val timestampedData = generateTermName(name.toString + "$valueAndCounter")

        val analyzeCaches = analyzeCachesEnabled(c)
        val defdefFQN = thisFunctionFQN(name.toString)

        //DefDef parameters
        val flatParams = paramss.flatten
        val paramNames = flatParams.map(_.name)
        val hasParameters: Boolean = flatParams.nonEmpty

        val lockFieldName = generateTermName("lock")
        def lockField = if (synchronized) q"private val $lockFieldName = new _root_.java.lang.Object()" else EmptyTree

        val analyzeCachesField =
          if(analyzeCaches) q"private val $cacheStatsName = $cacheStatisticsFQN($keyId, $defdefFQN)"
          else EmptyTree

        val actualCalculation = transformRhsToAnalyzeCaches(c)(cacheStatsName, retTp, rhs)

        val currModCount = modCount match {
          case ModCount.getBlockModificationCount =>
            q"val currModCount = $cachesUtilFQN.enclosingModificationOwner($psiElement).getModificationCount"
          case ModCount.getOutOfCodeBlockModificationCount =>
            q"val currModCount = $scalaPsiManagerFQN.instance($psiElement.getProject).getModificationCount"
          case ModCount.anyScalaPsiModificationCount =>
            q"val currModCount = $scalaPsiManagerFQN.AnyScalaPsiModificationTracker.getModificationCount"
          case _ =>
            q"val currModCount = $psiElement.getManager.getModificationTracker.${TermName(modCount.toString)}"
        }

        val (fields, updatedRhs) = if (hasParameters) {
          //wrap type of value in Some to avoid unboxing in putIfAbsent for primitive types
          val mapType = tq"$concurrentMapTypeFqn[(..${flatParams.map(_.tpt)}), _root_.scala.Some[$retTp]]"

          def createNewMap = q"_root_.com.intellij.util.containers.ContainerUtil.newConcurrentMap()"

          val fields = q"""
              new _root_.scala.volatile()
              private var $mapAndCounterName: $timestampedTypeFQN[$mapType] = $timestampedFQN(null, -1L)
              ..$lockField

              ..$analyzeCachesField
           """

          val getOrUpdateMapDef = {
            if (synchronized) q"""
              def getOrUpdateMap() = {
                if ($mapAndCounterName.modCount < currModCount) {
                  $lockFieldName.synchronized {
                    if ($mapAndCounterName.modCount < currModCount) { //double checked locking
                      $mapAndCounterName = $timestampedFQN($createNewMap, currModCount)
                    }
                  }
                }
                $mapAndCounterName.data
              }
            """
            else q"""
              def getOrUpdateMap() = {
                if ($mapAndCounterName.modCount < currModCount) {
                  $mapAndCounterName = $timestampedFQN($createNewMap, currModCount)
                }
                $mapAndCounterName.data
              }
            """
          }

          def updatedRhs = q"""
             def $cachedFunName(): $retTp = {
               $actualCalculation
             }

             ..$currModCount

             $getOrUpdateMapDef

             val map = getOrUpdateMap()
             val key = (..$paramNames)

             map.get(key) match {
               case Some(v) => v
               case null =>
                 //wrap type of value in Some to avoid unboxing in putIfAbsent for primitive types
                 val computed = _root_.scala.Some($cachedFunName())
                 val race = map.putIfAbsent(key, computed)
                 if (race != null) race.get
                 else computed.get
             }
          """
          (fields, updatedRhs)
        } else {
          val fields = q"""
              new _root_.scala.volatile()
              private var $timestampedData: $timestampedTypeFQN[$retTp] = $timestampedFQN(${defaultValue(c)(retTp)}, -1L)
              ..$lockField

            ..$analyzeCachesField
          """

          val getOrUpdateValue =
            q"""
               val timestamped = $timestampedData
               if (timestamped.modCount == currModCount) timestamped.data
               else {
                 val computed = $cachedFunName()
                 $timestampedData = $timestampedFQN(computed, currModCount)
                 computed
               }
             """

          val getOrSyncUpdate = if (synchronized)
            q"""
                val timestamped = $timestampedData
                if (timestamped.modCount == currModCount) timestamped.data
                else {
                  $lockFieldName.synchronized {
                    $getOrUpdateValue
                  }
                }
              """
          else getOrUpdateValue

          val updatedRhs =
            q"""
               def $cachedFunName(): $retTp = {
                 $actualCalculation
               }

               ..$currModCount

               $getOrSyncUpdate
             """
          (fields, updatedRhs)
        }

        val updatedDef = DefDef(mods, name, tpParams, paramss, retTp, updatedRhs)
        val res = q"""
          ..$fields
          $updatedDef
          """
        CachedMacroUtil.println(res)
        c.Expr(res)
      case _ => abort("You can only annotate one function!")
    }
  }
}
