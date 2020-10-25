package io.getquill.norm

import io.getquill.ast._
import io.getquill.quat.Quat
import io.getquill.util.Interpolator
import io.getquill.util.Messages.{ TraceType, title }

/**
 * Rename properties now relies on the Quats themselves to propagate field renames. The previous
 * itreations of this phase relied on schema propagation via stateful transforms holding
 * field-renames which were then compared to Property AST elements. This was a painstakingly complex and
 * highly error-prone especially when embedded objects were used requiring computation of sub-schemas
 * in a process called 'schema protraction'.
 * The new variation of this phase relies on the Quats directly since the Quats of every Identity, Lift, etc...
 * now know what the field-names contained therein as well as the sub-Quats of any embedded property.
 * This is fairly simple process:
 *
 * <ul>
 * <li> Learning what Quats have which renames is simple since this can be propagated from the Quats of the Entity objects,
 * to the rest of the AST.
 * <li> This has the simple requirement that renames must be propagated fully before they are actually committed
 * so that the knowledge of what needs to be renamed into what can be distributed easily throughout the AST.
 * <li> Once these future-renames are staged to Quats throught the AST, a simple stateless reduction will then apply
 * the renames to the Property AST elements around the Ident's (and potentially Lifts etc...) with the renamed Quats.
 * </ul>
 *
 * The entire process above can be done with a series of stateless transformations with straighforward operations
 * since the majority of the logic actually lives within the Quats themselves.
 */
object RenameProperties {
  private def demarcate(heading: String) =
    ((ast: Ast) => title(heading)(ast))

  def apply(ast: Ast) = {
    (identity[Ast] _)
      .andThen(SeedRenames.apply(_: Ast)) // Stage field renames into the Quats of entities
      .andThen(demarcate("SeedRenames"))
      .andThen(RepropagateQuats.apply(_: Ast))
      .andThen(demarcate("RepropagateQuats")) // Propagate the renames from Entity-Quats to the rest of the Quats in the AST
      .andThen(ApplyRenamesToProps.apply(_: Ast))
      .andThen(demarcate("ApplyRenamesToProps")) // Go through the Quats and 'commit' the renames
      .andThen(CompleteRenames.apply(_: Ast))
      .andThen(demarcate("CompleteRenames"))(ast) // Quats can be invalid in between this phase and the previous one
  }
}

object CompleteRenames extends StatelessTransformer {
  // NOTE Leaving renames on Entities so knowledges of what renames have been done remains in the AST. May want to change this in the future.
  override def applyIdent(e: Ident): Ident =
    e.copy(quat = e.quat.applyRenames)

  override def apply(e: Query): Query =
    e match {
      case ent: Entity => ent.copy(quat = ent.quat.applyRenames)
      case _           => super.apply(e)
    }

  override def apply(e: Ast): Ast = e match {
    case e: Ident =>
      e.copy(quat = e.quat.applyRenames)

    case other =>
      super.apply(other)
  }
}

/** Take renames propogated to the quats and apply them to properties */
object ApplyRenamesToProps extends StatelessTransformer {

  val interp = new Interpolator(TraceType.RenameProperties, 1)
  import interp._

  override def apply(p: Property): Property =
    applyProperty(p)

  def applyProperty(p: Property) =
    p match {
      case p @ Property.Opinionated(ast, name, renameable, visibility) =>
        trace"Checking Property: ${p} for possible rename".andLog()
        val newAst = apply(ast)
        // Check the quat if it is renaming this property if so rename it. Otherwise property is the same
        newAst.quat.renames.get(name) match {
          case Some(newName) =>
            trace"Applying Rename on Property:" andReturn
              Property.Opinionated(newAst, newName, Renameable.Fixed, visibility)
          case None => p
        }
    }

  override def apply(e: Ast): Ast = e match {
    case p: Property => applyProperty(p)
    case other       => super.apply(other)
  }
}

object SeedRenames extends StatelessTransformer {
  /**
   * In the case that there are entities inside of an infix and the infix is cast to the same entity, propagate the
   * renames from the entity inside of the infix to the external AST. For example say we have something like this:
   * {{{
   * val q = quote {
   *  infix"$${querySchema[A]("C", _.v -> "m")} LIMIT 10".as[Query[A]].filter(x => x.v == 1)
   * }
   * run(q)
   * }}}
   * We want to propagate the rename v -> "m" into the outside filter etc...
   */
  override def apply(e: Ast): Ast =
    e match {
      case Infix(a, b, pure, qu) =>
        // There could be an entity further in the AST of the elements, find them.
        val br = b.map(apply)
        br match {
          case elem :: Nil =>
            val renamedQuat =
              qu match {
                // Use the Quat from the Infix to do the renames
                case p: Quat.Product =>
                  p.withRenamesFrom(elem.quat)
                // If the infix is marked as generic or unknown, use the Infix from the single-element to do the renames
                // This typically happens with situations where the generic type of an infix needs to be cast
                // into is not known by the macros yet. See allowFiltering for the cassandra quill-context or
                // liftQuery(ds: Dataset) in the quill-spark context.
                case Quat.Placeholder(_) =>
                  elem.quat
                case _ =>
                  qu
              }
            Infix(a, List(elem), pure, renamedQuat)

          case _ =>
            // Check if there are any entities that have defined renames and warn them that renames cannot be applied
            // if there is more then one entity in the infix block
            if (br.length > 1 && br.find { case e: Entity => e.properties.length > 0; case _ => false }.isDefined)
              println(s"Cannot propagate renames from the entity ${e} into Query since there are other AST elements in the infix: ${br}")

            Infix(a, br, pure, qu)
        }
      case _ =>
        super.apply(e)
    }

  override def apply(e: Query): Query =
    e match {
      case e: Entity => e.syncToQuat
      case _         => super.apply(e)
    }
}

// Represents a nested property path to an identity i.e. Property(Property(... Ident(), ...))
object PropertyMatroshka {

  def traverse(initial: Property): Option[(Ast, List[String])] =
    initial match {
      // If it's a nested-property walk inside and append the name to the result (if something is returned)
      case Property(inner: Property, name) =>
        traverse(inner).map { case (id, list) => (id, list :+ name) }
      // If it's a property with ident in the core, return that
      case Property(inner, name) if !inner.isInstanceOf[Property] =>
        Some((inner, List(name)))
      // Otherwise an ident property is not inside so don't return anything
      case _ =>
        None
    }

  def unapply(ast: Property): Option[(Ast, List[String])] =
    ast match {
      case p: Property => traverse(p)
      case _           => None
    }

}
