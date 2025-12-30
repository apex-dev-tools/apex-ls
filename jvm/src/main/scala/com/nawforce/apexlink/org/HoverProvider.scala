/*
 Copyright (c) 2023 Certinia Inc, All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.cst.{Creator, NewExpression}
import com.nawforce.apexlink.rpc.HoverItem
import com.nawforce.apexlink.types.apex.{
  ApexClassDeclaration,
  ApexConstructorLike,
  ApexFullDeclaration,
  ApexMethodLike
}
import com.nawforce.apexlink.types.core.TypeId
import com.nawforce.pkgforce.path.{Locatable, Location, PathLike}

trait HoverProvider extends SourceOps {
  this: OPM.PackageImpl =>

  def getHover(path: PathLike, line: Int, offset: Int, content: Option[String]): HoverItem = {
    val sourceAndType = loadFullSourceAndType(path, content)
    if (sourceAndType.isEmpty)
      return HoverItem(None, None)

    toHoverItem(getFromValidationLocatable(sourceAndType.get._2, line, offset))
  }

  private def getFromValidationLocatable(
    td: ApexFullDeclaration,
    line: Int,
    offset: Int
  ): Option[(Locatable, Location)] = {
    val validation = locateFromValidation(td, line, offset)

    // Check all validation results to see if we have a class directly
    val allLocatables = validation._1
      .filter { case (loc, _) => loc.contains(line, offset) }
      .flatMap { case (loc, vr) =>
        vr.result.locatable.map((_, loc))
      }
      .toSeq

    val classLocatable = allLocatables.collectFirst { case (l: ApexClassDeclaration, loc) =>
      (l, loc)
    }
    val constructorLocatable = allLocatables.collectFirst { case (l: ApexConstructorLike, loc) =>
      (l, loc)
    }
    val methodLocatable = allLocatables.collectFirst { case (l: ApexMethodLike, loc) =>
      (l, loc)
    }

    // If we found a class directly, use it
    // Otherwise, if we found a constructor, check if it's parameterless
    // Parameterless constructors should show the class, constructors with parameters should show the constructor
    methodLocatable
      .orElse(classLocatable)
      .orElse(constructorLocatable.flatMap { case (ctor, loc) =>
        if (ctor.parameters.isEmpty) {
          // Parameterless constructor - show the class
          // For parameterless constructors, show the class declaration
          // The location should be where the class name appears in the source (from validation result)
          ctor.thisTypeId.toTypeDeclaration[ApexClassDeclaration] match {
            case Some(classDecl) =>
              // Use the validation result location start, but adjust end to match class name length
              // The location should span just the class name, not the entire "new Dummy()" expression
              val classNameLength = classDecl.name.value.length
              val adjustedLocation = Location(
                loc.startLine,
                loc.startPosition,
                loc.endLine,
                loc.startPosition + classNameLength
              )
              Some(classDecl, adjustedLocation)
            case None => Some(ctor, loc)
          }
        } else {
          // Constructor with parameters - show the constructor
          Some(ctor, loc)
        }
      })
  }

  private def toHoverItem(l: Option[(Locatable, Location)]): HoverItem = {
    l match {
      case Some((td, loc)) => HoverItem(Some(td.toString), Some(loc))
      case _               => HoverItem(None, None)
    }
  }
}
