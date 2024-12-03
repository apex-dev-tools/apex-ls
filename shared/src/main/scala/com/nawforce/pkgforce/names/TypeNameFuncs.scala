/*
 Copyright (c) 2022 Kevin Jones, All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 */
package com.nawforce.pkgforce.names

/** Utility functions that extend basic TypeName handling */
object TypeNameFuncs {
  implicit class TypeNameFuncs(typeName: TypeName) {

    def outerName: Name =
      typeName.outer.map(_.outerName).getOrElse(typeName.name)

    /** Replace the tail section of a TypeName with something else.
      * @return an updated TypeName or this if not matched.
      */
    def replaceTail(tail: TypeName, replacement: Option[TypeName]): TypeName = {
      if (typeName.outer.contains(tail))
        return TypeName(typeName.name, typeName.params, replacement)

      typeName.outer
        .map(outer => {
          val dropped = outer.replaceTail(tail, replacement)
          if (dropped ne outer) {
            TypeName(typeName.name, typeName.params, Some(dropped))
          } else {
            typeName
          }
        })
        .getOrElse(typeName)
    }
  }
}
