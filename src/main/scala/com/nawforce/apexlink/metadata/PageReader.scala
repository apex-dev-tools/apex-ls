/*
 [The "BSD licence"]
 Copyright (c) 2017 Kevin Jones
 All rights reserved.

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

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package com.nawforce.apexlink.metadata

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}

import com.nawforce.apexlink.utils.{LinkerException, LinkerLog, TraversePath, XMLLineLoader}

class PageReader extends SymbolReader {

  override def loadSymbols(ctx: SymbolReaderContext): Unit = {
    try {
      val pagesDir = ctx.getBaseDir.resolve("pages")
      LinkerLog.ifNotLogAndThrow(Files.isDirectory(pagesDir), 0, "Pages directory is not present")

      val traverse = new TraversePath(pagesDir)
      traverse foreach {
        case (file: Path, attr: BasicFileAttributes) =>
          if (attr.isRegularFile && file.toString.endsWith(".page")) {
            loadPage(ctx, file.getFileName.toString.replaceFirst(".page$", ""), file)
          } else if (attr.isRegularFile && file.toString.endsWith(".page-meta.xml")) {
            // Ignore
          } else if (attr.isRegularFile) {
            if (!isIgnoreable(file))
              LinkerLog.logMessage(file.toString, 0, "Unexpected file in pages directory")
          } else {
            LinkerLog.logMessage(file.toString, 0, "Only expected to find files in pages directory")
          }
      }
    }
    catch {
      case _: LinkerException => () // Ignore, just used to abort processing
    }
  }

  private def loadPage(ctx: SymbolReaderContext, fullName: String, objectFile: Path): Unit = {
    LinkerLog.pushContext(objectFile.toString)
    try {
      val root = XMLLineLoader.loadFile(objectFile.toString)

      Page.create(fullName, root).foreach(o => ctx.addPage(o))
    } finally {
      LinkerLog.popContext()
    }
  }
}
