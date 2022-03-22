package com.nawforce.runtime.sfparser

import apex.jorje.lsp.api.client.ExtendedLanguageClient
import apex.jorje.lsp.impl.index.{ApexIndex, ApexIndexer}
import apex.jorje.lsp.impl.injection.ApexLanguageServerModule
import apex.jorje.lsp.impl.services.ApexLanguageServer
import apex.jorje.lsp.impl.symbols.ApexSymbolProvider
import apex.jorje.lsp.impl.workspace.ServerSetup
import com.google.inject.Guice
import org.eclipse.lsp4j.{
  InitializeParams,
  MessageActionItem,
  MessageParams,
  PublishDiagnosticsParams,
  ShowMessageRequestParams
}
import org.eclipse.lsp4j.services.LanguageServer

import java.nio.file.Path
import java.util.concurrent.CompletableFuture

object SFLanguageServer {

  private val injector = Guice.createInjector(new ApexLanguageServerModule())
  private val languageServer: LanguageServer = injector.getInstance(classOf[ApexLanguageServer])

  def runAndExit[T](workspace: Path)(op: ApexSymbolProvider => T): Unit = {
    //TODO: use apexDb for indexer instead of running it in workspace
    start(workspace)
    op(getSymbolProvider)
    exit()
  }

  def start(workspace: Path): Unit = {
    val onIndexComplete = setupOnDoneIndexing()
    val params = getInitParams(workspace)
    languageServer.initialize(params)
    onIndexComplete.get()
  }

  def exit(): Unit = {
    languageServer.exit()
  }

  private def getSymbolProvider: ApexSymbolProvider = {
    injector.getInstance(classOf[ApexSymbolProvider])
  }

  private def getInitParams(root: Path): InitializeParams = {
    val params = new InitializeParams()
    params.setRootPath(root.toString)
    params
  }

  private def setupOnDoneIndexing(): CompletableFuture[Unit] = {
    val future = new CompletableFuture[Unit]()
    val setup = injector.getInstance(classOf[ServerSetup])
    setup.setLanguageClient(new ExtendedLanguageClient() {
      override def doneIndexing(): Unit = {
        future.complete()
      }

      override def telemetryEvent(o: Any): Unit = None

      override def publishDiagnostics(publishDiagnosticsParams: PublishDiagnosticsParams): Unit =
        None

      override def showMessage(messageParams: MessageParams): Unit = None

      override def showMessageRequest(
                                       showMessageRequestParams: ShowMessageRequestParams
                                     ): CompletableFuture[MessageActionItem] = null

      override def logMessage(messageParams: MessageParams): Unit = None
    })
    future
  }
}
