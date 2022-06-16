/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser.testutil

import apex.jorje.lsp.ApexLanguageServerLauncher
import apex.jorje.lsp.api.codeActions.{CodeActionsProvider, QuickFixProvider}
import apex.jorje.lsp.api.codeLenses.CodeLensProvider
import apex.jorje.lsp.api.completions.CompletionStrategy
import apex.jorje.lsp.api.connection.ConnectionStreamProvider
import apex.jorje.lsp.api.debug.DebuggerService
import apex.jorje.lsp.api.definition.DefinitionStrategy
import apex.jorje.lsp.api.document.DocumentLifecycleHandler
import apex.jorje.lsp.api.error.ErrorHandler
import apex.jorje.lsp.api.hover.HoverProvider
import apex.jorje.lsp.api.index.converter.ConverterFactory
import apex.jorje.lsp.api.references.{ReferenceLocationProvider, ReferenceStrategy}
import apex.jorje.lsp.api.rename.RenameProvider
import apex.jorje.lsp.api.search.MemberDefinitionLocator
import apex.jorje.lsp.api.services.{ApexCompilerService, DocumentationService}
import apex.jorje.lsp.api.symbols.CachingSymbolProvider
import apex.jorje.lsp.api.telemetry.TelemetryHandler
import apex.jorje.lsp.api.tests.TestService
import apex.jorje.lsp.api.utils.SymbolPrinter
import apex.jorje.lsp.api.visitors.VisitorFactory
import apex.jorje.lsp.api.workspace.{
  ApexDocumentService,
  DependentCompilationTracker,
  WorkspaceChangeListener
}
import apex.jorje.lsp.impl.codeActions.quickFix.DeclareMissingMethodProvider
import apex.jorje.lsp.impl.codeActions.{ApexCodeActionsProvider, CodeActionsProviderAggregator}
import apex.jorje.lsp.impl.codeLens.{
  AnonymousApexCodeLensesProvider,
  ApexTestRunCodeLensesProvider,
  CodeLensesProviderAggregator
}
import apex.jorje.lsp.impl.completions._
import apex.jorje.lsp.impl.completions.soql.EmbeddedSoqlCompletionStrategy
import apex.jorje.lsp.impl.completions.triggers.{
  TriggerContextVariablesCompletionStrategy,
  TriggerKeywordCompletionStrategy
}
import apex.jorje.lsp.impl.concurrency.ExecutorServiceProvider
import apex.jorje.lsp.impl.connection.StandardIoProvider
import apex.jorje.lsp.impl.debug.ApexDebuggerService
import apex.jorje.lsp.impl.definition.{
  ApexDefinitionStrategyAggregator,
  StandardSymbolsDefinitionStrategy
}
import apex.jorje.lsp.impl.diagnostics.DiagnosticsReporter
import apex.jorje.lsp.impl.document.DocumentLifecycleDispatcher
import apex.jorje.lsp.impl.error.StandardErrorHandler
import apex.jorje.lsp.impl.hover.StandardHoverProvider
import apex.jorje.lsp.impl.index.converter.TypeInfoConverter
import apex.jorje.lsp.impl.index.node._
import apex.jorje.lsp.impl.index.{ApexIndex, ApexIndexer}
import apex.jorje.lsp.impl.injection.ApexLanguageServerModule
import apex.jorje.lsp.impl.references.{
  ApexReferenceStrategyAggregator,
  DBBackedReferenceProvider,
  TrackedUsageReferenceStrategy
}
import apex.jorje.lsp.impl.rename.StandardRenameProvider
import apex.jorje.lsp.impl.search.StandardMemberDefinitionLocator
import apex.jorje.lsp.impl.services._
import apex.jorje.lsp.impl.symbols.ApexSymbolProvider
import apex.jorje.lsp.impl.telemetry.StandardTelemetryHandler
import apex.jorje.lsp.impl.tests.StandardTestService
import apex.jorje.lsp.impl.typings.TypeDefinitionLifecycleHandler
import apex.jorje.lsp.impl.utils.{SfdxProjects, StandardSymbolPrinter, Symbols}
import apex.jorje.lsp.impl.visitors.StandardDocumentSymbolVisitor
import apex.jorje.lsp.impl.workspace.{
  ServerSetup,
  StandardApexDocumentService,
  StandardDependentCompilationTracker,
  WorkspaceChangeListenerDispatcher
}
import apex.jorje.semantic.ast.visitor.{AstVisitor, Scope}
import com.google.common.util.concurrent.ServiceManager
import com.google.inject.assistedinject.FactoryModuleBuilder
import com.google.inject.multibindings.Multibinder
import com.google.inject.{Guice, Injector, Provider, Singleton}
import com.financialforce.oparser.testutil.SymbolProvider.SFModuleWithDb
import org.eclipse.jdt.internal.core.nd.db.{ChunkCache, Database}
import org.eclipse.jdt.internal.core.nd.{Nd, NdNode, NdNodeTypeRegistry}
import org.eclipse.lsp4j.services.{
  LanguageClient,
  LanguageServer,
  TextDocumentService,
  WorkspaceService
}

import java.nio.file.Path
import java.util.concurrent.ExecutorService

private class SymbolProvider(dbPath: Path) {
  val injector: Injector = Guice.createInjector(new SFModuleWithDb(dbPath))
  val compilerService: StandardCompilerService =
    injector.getInstance(classOf[StandardCompilerService])
  val apexSymbolProvider =
    new ApexSymbolProvider(injector.getProvider(classOf[ApexIndex]), compilerService)
}

object SymbolProvider {

  def apply(dbPath: Path): ApexSymbolProvider = {
    new SymbolProvider(dbPath).apexSymbolProvider
  }

  private class IndexProvider(dbPath: Path) extends Provider[ApexIndex] {
    private val ND_VERSION = Nd.version(42, 8)

    override def get(): ApexIndex = {
      def createTypeRegistry: NdNodeTypeRegistry[NdNode] = {
        val registry = new NdNodeTypeRegistry[NdNode]
        registry.register(1, ApexField.`type`.getFactory)
        registry.register(2, ApexMethod.`type`.getFactory)
        registry.register(3, ApexMethodParameter.`type`.getFactory)
        registry.register(4, ApexResourceFile.`type`.getFactory)
        registry.register(5, ApexTreeNode.`type`.getFactory)
        registry.register(6, ApexType.`type`.getFactory)
        registry.register(7, ApexTypeId.`type`.getFactory)
        registry.register(8, SObjectField.`type`.getFactory)
        registry.register(9, ApexReference.`type`.getFactory)
        registry.register(16, ApexClassInterfaceJunction.`type`.getFactory)
        registry
      }

      val nd = new Nd(
        dbPath.toFile,
        ChunkCache.getSharedInstance,
        createTypeRegistry,
        ND_VERSION,
        ND_VERSION,
        ND_VERSION
      )
      new ApexIndex(nd, Database.DATA_AREA_OFFSET)
    }
  }

  private class SFModuleWithDb(dbPath: Path) extends ApexLanguageServerModule {

    /**
      * This is exact copy of ApexLanguageServerModule
      * where the only difference is we provide our own ApexIndexProvider
      * so that we can make use of the DB that gets passed in.
      */
    override def configure(): Unit = {

      this.install((new FactoryModuleBuilder).build(classOf[CompletionActivationFactory]))
      this.bind(classOf[CompletionStrategy]).to(classOf[ApexCompletionStrategyAggregator])
      val completionStrategies = Multibinder.newSetBinder(this.binder, classOf[CompletionStrategy])
      completionStrategies.addBinding.to(classOf[LocalVariableNamesCompletionStrategy])
      completionStrategies.addBinding.to(classOf[FieldNamesCompletionStrategy])
      completionStrategies.addBinding.to(classOf[MethodNamesCompletionStrategy])
      completionStrategies.addBinding.to(classOf[MembersCompletionStrategy])
      completionStrategies.addBinding.to(classOf[TypesCompletionStrategy])
      completionStrategies.addBinding.to(classOf[SystemNamespaceCompletionStrategy])
      completionStrategies.addBinding.to(classOf[TriggerKeywordCompletionStrategy])
      completionStrategies.addBinding.to(classOf[TriggerContextVariablesCompletionStrategy])
      completionStrategies.addBinding.to(classOf[EmbeddedSoqlCompletionStrategy])
      completionStrategies.addBinding.to(classOf[SObjectFieldNamesCompletionStrategy])
      this.bind(classOf[DefinitionStrategy]).to(classOf[ApexDefinitionStrategyAggregator])
      this.bind(classOf[MemberDefinitionLocator]).to(classOf[StandardMemberDefinitionLocator])
      val definitionStrategies = Multibinder.newSetBinder(this.binder, classOf[DefinitionStrategy])
      definitionStrategies.addBinding.to(classOf[StandardSymbolsDefinitionStrategy])
      this.bind(classOf[DocumentationService]).to(classOf[StandardDocumentationService])
      this.bind(classOf[ReferenceStrategy]).to(classOf[ApexReferenceStrategyAggregator])
      val referenceStrategies = Multibinder.newSetBinder(this.binder, classOf[ReferenceStrategy])
      referenceStrategies.addBinding.to(classOf[TrackedUsageReferenceStrategy])
      this.bind(classOf[ReferenceLocationProvider]).to(classOf[DBBackedReferenceProvider])
      this.bind(classOf[RenameProvider]).to(classOf[StandardRenameProvider])
      this.bind(classOf[HoverProvider]).to(classOf[StandardHoverProvider])
      this
        .bind(classOf[ExecutorService])
        .toProvider(classOf[ExecutorServiceProvider])
        .in(classOf[Singleton])
      this.bind(classOf[ConnectionStreamProvider]).to(classOf[StandardIoProvider])
      this.bind(classOf[DocumentLifecycleHandler]).to(classOf[DocumentLifecycleDispatcher])
      val lifecycleHandlers =
        Multibinder.newSetBinder(this.binder, classOf[DocumentLifecycleHandler])
      lifecycleHandlers.addBinding.to(classOf[ApexIndexer])
      lifecycleHandlers.addBinding.to(classOf[DiagnosticsReporter])
      lifecycleHandlers.addBinding.to(classOf[TypeDefinitionLifecycleHandler])
      this.install(
        (new FactoryModuleBuilder)
          .implement(classOf[TypeInfoConverter], classOf[TypeInfoConverter])
          .build(classOf[ConverterFactory])
      )
      //Inject our own provider
      this.bind(classOf[ApexIndex]).toProvider(new IndexProvider(dbPath)).in(classOf[Singleton])

      this
        .bind(classOf[ApexCompilerService])
        .to(classOf[StandardCompilerService])
        .in(classOf[Singleton])
      this.bind(classOf[LanguageServer]).to(classOf[ApexLanguageServer])
      this.bind(classOf[ApexPreludeManagedService])
      this.bind(classOf[ServerSetup]).toProvider(classOf[ApexLanguageServer]).in(classOf[Singleton])
      this
        .bind(classOf[LanguageClient])
        .toProvider(classOf[ApexLanguageServerLauncher])
        .in(classOf[Singleton])
      this
        .bind(classOf[ServiceManager])
        .toProvider(classOf[ServiceManagerProvider])
        .in(classOf[Singleton])
      this
        .bind(classOf[CachingSymbolProvider])
        .to(classOf[ApexSymbolProvider])
        .in(classOf[Singleton])
      this.bind(classOf[DebuggerService]).to(classOf[ApexDebuggerService])
      this.bind(classOf[TestService]).to(classOf[StandardTestService])
      this.bind(classOf[SymbolPrinter]).to(classOf[StandardSymbolPrinter])
      this.bind(classOf[Symbols])
      this.bind(classOf[SfdxProjects])
      this.install(
        (new FactoryModuleBuilder)
          .implement(classOf[AstVisitor[_ <: Scope]], classOf[StandardDocumentSymbolVisitor])
          .build(classOf[VisitorFactory])
      )
      this.bind(classOf[ApexDocumentService]).to(classOf[StandardApexDocumentService])
      this.bind(classOf[TextDocumentService]).to(classOf[StandardTextDocumentService])
      this
        .bind(classOf[DependentCompilationTracker])
        .to(classOf[StandardDependentCompilationTracker])
        .in(classOf[Singleton])
      this.bind(classOf[WorkspaceService]).to(classOf[StandardWorkspaceService])
      this.bind(classOf[WorkspaceChangeListener]).to(classOf[WorkspaceChangeListenerDispatcher])
      val workspaceListeners =
        Multibinder.newSetBinder(this.binder, classOf[WorkspaceChangeListener])
      workspaceListeners.addBinding.to(classOf[ApexIndexer])
      workspaceListeners.addBinding.to(classOf[TypeDefinitionLifecycleHandler])
      this.bind(classOf[CodeActionsProvider]).to(classOf[CodeActionsProviderAggregator])
      val codeActionsProviders = Multibinder.newSetBinder(this.binder, classOf[CodeActionsProvider])
      codeActionsProviders.addBinding.to(classOf[DeclareMissingMethodProvider])
      codeActionsProviders.addBinding.to(classOf[ApexCodeActionsProvider])
      val quickFixProviders = Multibinder.newSetBinder(this.binder, classOf[QuickFixProvider])
      quickFixProviders.addBinding.to(classOf[DeclareMissingMethodProvider])
      this.bind(classOf[CodeLensProvider]).to(classOf[CodeLensesProviderAggregator])
      val codeLensProviders = Multibinder.newSetBinder(this.binder, classOf[CodeLensProvider])
      codeLensProviders.addBinding.to(classOf[ApexTestRunCodeLensesProvider])
      codeLensProviders.addBinding.to(classOf[AnonymousApexCodeLensesProvider])
      this.bind(classOf[ErrorHandler]).to(classOf[StandardErrorHandler])
      this.bind(classOf[TelemetryHandler]).to(classOf[StandardTelemetryHandler])

    }
  }

}
