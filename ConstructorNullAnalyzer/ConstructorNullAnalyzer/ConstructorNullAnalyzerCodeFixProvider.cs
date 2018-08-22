using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace ConstructorNullAnalyzer
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(ConstructorNullAnalyzerCodeFixProvider)), Shared]
    public class ConstructorNullAnalyzerCodeFixProvider : CodeFixProvider
    {
        private const string simpleIfTitle = "Add null reference check";
        private const string IfWithBracesTitle = "Add null reference check (with braces)";

        public sealed override ImmutableArray<string> FixableDiagnosticIds => ImmutableArray.Create(ConstructorNullAnalyzer.DiagnosticId);

        public sealed override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            // Find the type declaration identified by the diagnostic.
            var constructorToken = root.FindToken(diagnosticSpan.Start).Parent as ConstructorDeclarationSyntax;
            var paramNames = diagnostic.AdditionalLocations.Select(x => GetParamName(constructorToken, x));

            // Register a code action that will invoke the fix.
            context.RegisterCodeFix(
                CodeAction.Create(
                    title: simpleIfTitle,
                    createChangedSolution: c => AddNullCheck(context.Document, constructorToken, paramNames, FixType.SimpleIf, c), 
                    equivalenceKey: simpleIfTitle),
                diagnostic);

            context.RegisterCodeFix(
                CodeAction.Create(
                    title: IfWithBracesTitle,
                    createChangedSolution: c => AddNullCheck(context.Document, constructorToken, paramNames, FixType.IfWithBlock, c),
                    equivalenceKey: IfWithBracesTitle),
                diagnostic);
        }

        private string GetParamName(SyntaxNode root, Location location)
        {
            var paramToken = root.FindToken(location.SourceSpan.Start);
            return paramToken.ValueText;
        }

        private async Task<Solution> AddNullCheck(Document document, ConstructorDeclarationSyntax constructor, IEnumerable<string> paramNames,  FixType fixType, CancellationToken cancellationToken)
        {
            var ifStatements = paramNames.Select(x => CreateIfStatement(x, fixType));
            var newBodyStatements = constructor.Body.Statements.InsertRange(0, ifStatements);
            var newBody = constructor.Body.WithStatements(newBodyStatements);

            var documentEditor = await DocumentEditor.CreateAsync(document, cancellationToken);
            documentEditor.ReplaceNode(constructor.Body, newBody);
            var newDocument = documentEditor.GetChangedDocument();

            if (documentEditor.OriginalRoot is CompilationUnitSyntax compilationUnitSyntax 
                && compilationUnitSyntax.Usings.All(x => x.Name.ToString() != "System"))
            {
                // Have to create new editor, since it overwrites/can't find node if do both changes in one editor
                var usingsDocumentEditor = await DocumentEditor.CreateAsync(newDocument, cancellationToken);
                var newCompilationUnitSyntax = usingsDocumentEditor.OriginalRoot as CompilationUnitSyntax;
                var newUsings = newCompilationUnitSyntax.Usings.Add(UsingDirective(IdentifierName("System")))
                    .OrderBy(x => x.Name.ToString());
                var newRoot = newCompilationUnitSyntax.WithUsings(new SyntaxList<UsingDirectiveSyntax>(newUsings));
                usingsDocumentEditor.ReplaceNode(newCompilationUnitSyntax, newRoot);

                return usingsDocumentEditor.GetChangedDocument().Project.Solution;
            }

            return newDocument.Project.Solution;
        }

        private static IfStatementSyntax CreateIfStatement(string paramName, FixType fixType)
        {
            var identifier = IdentifierName(paramName);
            var nullSyntax = LiteralExpression(SyntaxKind.NullLiteralExpression);
            var condition = BinaryExpression(SyntaxKind.EqualsExpression, identifier, nullSyntax);

            var exceptionTypeSyntax = ParseTypeName("ArgumentNullException");

            var nameOfIdentifier = Identifier(TriviaList(), SyntaxKind.NameOfKeyword, "nameof", "nameof", TriviaList());
            var argumentSyntax = Argument(InvocationExpression(IdentifierName(nameOfIdentifier))
                .WithArgumentList(
                    ArgumentList(
                        SingletonSeparatedList(
                            Argument(identifier)))));

            var newStatement = ObjectCreationExpression(exceptionTypeSyntax)
                .WithArgumentList(ArgumentList(SingletonSeparatedList(argumentSyntax)));

            var throwStatement = ThrowStatement().WithExpression(newStatement);

            switch (fixType)
            {
                case FixType.SimpleIf:
                {
                    var ifStatement = IfStatement(condition, throwStatement);
                    return ifStatement;
                }
                case FixType.IfWithBlock:
                {
                    var blockSyntax = Block(throwStatement);
                    var ifStatement = IfStatement(condition, blockSyntax);
                    return ifStatement;
                }
                default: throw new NotImplementedException($"Unknown fix type {fixType}");
            }
        }
    }
}
