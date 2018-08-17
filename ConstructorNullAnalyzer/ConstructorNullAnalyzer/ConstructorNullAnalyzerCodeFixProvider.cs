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
        private const string title = "Add null reference check";

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
                    title: title,
                    createChangedSolution: c => AddNullCheck(context.Document, constructorToken, paramNames, c), 
                    equivalenceKey: title),
                diagnostic);
        }

        private string GetParamName(SyntaxNode root, Location location)
        {
            var paramToken = root.FindToken(location.SourceSpan.Start);
            return paramToken.ValueText;
        }

        private async Task<Solution> AddNullCheck(Document document, ConstructorDeclarationSyntax constructor, IEnumerable<string> paramNames, CancellationToken cancellationToken)
        {
            var ifStatements = paramNames.Select(CreateIfStatement);
            var newBodyStatements = constructor.Body.Statements.InsertRange(0, ifStatements);
            var newBody = constructor.Body.WithStatements(newBodyStatements);

            var documentEditor = await DocumentEditor.CreateAsync(document, cancellationToken);
            documentEditor.ReplaceNode(constructor.Body, newBody);

            var newDocument = documentEditor.GetChangedDocument();

            return newDocument.Project.Solution;
        }

        private static IfStatementSyntax CreateIfStatement(string paramName)
        {
            var identifier = IdentifierName(paramName);
            var nullSyntax = LiteralExpression(SyntaxKind.NullLiteralExpression);
            var condition = BinaryExpression(SyntaxKind.EqualsExpression, identifier, nullSyntax);

            var exceptionTypeSyntax = ParseTypeName("ArgumentNullException");
            var argumentSyntax = Argument(LiteralExpression(SyntaxKind.StringLiteralExpression, Literal(paramName)));
            var newStatement = ObjectCreationExpression(exceptionTypeSyntax)
                .WithArgumentList(ArgumentList(SingletonSeparatedList(argumentSyntax)));

            var throwStatement = ThrowStatement().WithExpression(newStatement);

            var ifStatement = IfStatement(condition, throwStatement);
            return ifStatement;
        }
    }
}
