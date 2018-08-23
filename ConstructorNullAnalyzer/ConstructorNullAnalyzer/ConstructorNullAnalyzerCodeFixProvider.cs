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
        private const string SimpleIfTitle = "Add null reference check";
        private const string IfWithBracesTitle = "Add null reference check (with braces)";
        private const string CoalesceTitle = "Add coalesce checks for existing assignments";

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
            var paramNames = diagnostic.AdditionalLocations.Select(x => GetParamName(constructorToken, x)).ToList();

            // Register a code action that will invoke the fix.
            context.RegisterCodeFix(
                CodeAction.Create(
                    title: SimpleIfTitle,
                    createChangedSolution: c => AddNullCheck(context.Document, constructorToken, paramNames, FixType.SimpleIf, c), 
                    equivalenceKey: SimpleIfTitle),
                diagnostic);

            context.RegisterCodeFix(
                CodeAction.Create(
                    title: IfWithBracesTitle,
                    createChangedSolution: c => AddNullCheck(context.Document, constructorToken, paramNames, FixType.IfWithBlock, c),
                    equivalenceKey: IfWithBracesTitle),
                diagnostic);

            context.RegisterCodeFix(
                CodeAction.Create(
                    title: CoalesceTitle,
                    createChangedSolution: c => AddNullCheck(context.Document, constructorToken, paramNames, FixType.CoalesceOperator, c),
                    equivalenceKey: CoalesceTitle),
                diagnostic);
        }

        private string GetParamName(SyntaxNode root, Location location)
        {
            var paramToken = root.FindToken(location.SourceSpan.Start);
            return paramToken.ValueText;
        }

        private async Task<Solution> AddNullCheck(Document document, ConstructorDeclarationSyntax constructor, IList<string> paramNames,  FixType fixType, CancellationToken cancellationToken)
        {
            var newBodyStatements = GetNewBodyStatements(constructor, paramNames, fixType);

            if (!newBodyStatements.Any())
            {
                return document.Project.Solution;
            }

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

        private static SyntaxList<StatementSyntax> GetNewBodyStatements(
            ConstructorDeclarationSyntax constructor,
            IList<string> paramNames,
            FixType fixType)
        {
            switch (fixType)
            {
                case FixType.CoalesceOperator:
                {
                    var assignmentStatements = paramNames.Select(x => CreateFixerForAssignment(constructor, x))
                        .Where(x => x.newStatement != null);
                    var declarationFixStatements = paramNames.Select(x => CreateFixerForDeclaration(constructor, x)).ToList();

                    var paramFixStatements = declarationFixStatements.Union(assignmentStatements);
                    var updatedStatements = constructor.Body.Statements;
                    foreach (var (oldStatement, newStatement) in paramFixStatements.Where(x => x.oldStatement != null))
                    {
                        var oldStatementIndex = constructor.Body.Statements.IndexOf(oldStatement);
                        updatedStatements = updatedStatements.RemoveAt(oldStatementIndex);
                        updatedStatements = updatedStatements.Insert(oldStatementIndex, newStatement);
                    }

                    return updatedStatements;
                }
                case FixType.SimpleIf:
                case FixType.IfWithBlock:
                {

                    var paramFixStatements = paramNames.Select(x => CreateIfStatement(x, fixType))
                        .Where(x => x != null)
                        .ToList();
                    return constructor.Body.Statements.InsertRange(0, paramFixStatements);
                }
                default: throw new NotImplementedException($"Unknown fix type {fixType}");
            }
        }

        private static (StatementSyntax oldStatement, StatementSyntax newStatement) CreateFixerForDeclaration(
            ConstructorDeclarationSyntax constructor,
            string paramName)
        {
            var declarationStatements = constructor.Body.Statements.OfType<LocalDeclarationStatementSyntax>();
            var declarationStatement = declarationStatements
                .FirstOrDefault(x => IsDeclarationStatementInvolvesParam(x.Declaration, paramName));

            if (declarationStatement == null)
            {
                return (null, null);
            }

            var identifier = GetIdentifiersFromDeclaration(declarationStatement.Declaration)
                .FirstOrDefault(x => x.Identifier.ValueText == paramName);
            var coalesceExpression = BuildCoalesceExpression(identifier);

            var declarationToChange =
                declarationStatement.Declaration.Variables.FirstOrDefault(x => x.Initializer != null);
            if (declarationToChange == null)
            {
                return (null, null);
            }

            var newInitializer = declarationToChange.Initializer.WithValue(coalesceExpression);
            var newDeclaration = declarationToChange.WithInitializer(newInitializer);
            var newVariablesDeclaration = declarationStatement.Declaration.Variables.Replace(declarationToChange, newDeclaration);
            var newDeclarationStatement =
                LocalDeclarationStatement(
                    VariableDeclaration(declarationStatement.Declaration.Type, newVariablesDeclaration));
            return (oldStatement: declarationStatement, newStatement: newDeclarationStatement);
        }

        private static bool IsDeclarationStatementInvolvesParam(VariableDeclarationSyntax argDeclaration, string paramName)
        {
            return GetIdentifiersFromDeclaration(argDeclaration)
                .Any(x => x?.Identifier.ValueText == paramName);
        }

        private static IEnumerable<IdentifierNameSyntax> GetIdentifiersFromDeclaration(
            VariableDeclarationSyntax declaration)
        {
            return declaration.Variables
                .Where(x => x.Initializer != null)
                .Select(x => x.Initializer.Value)
                .OfType<IdentifierNameSyntax>();
        }

        private static (StatementSyntax oldStatement, StatementSyntax newStatement) CreateFixerForAssignment(
            ConstructorDeclarationSyntax constructor, string paramName)
        {
            var expressionStatements = constructor.Body.Statements
                .OfType<ExpressionStatementSyntax>()
                .ToList();
            var assignementExpressions = expressionStatements
                .Select(x => x.Expression)
                .OfType<AssignmentExpressionSyntax>();
            var assignmentExpression = assignementExpressions
                .FirstOrDefault(x => IsAssignmentStatementInvolvesParam(x, paramName));
            if (assignmentExpression == null)
            {
                return (null, null);
            }

            var identifier = assignmentExpression.Right as IdentifierNameSyntax;
            var coalesceExpression = BuildCoalesceExpression(identifier);

            var oldStatement = expressionStatements.First(x => x.Expression == assignmentExpression);
            var newAssignement = AssignmentExpression(
                SyntaxKind.SimpleAssignmentExpression,
                (oldStatement.Expression as AssignmentExpressionSyntax).Left,
                coalesceExpression);
            return (oldStatement: oldStatement, newStatement: ExpressionStatement(newAssignement));
        }

        private static ExpressionSyntax BuildCoalesceExpression(IdentifierNameSyntax identifier)
        {
            var throwExpression = BuildExceptionExpression(identifier);
            var coalesce = BinaryExpression(SyntaxKind.CoalesceExpression, identifier, ThrowExpression(throwExpression));
            return coalesce;
        }

        private static bool IsAssignmentStatementInvolvesParam(AssignmentExpressionSyntax assignment, string paramName)
        {
            var rightPart = assignment.Right;
            if (rightPart is IdentifierNameSyntax identifier)
            {
                return identifier.Identifier.Text == paramName;
            }

            return false;
        }

        private static IfStatementSyntax CreateIfStatement(string paramName, FixType fixType)
        {
            var identifier = IdentifierName(paramName);

            var nullSyntax = LiteralExpression(SyntaxKind.NullLiteralExpression);
            var condition = BinaryExpression(SyntaxKind.EqualsExpression, identifier, nullSyntax);
            var throwStatement = ThrowStatement(BuildExceptionExpression(identifier));

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

        private static ExpressionSyntax BuildExceptionExpression(IdentifierNameSyntax identifier)
        {
            var exceptionTypeSyntax = ParseTypeName("ArgumentNullException");

            var nameOfIdentifier = Identifier(TriviaList(), SyntaxKind.NameOfKeyword, "nameof", "nameof", TriviaList());
            var argumentSyntax = Argument(InvocationExpression(IdentifierName(nameOfIdentifier))
                .WithArgumentList(
                    ArgumentList(
                        SingletonSeparatedList(
                            Argument(identifier)))));

            var throwExpression = ObjectCreationExpression(exceptionTypeSyntax)
                .WithArgumentList(ArgumentList(SingletonSeparatedList(argumentSyntax)));
            return throwExpression;
        }
    }
}
