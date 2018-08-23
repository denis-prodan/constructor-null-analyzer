using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace ConstructorNullAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class ConstructorNullAnalyzer : DiagnosticAnalyzer
    {
        private const string Category = "Correctness";
        private const string AnalyzerErrorDiagnosticId = "CA000";
        public const string DiagnosticId = "CA001";
        private static readonly LocalizableString Title = "Not checked reference parameter in constructor";
        private static readonly LocalizableString MessageFormat = "Constructor should check that parameter(s) {0} are not null";
        private static readonly LocalizableString Description = "All reference type parameters should be checked for not-null";

        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(
            id: DiagnosticId,
            title: Title,
            messageFormat: MessageFormat,
            category: Category,
            defaultSeverity: DiagnosticSeverity.Warning,
            isEnabledByDefault: true,
            description: Description);

        private static readonly DiagnosticDescriptor AnalyzerErrorDescriptor = new DiagnosticDescriptor(
            id: AnalyzerErrorDiagnosticId,
            title: "ConstructorNullAnalyzer throws unhandled exception",
            messageFormat: "Analyzer failed with exception: {0}",
            category: Category,
            defaultSeverity: DiagnosticSeverity.Info,
            isEnabledByDefault: true,
            description: "Inner exception inside analyzer. Please, contact author with details");

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule, AnalyzerErrorDescriptor);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeConstructorAndCatchUnhandledExceptions, SyntaxKind.ConstructorDeclaration);
        }

        private static void AnalyzeConstructorAndCatchUnhandledExceptions(SyntaxNodeAnalysisContext context)
        {
            try
            {
                AnalyzeConstructor(context);
            }
            catch (Exception e)
            {
                var diagnostic = Diagnostic.Create(
                    descriptor: AnalyzerErrorDescriptor,
                    location: context.Node.GetLocation(),
                    messageArgs: e.ToString());

                context.ReportDiagnostic(diagnostic);
            }
        }

        private static void AnalyzeConstructor(SyntaxNodeAnalysisContext context)
        {
            var referenceParameters = GetReferenceParameters(context, context.SemanticModel).ToList();

            var checkedValues = GetCheckedIdentifiers(context).ToList();

            var notCheckedParameters = referenceParameters
                .Where(parameter => checkedValues
                    .All(checkedIdentifier => checkedIdentifier != parameter))
                .ToList();

            if (!notCheckedParameters.Any())
            {
                return;
            }

            var constructorNode = context.Node as ConstructorDeclarationSyntax;
            var affectedParameters = constructorNode.ParameterList.Parameters
                .Where(x => notCheckedParameters.Any(par => par == x.Identifier.Text))
                .Select(x => x.Identifier);
            var paramsList = string.Join(", ", notCheckedParameters);
            var diagnostic = Diagnostic.Create(descriptor: Rule,
                location: constructorNode.Identifier.GetLocation(),
                additionalLocations: affectedParameters.Select(x => x.GetLocation()),
                messageArgs: paramsList);

            context.ReportDiagnostic(diagnostic);
        }

        private static IEnumerable<string> GetCheckedIdentifiers(SyntaxNodeAnalysisContext context)
        {
            var body = ((ConstructorDeclarationSyntax) context.Node).Body;
            return body.Statements.SelectMany(GetCheckedValuesInStatement).Where(x => !string.IsNullOrEmpty(x));
        }

        private static IEnumerable<string> GetCheckedValuesInStatement(StatementSyntax statement)
        {
            if (statement is IfStatementSyntax ifStatement)
            {
                var conditionExpression = ifStatement.Condition as BinaryExpressionSyntax;
                var checkedVariables = CheckBinaryExpression(conditionExpression);
                return checkedVariables;
            }

            if (statement is LocalDeclarationStatementSyntax declarationStatement)
            {
                return declarationStatement.Declaration.Variables
                    .Where(x => x.Initializer != null)
                    .Select(x => x.Initializer.Value)
                    .OfType<BinaryExpressionSyntax>()
                    .Select(ProcessBinaryIfCoalesce);
            }

            if (statement is ExpressionStatementSyntax expressionStatement)
            {
                if (expressionStatement.Expression is AssignmentExpressionSyntax assignmentExpression)
                {
                    if (assignmentExpression.Right is BinaryExpressionSyntax binaryExpression)
                    {
                        return new[] {ProcessBinaryIfCoalesce(binaryExpression)};
                    }
                }
            }

            return new List<string>();
        }

        private static string ProcessBinaryIfCoalesce(BinaryExpressionSyntax expression)
        {
            if (expression.IsKind(SyntaxKind.CoalesceExpression) 
                && expression.Left is IdentifierNameSyntax identifier)
            {
                return identifier.Identifier.ValueText;
            }

            return null;
        }

        private static List<string> CheckBinaryExpression(BinaryExpressionSyntax binaryExpression)
        {
            if (binaryExpression.Left is IdentifierNameSyntax leftIdentifier 
                && binaryExpression.Right.IsKind(SyntaxKind.NullLiteralExpression)
                && binaryExpression.OperatorToken.IsKind(SyntaxKind.EqualsEqualsToken))
            {
                return new List<string> {leftIdentifier.Identifier.Text};
            }

            if (binaryExpression.Left.IsKind(SyntaxKind.NullLiteralExpression) 
                && binaryExpression.Right is IdentifierNameSyntax rightIdentifier
                && binaryExpression.OperatorToken.IsKind(SyntaxKind.EqualsEqualsToken))
            {
                return new List<string> { rightIdentifier.Identifier.Text };
            }

            if (binaryExpression.Left is BinaryExpressionSyntax leftBinaryExpression
                && binaryExpression.Right is BinaryExpressionSyntax rightBinaryExpression)
            {
                var leftResult = CheckBinaryExpression(leftBinaryExpression);
                var rightResult = CheckBinaryExpression(rightBinaryExpression);
                return leftResult.Union(rightResult).ToList();
            }

            return new List<string>();
        }

        private static IEnumerable<string> GetReferenceParameters(SyntaxNodeAnalysisContext context, SemanticModel semanticModel)
        {
            var syntax = ((ConstructorDeclarationSyntax) context.Node).ParameterList;

            // todo: recompose
            var symbols = semanticModel.LookupSymbols(syntax.GetLocation().SourceSpan.Start);
            var namedTypeSymbols = symbols.Where(x => x.Kind == SymbolKind.NamedType).OfType<INamedTypeSymbol>().ToImmutableArray();
            var s = syntax.Parameters.Select(x => (GetTypeName(x.Type, namedTypeSymbols), x.Identifier.ValueText)).ToList();

            return s.Where(x => x.Item1.shouldCheck).Select(x => x.Item2);
        }

        private static bool ShouldCheckPredefinedTypeForNull(string typeName)
        {
            if (typeName == "object" || typeName == "string")
            {
                return true;
            }

            return false;
        }

        private static bool ShouldCheckNamedTypeForNull(string typeName, ImmutableArray<INamedTypeSymbol> symbols)
        {
            var typeDefinition = symbols.FirstOrDefault(x => x.Name == typeName);
            if (typeDefinition == null)
            {
                return false;
            }

            return typeDefinition.IsReferenceType;
        }
        private static bool ShouldCheckGenericTypeForNull(string typeName, int arity, ImmutableArray<INamedTypeSymbol> symbols)
        {
            var metadataName = $"{typeName}`{arity}";
            var typeDefinition = symbols.FirstOrDefault(x => x.MetadataName == metadataName);
            if (typeDefinition == null)
            {
                return false;
            }

            return typeDefinition.IsReferenceType;
        }

        private static (string typeName, bool shouldCheck) GetTypeName(TypeSyntax typeSyntax, ImmutableArray<INamedTypeSymbol> symbols)
        {
            if (typeSyntax is PredefinedTypeSyntax ts)
            {
                var keyword = ts.Keyword.Text;
                return (keyword, ShouldCheckPredefinedTypeForNull(keyword));
            }

            if (typeSyntax is GenericNameSyntax gs)
            {
                var typeName = gs.Identifier.Text;

                return (typeName, ShouldCheckGenericTypeForNull(typeName, gs.Arity, symbols));
            }

            if (typeSyntax is IdentifierNameSyntax ins)
            {
                var typeName = ins.Identifier.Text;
                return (typeName, ShouldCheckNamedTypeForNull(typeName, symbols));
            }

            if (typeSyntax is NullableTypeSyntax nts)
            {
                return (GetTypeName(nts.ElementType, symbols).typeName, false);
            }

            return (null, false);
        }
    }
}
