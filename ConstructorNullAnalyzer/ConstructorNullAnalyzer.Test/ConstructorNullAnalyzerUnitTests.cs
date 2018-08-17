using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TestHelper;

namespace ConstructorNullAnalyzer.Test
{
    [TestClass]
    public class ConstructorNullAnalyzerTest : CodeFixVerifier
    {

        //No diagnostics expected to show up
        [TestMethod]
        public void EmptyIsValid()
        {
            var test = @"";

            VerifyCSharpDiagnostic(test);
        }

        //Diagnostic and CodeFix both triggered and checked for
        [TestMethod]
        public void FixForMixedType()
        {
            var test = @"
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Threading.Tasks;
    using System.Diagnostics;

    namespace ConsoleApplication1
    {
        class TypeName
        {
            public TypeName(string param1, int param2, String param3, int? param4)
            {
            }
        }
    }";
            var expected1 = new DiagnosticResult
            {
                Id = "CA001",
                Message = string.Format("Constructor should check that parameter(s) {0} are not null", "param1, param3"),
                Severity = DiagnosticSeverity.Warning,
                Locations =
                    new[]
                    {
                        new DiagnosticResultLocation("Test0.cs", 13, 20),
                        new DiagnosticResultLocation("Test0.cs", 13, 36),
                        new DiagnosticResultLocation("Test0.cs", 13, 63)
                    }
            };

            VerifyCSharpDiagnostic(test, expected1);

            var fixtest = @"
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Threading.Tasks;
    using System.Diagnostics;

    namespace ConsoleApplication1
    {
        class TypeName
        {
            public TypeName(string param1, int param2, String param3, int? param4)
            {
            if (param1 == null)
                throw new ArgumentNullException(""param1"");
            if (param3 == null)
                throw new ArgumentNullException(""param3"");
        }
        }
    }";
            VerifyCSharpFix(test, fixtest);
        }

        protected override CodeFixProvider GetCSharpCodeFixProvider()
        {
            return new ConstructorNullAnalyzerCodeFixProvider();
        }

        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer()
        {
            return new ConstructorNullAnalyzer();
        }
    }
}
