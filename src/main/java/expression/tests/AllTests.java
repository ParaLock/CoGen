package expression.tests;

import expression.DomainModel;
import expression.data.Add;
import expression.data.Eval;
import expression.data.Lit;
import expression.extensions.*;
import expression.instances.Expression;
import expression.instances.Instance;
import expression.instances.UnitSuite;
import expression.operations.SimplifyExpr;

import java.util.ArrayList;

/**
 * Develop a simple test around the following SimpleExpression:
 *
 *   ((5*7)+(8/9))
 *
 * These tests are developed within the APPLICATION DOMAIN but their code is used
 * within the generated Drivers.
 */
public class AllTests extends UnitSuite {

    /** This is invoked from the superclass, not directly here. */
    protected static Expression test = new Expression(
            new expression.instances.BinaryExp(new Add(),
                    new expression.instances.BinaryExp(new Mult(),
                            new expression.instances.Lit(new Lit(),5),
                            new expression.instances.Lit(new Lit(),7)),
                    new expression.instances.BinaryExp(new Divd(),
                       new expression.instances.Lit(new Lit(),18),
                      new expression.instances.Lit(new Lit(),9)
               )
           )
        );

    public AllTests() {}

    public static Expression testPair() {

        test.add(new Eval(), 37.0);
        test.add(new PrettyP(), "((5.0*7.0)+(18.0/9.0))");

        ArrayList<Instance> collected = new ArrayList<>();
        collected.add(new expression.instances.Lit(new Lit(),5.0));
        collected.add(new expression.instances.Lit(new Lit(),7.0));
        collected.add(new expression.instances.Lit(new Lit(),18.0));
        collected.add(new expression.instances.Lit(new Lit(),9.0));

        // will be hard to check for equality since literals will
        // appear in any order (and might be duplicated).
        test.add(new Collect(), collected);
        return test;
    }

    public static Expression testJustEval() {

        test.add(new Eval(), 37.0);
        return test;
    }

    public static Expression testJustPrettyP() {

        test.add(new PrettyP(), "((5.0*7.0)+(18.0/9.0))");
        return test;
    }

    public static Expression testSimple(DomainModel model) {
        Expression exp = new Expression(
                 new expression.instances.BinaryExp(new Add(),
                        new expression.instances.Lit(new Lit(),1.0),
                        new expression.instances.Lit(new Lit(),2.0)));

        exp.add(new Eval(), 3.0);
        exp.add(new PrettyP(), "(1.0+2.0)");

        ArrayList<Instance> collected = new ArrayList<>();
        collected.add(new expression.instances.Lit(new Lit(),1.0));
        collected.add(new expression.instances.Lit(new Lit(),2.0));

        // will be hard to check for equality since literals will
        // appear in any order (and might be duplicated).
        exp.add(new Collect(), collected);
        return exp;
    }

    public static Expression testSimplifyDivide(DomainModel model) {
        // (5*7) / 1 --> just (5*7)
        final Instance mult = new expression.instances.BinaryExp(new Mult(),
                new expression.instances.Lit(new Lit(),5.0),
                new expression.instances.Lit(new Lit(),7.0));

        Expression exp = new Expression(new expression.instances.BinaryExp(new Divd(),
                        mult,
                        new expression.instances.Lit(new Lit(),1.0)));

        exp.add(new SimplifyExpr(), mult);

        return exp;
    }

// causes problems?
//    public static Expression testSimplifyMultiply(DomainModel model) {
//        // (5/7) / (7-(2*3) --> just (5/7)
//        Expression exp = new Expression(model) {
//
//            @Override
//            protected Instance expression() {
//                return new expression.instances.BinaryExp(new Divd(),
//                        new expression.instances.BinaryExp(new Mult(),
//                                new expression.instances.Lit(new Lit(), 5.0),
//                                new expression.instances.Lit(new Lit(), 7.0)),
//                        new expression.instances.BinaryExp(new Sub(),
//                                new expression.instances.Lit(new Lit(), 7.0),
//                                new expression.instances.BinaryExp(new Mult(),
//                                        new expression.instances.Lit(new Lit(), 2.0),
//                                        new expression.instances.Lit(new Lit(), 3.0))));
//            }
//        };
//
//        Instance divd = new expression.instances.BinaryExp(new Mult(),
//                new expression.instances.Lit(new Lit(),5.0),
//                new expression.instances.Lit(new Lit(),7.0));
//
//        // this validates the simplify capability works.
//        exp.add(new SimplifyExpr(), divd);
//
//        return exp;
//    }
}
