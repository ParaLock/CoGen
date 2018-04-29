package expression.instances;

import expression.Exp;

import java.util.stream.Stream;

/**
 * Represents an actual Unary Expression instance.
 */
public class UnaryExp implements Instance {
    /** The actual operator. */
    public final expression.data.BinaryExp op;
    public final Instance exp;

    public UnaryExp(expression.data.BinaryExp op, Instance exp) {
        this.op = op;
        this.exp = exp;
    }

    @Override
    public Stream<Instance> subInstances() {
        return Stream.concat(Stream.of(this), exp.subInstances());
    }

    @Override
    public Exp self() {
        return op;
    }
}
