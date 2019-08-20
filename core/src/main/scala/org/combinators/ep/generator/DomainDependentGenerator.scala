package org.combinators.ep.generator

import org.combinators.ep.domain.abstractions._

/**
 * Contains the domain-dependent methods required for generation.
 *
 * @param independent
 */
abstract class DomainDependentGenerator(val independent:DomainIndependentGenerator) {
  import independent._
//  /**
//   * Base concept for the representation of program unit on disk.
//   * @group types
//   */
//  abstract type CompilationUnit
//
//  /**
//   * Base concept for a single expression in the language.
//   * @group types
//   */
//  abstract type Expression
//
//  /**
//   * Base concept for a meaningful type in the language.
//   * @group types
//   */
//  abstract type Type
//
//  /**
//   * Base concept for a meaningful line-of-code in the language.
//   * @group types
//   */
//  abstract type Statement


  /**
   * For all possible EP solutions, this method generates the sequence of statements that result
   * for a given operation and data-type.
   *
   * Must be return a sequence of statements since some operations require a more substantial
   * implementation depending upon the programming language.
   *
   * Must be Statements (rather than just an Expression) because in most operations, a value of
   * some sort is returned, thus instead of just "expr" it becomes "return expr;" To activate the
   * "return expr;" statement, use the [[toOperationResult]] method.
   *
   * @param tpeCase    data-type for the context
   * @param op     operation for the context
   * @group api
   */
  @throws[scala.NotImplementedError]("If no (data-type, operation) combination defined.")
  def logic(tpeCase:DataTypeCase, op:Operation) : Seq[Statement] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for type case "${tpeCase.name}" """)
  }

  /**
   * For the processed model, return generated code artifacts for solution.
   * @group api
   */
  def generatedCode: Seq[CompilationUnit]
}
