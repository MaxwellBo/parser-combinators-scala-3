
trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

trait Applicative[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]
  extension [A, B](fa: F[A => B])
    def ap(f: F[A]): F[B]

trait Monad[F[_]] extends Applicative[F]:
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]

trait Alternative[F[_]] extends Applicative[F]:
  def empty[A]: F[A]
  extension [A](fa: F[A])
    def orElse(fa2: F[A]): F[A]

/**
 * Structurally a parser is a function which takes an input stream of
 * characters and yields a parse tree by applying the parser logic over
 * sections of the character stream (called lexemes) to build up a
 * composite data structure for the AST.
 */
case class Parser[A](parse: String => List[(A, String)]):

  /**
   * Running the function will result in traversing the stream of characters
   * yielding a value of type `A` that usually represents the AST for the
   * parsed expression, or failing with a parse error for malformed input,
   * or failing by not consuming the entire stream of input. A more
   * robust implementation would track the position information of failures
   * for error reporting.
   */
  def run(stream: String): Either[String, A] =
    parse(stream) match {
      case List((res, "")) =>
        Right(res)
      case List((_, rs)) =>
        Left(s"Parser did not consume entire stream. Remaining: $rs")
      case d =>
        Left(s"Parser error. Conflict: $d")
    }

/**
 * We advance the parser by extracting a single character from the parser
 * stream and returning in a tuple containing itself and the rest of the
 * stream. The parser logic will then scrutinize the character and either
 * transform it in some portion of the output or advance the stream and
 * proceed.
 */
def item: Parser[Char] = Parser {
  case "" => List.empty
  case xs => List((xs.head, xs.tail))
}

item.parse("ABC")

given Functor[Parser] with
  extension[A](fa: Parser[A])

    /**
     * A map operation that takes a parse operation and transforms its result.
     */
    def map[B](f: A => B): Parser[B] =
      Parser { s =>
        for {
          (a, rs) <- fa.parse(s)
        } yield (f(a), rs)
      }


item.map(_.toInt).parse("ABC")


given Applicative[Parser] with
  /**
   * The pure operation injects a single pure value as the result,
   * without reading from the parse stream.
   */
  def pure[A](a: A): Parser[A] =
    Parser { s => List((a, s)) }

  extension[A, B](ff: Parser[A => B])
    def ap(fa: Parser[A]): Parser[B] =
      Parser { s =>
        for {
          (f, s1) <- ff.parse(s) // consume some of the stream
          (a, s2) <- fa.parse(s1) // consume some more of the stream
        } yield (f(a), s2)
      }

  extension[A] (fa: Parser[A])
    def map[B](f: A => B): Parser[B] =
      Parser { s =>
        for {
          (a, rs) <- fa.parse(s)
        } yield (f(a), rs)
      }



given Monad[Parser] with
  extension[A] (fa: Parser[A])
    def map[B](f: A => B): Parser[B] =
      Parser { s =>
        for {
          (a, rs) <- fa.parse(s)
        } yield (f(a), rs)
      }

  /**
   * The pure operation injects a single pure value as the result,
   * without reading from the parse stream.
   */
  def pure[A](a: A): Parser[A] =
    Parser { s => List((a, s)) }

  extension[A, B] (ff: Parser[A => B])
    def ap(fa: Parser[A]): Parser[B] =
      Parser { s =>
        for {
          (f, s1) <- ff.parse(s) // consume some of the stream
          (a, s2) <- fa.parse(s1) // consume some more of the stream
        } yield (f(a), s2)
      }

  extension[A] (fa: Parser[A])

    /**
     * The flatMap operation for our parser will take one parse operation and compose a parser
     * function over the result of the first.
     *
     * This allows us to sequence parsers, where a second parser can
     * depend on the result of the first parser.
     */
    def flatMap[B](f: A => Parser[B]): Parser[B] =
      Parser { s =>
        fa.parse(s).flatMap { case (a: A, s1: String) =>
          val fb: List[(B, String)] = f(a).parse(s1)
          fb
        }
      }

summon[Functor[Parser]]