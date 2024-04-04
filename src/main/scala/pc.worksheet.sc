
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

object Parser:
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

  /**
   * Of particular importance is that this particular monad has
   * a zero value (`failure`), namely the function which halts reading the
   * stream and returns the empty stream.
   */
  def failure[A]: Parser[A] =
    Parser { cs => List.empty }

  /**
   * The point operation injects a single pure value as the result,
   * without reading from the parse stream.
   */
  def point[A](a: A): Parser[A] =
    Parser { s => List((a, s)) }

  /**
   * Together this forms a monoidal structure with a secondary operation
   * (`combine`) which applies two parser functions over the same stream and
   * concatenates the result.
   */
  def option[A](pa: Parser[A], qa: => Parser[A]): Parser[A] =
    Parser { s =>
      pa.parse(s) match {
        case Nil => qa.parse(s)
        case res => res
      }
    }

Parser.item.parse("ABC")
Parser.point(42).parse("ABC")
Parser.failure.parse("ABC")
Parser.option(Parser.failure, Parser.item).parse("ABC")

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

given functorParser: Functor[Parser] with
  extension[A] (fa: Parser[A])
    def map[B](f: A => B): Parser[B] =
      Parser { s =>
        for {
          (a, rs) <- fa.parse(s)
        } yield (f(a), rs)
      }

given applicativeParser(using fp: Functor[Parser]): Applicative[Parser] with
  // IGNORE ME
  extension[A] (fa: Parser[A])
    def map[B](f: A => B): Parser[B] = fp.map(fa)(f)
  // IGNORE ME

  def pure[A](a: A): Parser[A] =
    Parser.point(a)

  extension[A, B] (ff: Parser[A => B])
    def ap(fa: Parser[A]): Parser[B] =
      Parser { s =>
        for {
          (f, s1) <- ff.parse(s) // consume some of the stream
          (a, s2) <- fa.parse(s1) // consume some more of the stream
        } yield (f(a), s2)
      }


given monadParser(using fp: Applicative[Parser]): Monad[Parser] with
  // IGNORE ME
  def pure[A](a: A) = fp.pure(a)
  extension[A] (fa: Parser[A])
    def map[B](f: A => B): Parser[B] = fp.map(fa)(f)
  extension[A, B] (ff: Parser[A => B])
    def ap(fa: Parser[A]): Parser[B] = fp.ap(ff)(fa)
  // IGNORE ME

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

val parseASCIICode: Parser[Int] = Parser.item.map(_.toInt)

parseASCIICode.parse("ABC")

parseASCIICode
  .map((a: Int) => (b: Int) => a + b)
  .ap(parseASCIICode)
  .parse("ABC")

parseASCIICode.flatMap { a =>
  parseASCIICode.flatMap { b =>
    summon[Applicative[?]].pure(a + b)
  }
}.parse("ABC")


val go = for {
  a <- parseASCIICode
  b <- parseASCIICode
} yield a + b

go.parse("ABC")

/**
 * A monoid on applicative functors.
 */
trait Alternative[F[_]] extends Applicative[F]:
  def empty[A]: F[A]
  extension [A](fa: F[A])
    def orElse(fa2: => F[A]): F[A]

object Alternative:
  /**
   * `many` takes a single function argument and repeatedly applies it until
   * the function fails and then yields the collected results up to that
   * point.
   */
  def many[F[_]: Alternative, A](v: => F[A]): F[List[A]] =
    some(v).orElse(summon[Applicative[F]].pure(List.empty[A]))

  /**
   * The `some` function behaves similar except that it will fail itself if
   * there is not at least a single match.
   */
  def some[F[_]: Alternative, A](v: => F[A]): F[List[A]] =
    def prepend: A => List[A] => List[A] = (x: A) => (xs: List[A]) =>
      xs.prepended(x)

    lazy val m: F[List[A]] = many(v)

    v.map(prepend).ap(m)

given alternativeParser(using fp: Applicative[Parser]): Alternative[Parser] with
  // IGNORE ME
  def pure[A](a: A) = fp.pure(a)
  extension[A] (fa: Parser[A])
    def map[B](f: A => B): Parser[B] = fp.map(fa)(f)
  extension[A, B] (ff: Parser[A => B])
    def ap(fa: Parser[A]): Parser[B] = fp.ap(ff)(fa)
  // IGNORE ME

  def empty[A]: Parser[A] =
    Parser.failure[A]

  extension[A] (fa: Parser[A])
    def orElse(fa2: => F[A]): Parser[A] =
      Parser.option(fa, fa2)