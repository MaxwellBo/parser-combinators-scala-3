
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
    parse(stream) match
      case List((res, "")) =>
        Right(res)
      case List((_, rs)) =>
        Left(s"Parser did not consume entire stream. Remaining: $rs")
      case d =>
        Left(s"Parser error. Conflict: $d")

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

/**
 * Of particular importance is that this particular monad has
 * a zero value (`failure`), namely the function which halts reading the
 * stream and returns the empty stream.
 */
def failure[A]: Parser[A] =
  Parser { cs => List.empty }

failure.parse("ABC")

/**
 * The unit operation injects a single pure value as the result,
 * without reading from the parse stream.
 */
def unit[A](a: A): Parser[A] =
  Parser { s => List((a, s)) }

unit(42).parse("ABC")

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

option(failure, item).parse("ABC")

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

trait Applicative[F[_]]:
  def pure[A](a: A): F[A]
  extension [A, B](fa: F[A => B])
    def ap(f: => F[A]): F[B]

trait Monad[F[_]]:
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]

given Functor[Parser] with
  extension [A](fa: Parser[A])
    def map[B](f: A => B): Parser[B] =
      Parser { s =>
        for {
          (a, rs) <- fa.parse(s)
        } yield (f(a), rs)
      }

given Applicative[Parser] with
  def pure[A](a: A): Parser[A] =
    unit(a)

  extension[A, B] (ff: Parser[A => B])
    def ap(fa: => Parser[A]): Parser[B] =
      Parser { s =>
        for {
          (f, s1) <- ff.parse(s) // consume some of the stream
          (a, s2) <- fa.parse(s1) // consume some more of the stream
        } yield (f(a), s2)
      }


given Monad[Parser] with
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

val parseASCIICode: Parser[Int] = item.map(_.toInt)

parseASCIICode.parse("ABC")

parseASCIICode
  .map((a: Int) => (b: Int) => a + b)
  .ap(parseASCIICode)
  .parse("ABC")

parseASCIICode.flatMap { a =>
  parseASCIICode.map { b =>
    a + b
  }
}.parse("ABC")

val go = for {
  a <- parseASCIICode
  b <- parseASCIICode
} yield a + b

go.parse("ABC")

/**
 * On top of this we can add functionality for checking whether the current
 * character in the stream matches a given predicate ( i.e is it a digit,
 * is it a letter, a specific word, etc).
 */
def satisfy(p: Char => Boolean): Parser[Char] =
  item.flatMap { c =>
    if (p(c))
      unit(c)
    else
      failure
  }

def digit: Parser[Char] =
  satisfy(_.isDigit)

digit.parse("000")
digit.parse("OOO")

/**
 * Using satisfy we can write down several combinators for detecting the
 * presence of specific common patterns of characters (numbers,
 * parenthesized expressions, whitespace, etc).
 */
def char(c: Char): Parser[Char] =
  satisfy(_ == c)

char('A').parse("ABC")
char('A').parse("FFF")

/**
 * Essentially this 50 lines code encodes the entire core of the parser
 * combinator machinery. All higher order behavior can be written on top of
 * just this logic. Now we can write down several higher level functions
 * which operate over sections of the stream.
 */
def oneOf(s: List[Char]): Parser[Char] =
  satisfy(s.contains)

oneOf("ABC".toList).parse("ABC")
oneOf("ABC".toList).parse("DBC")

def string(ccs: String): Parser[String] =
  ccs match {
    case "" => unit("")
    case cs => for {
      _ <- char(cs.head)
      _ <- string(cs.tail)
    } yield cs
  }

string("foo").parse("foobar")
string("foo").parse("bar")

/**
 * A monoid on applicative functors.
 */
trait Alternative[F[_]]:
  def empty[A]: F[A]
  extension [A](fa: F[A])
    def orElse(fa2: => F[A]): F[A]

object Alternative:
  /**
   * `many` takes a single function argument and repeatedly applies it until
   * the function fails and then yields the collected results up to that
   * point.
   */
  def many[F[_]: Functor: Alternative: Applicative, A](v: => F[A]): F[List[A]] =
    // ^ dodgy signature
    some(v).orElse(summon[Applicative[F]].pure(List.empty[A]))

  /**
   * The `some` function behaves similar except that it will fail itself if
   * there is not at least a single match.
   */
  def some[F[_]: Functor: Alternative: Applicative, A](v: => F[A]): F[List[A]] =
    def prepend: A => List[A] => List[A] = (x: A) => (xs: List[A]) =>
      xs.prepended(x)

    lazy val m: F[List[A]] = many(v)

    v.map(prepend).ap(m)

given Alternative[Parser] with
  def empty[A]: Parser[A] =
    failure[A]

  extension[A] (fa: Parser[A])
    def orElse(fa2: => Parser[A]): Parser[A] =
      option(fa, fa2)

def spaces: Parser[String] =
  Alternative.many(oneOf(" \n\r".toList)).map(_.mkString)

spaces.parse("    A")

def alpha: Parser[String] =
  Alternative.many(satisfy(_.isLetterOrDigit)).map(_.mkString)


def token[A](p: Parser[A]): Parser[A] = for {
  _ <- spaces
  a <- p
  _ <- spaces
} yield a

def reserved(s: String): Parser[String] =
  token(string(s))


def natural: Parser[Int] =
  Alternative.some(digit).map(_.mkString.toInt)

def number: Parser[Int] = for {
  s <- string("-").orElse(string(""))
  cs <- Alternative.some(digit)
} yield (s + cs.mkString).toInt

def surrounded[A](open: String)(m: Parser[A])(close: String): Parser[A] = for {
  _ <- reserved(open)
  n <- m
  _ <- reserved(close)
} yield n


/**
 * `chainl1` parses one or more occurrences of p, separated by op and
 * returns a value obtained by a recursing until failure on the left hand
 * side of the stream. This can be used to parse left-recursive grammar.
 *
 * `chainl` helps us parse things like "1 + 2 + 3" or "[1, 2, 3]"
 */
def chainl1[A](p: Parser[A])(op: Parser[A => A => A]): Parser[A] = {
  def rest(a: A): Parser[A] = (for {
    f <- op
    b <- p
    res <- rest(f(a)(b))
  } yield res).orElse(unit(a))

  for {
    a <- p
    res <- rest(a)
  } yield res
}

/**
 * The `chainl` function behaves similar except that we may provide a default value in the
 * event that `chanl1` immediately fails to parse a single occurrence of `p`.
 */
def chainl[A](p: Parser[A])(op: Parser[A => A => A])(a: A): Parser[A] =
  chainl1(p)(op).orElse(unit(a))

val op: Parser[Int => Int => Int] = char('+').map(_ => (x: Int) => (y: Int) => x + y)

chainl(number)(op)(0).parse("1+2+3")

/**
 * And that's about it! In a few hundred lines we have enough of a parser
 * library to write down a simple parser for a calculator grammar. In the
 * formal Backus–Naur Form our grammar would be written as:
 *
 * number ::= [ "-" ] digit { digit }.
 * digit  ::= "0" | "1" | ... | "8" | "9".
 * expr   ::= term { addop term }.
 * term   ::= factor { mulop factor }.
 * factor ::= "(" expr ")" | number.
 * addop  ::= "+" | "-".
 * mulop  ::= "*".
 *
 * The direct translation to Scala in terms of our newly constructed parser
 * combinator has the following form:
 */
object Calculator {
  sealed trait Expr
  case class Add(a: Expr, b: Expr) extends Expr
  case class Mul(a: Expr, b: Expr) extends Expr
  case class Sub(a: Expr, b: Expr) extends Expr
  case class Lit(n: Int) extends Expr

  def eval(ex: Expr): Int = ex match {
    case Add(a, b) => eval(a) + eval(b)
    case Mul(a, b) => eval(a) * eval(b)
    case Sub(a, b) => eval(a) - eval(b)
    case Lit(n) => n
  }

  def int: Parser[Expr] = for {
    n <- number
  } yield Lit(n)

  def expr: Parser[Expr] =
    token(chainl1(term)(addop))

  def term: Parser[Expr] =
    chainl1(factor)(mulop)

  def factor: Parser[Expr] =
    int.orElse(surrounded("(")(expr)(")"))

  def infixOp[A](x: String, f: A => A => A): Parser[A => A => A] = for {
    _ <- reserved(x)
  } yield f

  def addop: Parser[Expr => Expr => Expr] = {
    val add: Expr => Expr => Expr = (Add(_, _)).curried
    val sub: Expr => Expr => Expr = (Sub(_, _)).curried

    infixOp("+", add).orElse(infixOp("-", sub))
  }

  def mulop: Parser[Expr => Expr => Expr] =
    infixOp("*", (Mul(_, _)).curried)

  def apply(s: String): Either[String, Int] =
    expr.run(s).map(eval)
}

// Now we can try out our little parser.

Calculator("1 + 1") // 2
Calculator("(2 * (1 + 2) * (3 - (-4 + 5)))")  // 12

/**
 * <Json> ::= <Object>
 *         | <Array>
 *
 * <Object> ::= '{' '}'
 *            | '{' <Members> '}'
 *
 * <Members> ::= <Pair>
 *             | <Pair> ',' <Members>
 *
 * <Pair> ::= String ':' <Value>
 *
 * <Array> ::= '[' ']'
 *           | '[' <Elements> ']'
 *
 * <Elements> ::= <Value>
 *              | <Value> ',' <Elements>
 *
 * <Value> ::= String
 *           | Number
 *           | <Object>
 *           | <Array>
 *           | true
 *           | false
 *           | null
 */
object Json:
  ???