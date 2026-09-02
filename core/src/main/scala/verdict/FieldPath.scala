package verdict

import scala.util.matching.Regex

/** A dot-separated path into a record, such as `counterparty.jurisdiction`.
  *
  * Opaque so that an arbitrary `String` cannot arrive where a path is expected
  * without going through [[FieldPath.parse]] first.
  */
opaque type FieldPath = String

object FieldPath:

  private val Segment: Regex = "[A-Za-z_][A-Za-z0-9_]*".r

  def parse(raw: String): Either[String, FieldPath] =
    if raw.isEmpty then Left("a field path may not be empty")
    else
      val segments = raw.split("\\.", -1).toList
      segments.find(s => !Segment.matches(s)) match
        case Some("") =>
          Left(s"'$raw' has an empty segment; segments are separated by a single dot")
        case Some(bad) =>
          Left(s"'$bad' in '$raw' is not a legal field name")
        case None => Right(raw)

  def apply(raw: String): Either[String, FieldPath] = parse(raw)

  def of(segments: List[String]): Either[String, FieldPath] = parse(segments.mkString("."))

  /** Skips validation and throws on a malformed path.
    *
    * This exists for generated code: the phase 7 macro derives paths from real
    * field selections and has already checked them against the schema at compile
    * time, so a runtime re-parse could only re-derive an answer the compiler
    * already has. Hand-written code should use [[parse]].
    */
  def unsafe(raw: String): FieldPath =
    parse(raw).fold(msg => throw new IllegalArgumentException(s"verdict: $msg"), identity)

  extension (path: FieldPath)
    def render: String            = path
    def segments: List[String]    = path.split("\\.", -1).toList
    def isNested: Boolean         = path.contains('.')
    def /(segment: String): Either[String, FieldPath] = parse(s"$path.$segment")

  given Ordering[FieldPath] = Ordering.String
