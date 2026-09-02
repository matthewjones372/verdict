package verdict

import scala.annotation.StaticAnnotation

/** Marks a field whose value should not be reproduced in an explanation or a
  * log. The schema carries the mark; what a caller does with it is its own
  * business.
  */
final class Sensitive extends StaticAnnotation

/** Marks a field that identifies the subject of the record. */
final class Identity extends StaticAnnotation
