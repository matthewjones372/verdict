package verdict

/** Why a rule reached the answer it reached.
  *
  * Evaluation produces one of these rather than a `Boolean`, and the boolean
  * falls out of it. There is no second traversal that renders an explanation
  * alongside a fast path, so there is nothing for the two to disagree about.
  */
enum Evidence:
  case Leaf(description: String, override val held: Boolean)
  case All(children: List[Evidence])
  case Any(children: List[Evidence])
  case Negation(child: Evidence)

  def held: Boolean = this match
    case Leaf(_, h)      => h
    case All(children)   => children.forall(_.held)
    case Any(children)   => children.exists(_.held)
    case Negation(child) => !child.held

  def render(indent: String = ""): String =
    val mark = if held then "PASS" else "FAIL"
    this match
      case Leaf(description, _) =>
        s"$indent$mark $description"
      case All(children) =>
        (s"$indent$mark all of:" :: children.map(_.render(indent + "  "))).mkString("\n")
      case Any(children) =>
        (s"$indent$mark any of:" :: children.map(_.render(indent + "  "))).mkString("\n")
      case Negation(child) =>
        s"$indent$mark not:\n${child.render(indent + "  ")}"

  /** Only the leaves that decided the outcome, for a one-line summary. */
  def leaves: List[Evidence.Leaf] = this match
    case leaf: Leaf      => List(leaf)
    case All(children)   => children.flatMap(_.leaves)
    case Any(children)   => children.flatMap(_.leaves)
    case Negation(child) => child.leaves
