import scala.util.parsing.json._
import scala.sys.process._

object GithubApi {
  case class Commit(val sha: String, val message: String) {
    override def toString = s"commit $sha= $message"
  }

  val url = "https://api.github.com"

  private def parse(command: String): Option[Any] = {
    val request = s"$url/$command"
    println(s"Request: $request")

    val source = io.Source.fromURL(request).mkString
    JSON.parseFull(source)
  }

  private def get[T](option: Map[String, Any], key: String): T = option.get(key).get.asInstanceOf[T]

  def getCommits(owner: String, repo: String): List[Commit] = {
    val json = parse(s"repos/$owner/$repo/commits")

    json.get.asInstanceOf[List[Map[String, Any]]].map { item =>
      val sha = get[String](item, "sha")
      val commit = get[Map[String, Any]](item, "commit")
      val message = get[String](commit, "message")

      Commit(sha, message)
    }
  }
}

object Say extends App {
  GithubApi getCommits("netoneko", "scala-misc") foreach (commit => s"say $commit".!!)
}
