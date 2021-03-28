package fpinscala.parallelism.blocking

import fpinscala.UnitSpec
import fpinscala.parallelism.blocking.ParBroken._

import java.util.concurrent.{ExecutorService, Executors, Future}

class ParBrokenSpec extends UnitSpec {
  def getService(noOfThreads: Int): ExecutorService = {
    Executors.newFixedThreadPool(noOfThreads)
  }

  "unit" should "return argument on get" in {
    val service = getService(1)

    // NOTE: This line hangs, no idea why
    val p: Par[String] = unit("no")
    val f: Future[String] = p(service)
    assert(f.get() == "no")
  }
}