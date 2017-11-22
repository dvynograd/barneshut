import barneshut.Body
import common._
import barneshut.conctrees.{ConcBuffer, _}

import scala.math.{abs, _}

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad

    def massProdX(): Float = mass * massX

    def massProdY(): Float = mass * massY

    def xMax(): Float = centerX + size / 2

    def xMin(): Float = centerX - size / 2

    def yMax(): Float = centerY + size / 2

    def yMin(): Float = centerY - size / 2
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX

    def massY: Float = centerY

    def mass: Float = 0

    def total: Int = 0

    def insert(b: Body): Quad = new Leaf(centerX, centerY, size, Seq(b))
  }

  case class Fork(nw: Quad, ne: Quad, sw: Quad, se: Quad) extends Quad {
    val centerX: Float = nw.centerX + (nw.size / 2)
    val centerY: Float = nw.centerY + (nw.size / 2)
    val size: Float = nw.size + ne.size
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float = if (mass == 0) centerX else (nw.massProdX + ne.massProdX + sw.massProdX + se.massProdX) / mass
    val massY: Float = if (mass == 0) centerY else (nw.massProdY + ne.massProdY + sw.massProdY + se.massProdY) / mass
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      new Fork(
        if (b.isInQuad(nw, false, false)) nw.insert(b) else nw,
        if (b.isInQuad(ne, true, false)) ne.insert(b) else ne,
        if (b.isInQuad(sw, false, true)) sw.insert(b) else sw,
        if (b.isInQuad(se, true, true)) sw.insert(b) else se
      )
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
    extends Quad {
    val mass: Float = bodies.aggregate(0f)(_ + _.mass, _ + _)
    val massX: Float = bodies.aggregate(0f)(_ + _.massProdX, _ + _) / mass
    val massY: Float = bodies.aggregate(0f)(_ + _.massProdY, _ + _) / mass

    val total: Int = bodies.size

    def insert(b: Body): Quad = {
      if (size < minimumSize) {
        new Leaf(centerX, centerY, size, bodies :+ b)
      } else {
        val fork = new Fork(
          new Empty(centerX - size / 4, centerY - size / 4, size / 2),
          new Empty(centerX + size / 4, centerY - size / 4, size / 2),
          new Empty(centerX - size / 4, centerY + size / 4, size / 2),
          new Empty(centerX + size / 4, centerY + size / 4, size / 2)
        )
        bodies.foldLeft(fork) { (result: Fork, body: Body) => result.insert(body) }.insert(b)
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) => // no force, do  nothing
        case Leaf(_, _, _, bodies) =>
          //addForce(quad.mass, quad.massX, quad.massY) ?????
          bodies.foreach(b => addForce(b.mass, b.x, b.y))
        // add force contribution of each body by calling addForce
        case Fork(nw, ne, sw, se) =>
          val dist = distance(quad.massX, quad.massY, x, y)
          if (quad.size / dist < theta) {
            addForce(quad.mass, quad.massX, quad.massY)
          } else {
            List(nw, ne, sw, se).foreach({
              traverse(_)
            })
          }
        // see if node is far enough from the body,
        // or recursion is needed
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

    def massProdX(): Float = mass * x

    def massProdY(): Float = mass * y

    def isInQuad(q: Quad, includeMaxX: Boolean = false, includeMaxY: Boolean = false): Boolean = {
      val maxXLimit = if (includeMaxY) x <= q.xMax() else x < q.xMax()
      val maxYLimit = if (includeMaxY) y <= q.yMax() else y < q.yMax()
      val xInQuad = x >= q.xMin() && maxXLimit
      val yInQuad = y >= q.yMin() && maxYLimit
      xInQuad && yInQuad
    }
  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    /**
      * This method should use the body position, boundaries and sectorPrecision
      * to determine the sector into which the body should go into, and add the body
      * into the corresponding ConcBuffer object.
      * Importantly, if the Body lies outside of the Boundaries,
      * it should be considered to be located at the closest point within
      * the Boundaries for the purpose of finding which ConcBuffer should hold the body.
      *
      * @param b
      * @return
      */
    def +=(b: Body): SectorMatrix = {
      def validateBoundaries(x: Float, min: Float, max: Float): Float = {
        if (x < min) min
        else if (x > max) max
        else x
      }

      def getX = {
        val x: Float = validateBoundaries(b.x, boundaries.minX, boundaries.maxX)
        val positionInSector = abs(abs(boundaries.minX) - abs(b.x))
        val sector = floor(positionInSector / sectorSize).toInt
        if (sector > sectorPrecision - 1) sectorPrecision - 1 else sector
      }

      def getY = {
        val y = validateBoundaries(b.y, boundaries.minY, boundaries.minX)
        val positionInSector = abs(abs(boundaries.minY) - abs(b.y))
        val sector = floor(positionInSector / sectorSize).toInt
        if (sector > sectorPrecision - 1) sectorPrecision - 1 else sector
      }

      apply(getX, getY) += (b)
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      for (x <- 0 until sectorPrecision; y <- 0 until sectorPrecision) {
        matrix(y * sectorPrecision + x) = apply(x, y).combine(that(x, y))
      }
      this
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4

      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this (x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: => T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString ("\n")
    }
  }

}
